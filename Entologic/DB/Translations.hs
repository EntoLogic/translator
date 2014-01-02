
{-# LANGUAGE OverloadedStrings
           , ScopedTypeVariables
           , TemplateHaskell #-}

module Entologic.DB.Translations where

import Entologic.Translate
import Entologic.Error
import Entologic.Base
import Entologic.Ast
import Entologic.Ast.Json
import Entologic.Phrase
import Entologic.Phrase.Json
import Entologic.Output
import Entologic.Output.Json

import Database.MongoDB as DB

import System.Environment
import System.IO
import System.Process

import Control.Applicative
import Control.Monad.Error
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Lens
import Control.Lens.TH

import Data.Aeson
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text (Text(..))
import Data.Time.Clock
import Data.Word (Word16)
import Data.Endian

import Network.Socket.Internal (PortNumber(..))

import Text.Read (readMaybe)

type DBInfo = DB.Pipe

data Config = Config { _login :: Login
                     , _astGens :: M.Map Text Text
                     , _phrases :: Phrases
                     }

data Login = Login { _host :: String
                   , _port :: DB.PortID
                   , _username :: DB.Username
                   , _password :: DB.Password
                   , _db :: Text
                   }

$(makeLenses ''Config)
$(makeLenses ''Login)

toPortId :: Word16 -> DB.PortID
toPortId word = DB.PortNumber . PortNum $ swapEndian word

instance FromJSON Login where
    parseJSON (Object obj) = Login <$> obj .: "host"
                                   <*> (toPortId <$> obj .: "port")
                                   <*> obj .: "username"
                                   <*> obj .: "password"
                                   <*> obj .: "db"

readJson :: FromJSON a => String -> ErrorT String IO a
readJson filename = eFromRight =<< (liftIO $ eitherDecode <$>
                                                L8.readFile filename)

loadConfigs :: ErrorT String IO Config
loadConfigs = do
    login <- readJson "login.json"
    astGens <- readJson "astgens.json"
    phrases <- readJson "phrase.json"
    return $ Config login astGens phrases

dbConnect :: Config -> ErrorT String IO DBInfo
dbConnect (Config (Login hostname port user pass db) _ _)= do
    let host = Host hostname port
    liftIO $ putStrLn $ "connecting to " ++ hostname ++ ":" ++ show port
    liftIO $ putStrLn $ "connecting to " ++ showHostPort host
    pipe <- changeError show $ connect host
    authResult <- access pipe master db $ auth user pass
    case authResult of
      Left err -> throwError $ show err
      Right False -> throwError "authentication failure!"
      _ -> liftIO $ putStrLn "authenticated!"
    return pipe

dbInteract :: Config -> DBInfo -> ErrorT String IO ()
dbInteract config pipe = do
    accessResult <- access pipe master (config ^. login.db) (dbAccess $ config)
    case accessResult of
      Left err -> throwError $ show err
      Right _ -> liftIO $ putStrLn "Finished databasing"
    

dbAccess :: Config -> DB.Action (ErrorT String IO) ()
dbAccess config = do
    liftIO $ putStrLn "about to query"
    toTranslate <- nextN 10 =<< DB.find (select ["lastTranslated" := DB.Null]
                                            "explanations")
                                           {sort = ["updatedAt" =: (1 :: Int)]}
    liftIO $ putStrLn $ "got things to translate, length " ++ show (length toTranslate)
    translations <- liftIO $ mapM (fmap (_1 %~ (fmap $ T.pack . L8.unpack)) . runTranslation) toTranslate
    time <- liftIO getCurrentTime
    let results = map (update time) $ zip toTranslate translations
    mapM_ (DB.save "explanations") results

  where
    update time (doc, (translation, error)) = merge
        ["lastTranslated" =: time, "updatedAt" =: time
        , "outputTree" =: translation
        , "translatorMessages" =: ([["msg" =: error, "msgType" =: T.pack "error"]] :: [Document]) ]
      doc

    runTranslation :: Document -> IO (Maybe L.ByteString, Maybe String)
    runTranslation doc = do
        translation <- runErrorT $ runTranslation' doc
        case translation of
          Left err -> return (Nothing, Just err)
          Right trans -> return (Just trans, Nothing)

    runTranslation' :: Document -> ErrorT String IO L.ByteString
    runTranslation' doc = do
        pLang <- DB.lookup "pLang" doc
        sLang <- DB.lookup "nLang" doc
        liftIO $ putStrLn $ "running translation: pLang = " ++ T.unpack pLang ++ ", sLang = " ++ T.unpack sLang
        ast <- parseCode (config ^. astGens) pLang =<< L8.pack <$> DB.lookup "plainCodeInput" doc
        liftIO $ putStrLn "parsed code into AST"
        output <- liftIO $ runTL (TLInfo (config ^. phrases) pLang sLang) (TLState False "")
                      (translate  (uProg ast, Area Nothing Nothing))
        [(OCNode node)] <- eFromRight output
        liftIO $ putStrLn "runTranslation completed"
        return $ encode node

{-
modify :: Text -> Value -> Document -> Document
modify doc nk nv = finish $ foldl update' doc ([], False)
  where
    update' (acc, found) field@(k := _)
      | k == nk = ((nk := nv) : acc, True)
      | otherwise = (field : acc, found)

    finish (list, True) = list
    finish (list, False) = (nk := nv) : list


add :: Text -> Value -> Document -> Document
add doc k v = (k := v) : doc
-}

parseCode :: M.Map Text Text -> Text -> L.ByteString -> ErrorT String IO UAst
parseCode astGens pLang code = do
    liftIO $ putStr "About to parse code: " >> L8.putStrLn code
    gen <- eFromJust $ M.lookup pLang astGens
    let cp = CreateProcess
               { cmdspec = RawCommand (T.unpack gen) [], cwd = Nothing
               , env = Nothing, std_in = CreatePipe, std_out = CreatePipe
               , std_err = Inherit, close_fds = True, create_group = False
               , delegate_ctlc = False }
    (mstdin, mstdout, _, _) <- liftIO $ createProcess cp
    stdin <- errFromJust "Missing stdin handle for AST generator" mstdin
    stdout <- errFromJust "Missing stdout handle for AST generator" mstdout
    liftIO $ do
        L.hPut stdin code
        hClose stdin
    eFromRight =<< (liftIO $ eitherDecode <$> L.hGetContents stdout)
--    (stdin, stdout, stderr
