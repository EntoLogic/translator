
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
import Entologic.DB

import Database.MongoDB as DB

import System.Environment
import System.IO
import System.Process

import Control.Applicative
import Control.Monad.Error
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Lens

import Data.Aeson
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString as SBS
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Data.Text (Text(..))
import Data.Time.Clock
import Data.Semigroup ((<>))


import Text.Read (readMaybe)

loadConfigs :: ErrorT String IO Config
loadConfigs = do
    login <- readJson "login.json"
    let login' = login & (username %~ remEmptyTxt) . (password %~ remEmptyTxt)
    astGens <- readJson "astgens.json"
    phrases <- readJson "phrase.json"
    return $ Config login astGens phrases

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
    update time (doc, (translation, error)) = merge changes doc
      where
        message Nothing = DB.Null
        message (Just error) = DB.Array [Doc ["msg" =: error, "msgType" =: T.pack "error"]]
        changes = ["lastTranslated" =: time, "updatedAt" =: time
                   , "outputTree" =: translation, "translatorMessages" := message error ]

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

parseCode :: M.Map Text Text -> Text -> L.ByteString -> ErrorT String IO UAst
parseCode astGens pLang code = do
    liftIO $ putStr "About to parse code: " >> L8.putStrLn code
    gen <- eFromJust $ M.lookup pLang astGens
    liftIO $ T.IO.putStrLn $ " parsing code with " <> gen
    let cp = CreateProcess
               { cmdspec = RawCommand (T.unpack gen) [], cwd = Nothing
               , env = Nothing, std_in = CreatePipe, std_out = CreatePipe
               , std_err = Inherit, close_fds = True, create_group = False
               , delegate_ctlc = False }
    (mstdin, mstdout, _, pHandle) <- liftIO $ createProcess cp
    liftIO $ putStrLn "Launched AST generator"
    stdin <- errFromJust "Missing stdin handle for AST generator" mstdin
    stdout <- errFromJust "Missing stdout handle for AST generator" mstdout
    liftIO $ do
        L.hPut stdin code
        hClose stdin
    contents <- liftIO $ SBS.hGetContents stdout
    liftIO $ do
        putStr "AST generator result: "
        SBS.putStrLn contents
    ast <- efFromRight ("Error parsing AST generator output: " ++) .
                eitherDecode . L.fromStrict $ contents
    liftIO $ hClose stdout
    liftIO $ waitForProcess pHandle
    liftIO $ putStrLn "Got AST from generator"
    return ast
