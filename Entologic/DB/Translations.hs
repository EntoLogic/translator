
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

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
import Control.Monad.Trans.Error
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

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


dbInteract :: ErrorT String IO ()
dbInteract = do
    args <- liftIO getArgs
    case args of
      (dbHost:port:_) ->
        case readMaybe port :: Maybe Word16 of
          Nothing -> throwError "Invalid port number"
          Just port' -> do
            let host = Host dbHost (PortNumber $ PortNum $ swapEndian port')
            liftIO $ putStrLn $ "connecting to " ++ dbHost ++ ":" ++ show port'
            liftIO $ putStrLn $ "connecting to " ++ showHostPort host
            pipe <- liftIO (runIOE $ connect host)
            phrases <- liftIO $ readPhrases "phrase.json"
            access pipe master "entologic_dev" (dbAccess phrases)
            liftIO $ close pipe
            liftIO $ putStrLn "Finished databasing"
      _ -> throwError "Usage: ./Main host port"

dbAccess :: Phrases -> Action (ErrorT String IO) ()
dbAccess phrases = do
    auth "jobs" "ACHDsUWjJXNtlXIwlgf4EebC"
    liftIO $ putStrLn "authenticated!"
    toTranslate <- nextN 10 =<< DB.find (select ["lastTranslated" := DB.Null]
                                            "explanations")
                                           {sort = ["updatedAt" =: (1 :: Int)]}
    liftIO $ putStrLn $ "got things to translate, length " ++ show (length toTranslate)
    translations <- mapM (fmap (T.pack . L8.unpack) . lift . runTranslation) toTranslate
    time <- liftIO getCurrentTime
    let results = map (update time) $ zip toTranslate translations
    mapM_ (DB.save "explanations") results

  where
    update time (doc, translation) = merge ["lastTranslated" =: time, "updatedAt" =: time, "outputTree" =: translation] doc

    runTranslation :: Document -> ErrorT String IO L.ByteString
    runTranslation doc = do
        pLang <- DB.lookup "pLang" doc
        sLang <- DB.lookup "nLang" doc
        liftIO $ putStrLn $ "running translation: pLang = " ++ T.unpack pLang ++ ", sLang = " ++ T.unpack sLang
        ast <- parseCode pLang =<< L8.pack <$> DB.lookup "plainCodeInput" doc
        liftIO $ putStrLn "parsed code into AST"
        output <- liftIO $ runTL (TLInfo phrases pLang sLang) (TLState False "")
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

parseCode :: Text -> L.ByteString -> ErrorT String IO UAst
parseCode pLang code = do
    astGens <- eFromRight =<< (liftIO $ eitherDecode <$> L8.readFile "astgens.json")
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
