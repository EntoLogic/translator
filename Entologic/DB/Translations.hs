
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
import Data.Text as T
import Data.Time.Clock


dbInteract :: ErrorT String IO ()
dbInteract = do
    args <- liftIO getArgs
    case args of
      [] -> throwError "give DB host as first argument"
      (dbHost:rest) -> do
        pipe <- liftIO (runIOE . connect $ host dbHost)
        phrases <- liftIO $ readPhrases "phrase.json"
        access pipe master "entologic_dev" (dbAccess phrases)
        liftIO $ close pipe

dbAccess :: Phrases -> Action (ErrorT String IO) ()
dbAccess phrases = do
    toTranslate <- nextN 10 =<< DB.find (select ["output_tree" =: [DB.Null]] "")
                                           {sort = ["updatedAt" =: (1 :: Int)]}
    mapM (lift . runTranslation) toTranslate
    time <- liftIO getCurrentTime
    return ()

  where
    runTranslation :: Document -> ErrorT String IO L.ByteString
    runTranslation doc = do
        pLang <- DB.lookup "pLang" doc
        sLang <- DB.lookup "nLang" doc
        ast <- parseCode pLang =<< L8.pack <$> DB.lookup "plainCodeInput" doc
        output <- liftIO $ runTL (TLInfo phrases pLang sLang) (TLState False "")
                      (translate  (uProg ast, Area Nothing Nothing))
        (OCNode node) <- eFromRight output
        return $ encode node

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
