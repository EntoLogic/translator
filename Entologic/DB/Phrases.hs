
{-# LANGUAGE OverloadedStrings #-}

module Entologic.DB.Phrases where

import Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Map(Map(..))
import Data.Text(Text(..))
import Data.Int
import qualified Data.Traversable as TV
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.Identity
import Control.Lens ((^.))
import Control.Concurrent

import System.Environment
import System.Posix.IO
import System.IO

import Database.MongoDB hiding (lookup)
import qualified Database.MongoDB as DB

import Text.Read (readMaybe)

import Entologic.DB
import Entologic.Error
import Entologic.Phrase
import Entologic.Phrase.Json

dlPhrases :: Login -> Pipe -> ErrorT String IO Phrases
dlPhrases login pipe = do
    phrases <- efFromRight (("Error: " ++) . show) =<<
                    liftIO (access pipe master (login ^. db) dbAccess)
    liftIO $ putStrLn "Got phrases: "
    liftIO . putStrLn $ show phrases
    return phrases

(…) = (.).(.)
infixr 9 …

dlPhrasesT :: Config -> Pipe -> IO ThreadId
dlPhrasesT = forkIO … dlPhrasesT'

dlPhrasesT' :: Config -> Pipe -> IO ()
dlPhrasesT' config pipe = do
    phrases <- throwErrorT $ dlPhrases (config ^. login) pipe
    putMVar (config ^. newPhrases) phrases
    threadDelay (60 * 1000 * 1000)
    


writeToJson :: Phrases -> IO ()
writeToJson phrases = do
    let json = encode phrases
    file <- openFile "phrase.json" WriteMode
    hLock file
    LBS.hPut file json
    hUnlock file
    hClose file

dbAccess :: Action IO Phrases
dbAccess = do
    docs <- rest =<< DB.find (select [] "phrases")
    liftIO $ putStrLn $ "Got some phrases?, length " ++ show (length docs)
    nodes <- mFromJust $ sortPhrases docs
    let nodes' = topVotedNodes nodes
    let errPhrases = M.mapWithKey toPhrase nodes' :: M.Map Text (ErrorT String Identity Phrase)
    let (errs, phrases) = M.foldWithKey getError ([], M.empty) errPhrases
    liftIO $ outputErrs errs
    return phrases
  where
    getError :: Text -> ErrorT String Identity Phrase -> ([String], M.Map Text Phrase) -> ([String], M.Map Text Phrase)
    getError k errT (errs, map) =
      case runIdentity . runErrorT $ errT of
        Left err -> (err:errs, map)
        Right phrase -> (errs, M.insert k phrase map)

    outputErrs [] = return ()
    outputErrs list@(x:_) = hPutStrLn stderr "Errors reading phrases:" >>
                            mapM_ (hPutStrLn stderr) list




sortPhrases :: [Document] -> Maybe (Map Text (Map Text (Map Text [Document])))
sortPhrases = foldM sortPhrase M.empty
  where
    sortPhrase map phrase =
        insertPhrase map phrase <$> DB.lookup "phraseName" phrase
                                <*> DB.lookup "pLang" phrase
                                <*> DB.lookup "nLang" phrase
                                <*> DB.lookup "inUse" phrase
    insertPhrase :: Map Text (Map Text (Map Text [Document])) -> Document ->
                      Text -> PLang -> SLang -> Bool ->
                      Map Text (Map Text (Map Text [Document]))
    insertPhrase map phrase node plang slang inUse = if not inUse then map else
        case M.lookup node map of
          Nothing -> M.insert node (M.singleton plang $ M.singleton slang [phrase]) map
          Just nMap ->
            flip (M.insert node) map $ case M.lookup plang nMap of
              Nothing -> M.insert plang (M.singleton slang [phrase]) nMap
              Just pMap ->
                flip (M.insert plang) nMap $ case M.lookup slang pMap of
                  Nothing -> M.insert slang [phrase] pMap
                  Just nlPhrases -> M.insert slang (phrase:nlPhrases) pMap

topVotedNodes :: Map Text (Map Text (Map Text [Document]))
                 -> Map Text (Map Text (Map Text Document))
topVotedNodes = fmap.fmap.fmap $ maximumBy cmpVotes
  where
    cmpVotes d1 d2 = compare (votes d1) (votes d2)
    votes :: Document -> Int
    votes phrase = DB.at "voteCache" phrase

-- a = Phrase; b = Map Text Document; c = PPhrase
toGenPhrase :: (Monad m, Functor m) => (Text -> c -> Map Text c -> a) -> Text -> (Text -> b -> ErrorT String m c)
                                              -> Text -> Map Text b -> ErrorT String m a
toGenPhrase constr def subFunc name map =
    constr name <$> (subFunc def =<< (errFromJust "Missing default (p/s)phrase" $ M.lookup def map)) <*>
                    (TV.sequence . M.mapWithKey subFunc . M.filterWithKey notDef $ map)
-- sequence :: t (m a) -> m (t a)
  where
    notDef k _ = k /= def

toPhrase :: (Functor m, Monad m) => Text -> Map Text (Map Text Document) -> ErrorT String m Phrase
toPhrase = toGenPhrase Phrase "default" toPPhrase

toPPhrase :: (Functor m, Monad m) => Text -> Map Text Document -> ErrorT String m PPhrase
toPPhrase = toGenPhrase PPhrase "en" toSPhrase

toSPhrase :: (Functor m, Monad m) => Text -> Document -> ErrorT String m SPhrase
toSPhrase name doc =
    let clauseDocs = DB.lookup "clauses" doc :: Maybe [DB.Value]
        clauses = mapM toClause =<< errFromJust "Missing clauses for sphrase" clauseDocs
    in SPhrase name <$> clauses


toClause :: (Functor m, Monad m) => DB.Value -> ErrorT String m Clause
toClause (Doc doc) = case DB.lookup "condition" doc of
                 Nothing -> DefClause <$> DB.lookup "words" doc
                 (Just cond) -> CondClause <$> toClauseCond cond <*> DB.lookup "words" doc
toClause (DB.Array vals) = DefClause <$> mapM (errFromJust "Can't convert DB.Value to Text" . DB.cast') vals
    
toClauseCond :: (Functor m, Monad m) => Document -> ErrorT String m ClauseCond
toClauseCond doc =
    case DB.lookup "conditionType" doc :: Maybe Text of
      (Just "presence") -> Present reverse <$> errFromJust "Missing presence clause attribute" (DB.lookup "attribute" doc)
      (Just "comparison") -> Comp reverse <$> comp <*>
                DB.lookup "attribute" doc <*> DB.lookup "compared_with" doc
      _ -> throwError "Missing or invalid clause type"
  where
    reverse = case DB.lookup "reverse" doc of
                (Just True) -> True
                _ -> False
    comp = errFromJust "Missing comparison for comparison phrase" . readMaybe =<< T.unpack <$> DB.lookup "comparator" doc

{-
toPhrase :: Text -> Map Text Document -> Maybe PPhrase
toPPhrase name map = PPhrase name <$> M.lookup "en" map <*>
                      (mapWithKey toSPhrase . filterWithKey notEn $ map)
  where notEn k _ = k /= "en"
-}
