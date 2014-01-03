
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Map(Map(..))
import Data.Text(Text(..))
import Data.Int
import qualified Data.Traversable as TV

import Control.Applicative
import Control.Monad

import System.Environment

import Database.MongoDB hiding (lookup)
import qualified Database.MongoDB as DB

import Entologic.Phrase

main :: IO ()
main = do
    (dbHost:args) <- getArgs
    pipe <- runIOE . connect $ host dbHost
    phrases <- access pipe master "entologic_dev" dbAccess
    close pipe

mFromJust :: Monad m => Maybe a -> m a
mFromJust = maybe (fail "") return

dbAccess :: Action IO ()
dbAccess = do
    docs <- rest =<< DB.find (select [] "phrases")
    nodes <- mFromJust $ sortPhrases docs
    let nodes' = topVotedNodes nodes
    let phrases = M.mapWithKey toPhrase nodes'
    return ()

sortPhrases :: [Document] -> Maybe (Map Text (Map Text (Map Text [Document])))
sortPhrases = foldM sortPhrase M.empty
  where
    sortPhrase map phrase =
        insertPhrase map phrase <$> DB.lookup "phraseName" phrase
                                <*> DB.lookup "pLang" phrase
                                <*> DB.lookup "nLang" phrase
                                <*> DB.lookup "inUse" phrase
    insertPhrase :: Map Text (Map Text (Map Text [Document])) -> Document -> Text -> PLang -> SLang -> Bool -> Map Text (Map Text (Map Text [Document]))
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

topVotedNodes :: Map Text (Map Text (Map Text [Document])) -> Map Text (Map Text (Map Text Document))
topVotedNodes = fmap.fmap.fmap $ maximumBy cmpVotes
  where
    cmpVotes d1 d2 = compare (votes d1) (votes d2)
    votes :: Document -> Int
    votes phrase = DB.at "voteCache" phrase

-- a = Phrase; b = Map Text Document; c = PPhrase
toGenPhrase :: (Text -> c -> Map Text c -> a) -> Text -> (Text -> b -> Maybe c)
                                              -> Text -> Map Text b -> Maybe a
toGenPhrase constr def subFunc name map =
    constr name <$> (subFunc def =<< (M.lookup def map)) <*>
                    (TV.sequence . M.mapWithKey subFunc . M.filterWithKey notDef $ map)
  where
    notDef k _ = k /= def

toPhrase :: Text -> Map Text (Map Text Document) -> Maybe Phrase
toPhrase = toGenPhrase Phrase "default" toPPhrase

toPPhrase :: Text -> Map Text Document -> Maybe PPhrase
toPPhrase = toGenPhrase PPhrase "en" toSPhrase

toSPhrase :: Text -> Document -> Maybe SPhrase
toSPhrase name doc =
    let clauseDocs = DB.lookup "clauses" doc :: Maybe [Document]
        clauses = mapM toClause =<< clauseDocs
    in SPhrase name <$> clauses


toClause :: Document -> Maybe Clause
toClause doc = case DB.lookup "condition" doc of
                 Nothing -> DefClause <$> DB.lookup "words" doc
                 (Just cond) -> CondClause <$> toClauseCond cond <*> DB.lookup "words" doc
    
toClauseCond :: Document -> Maybe ClauseCond
toClauseCond doc =
    case DB.lookup "conditionType" doc :: Maybe Text of
      (Just "presence") -> Present reverse <$> DB.lookup "attribute" doc
      (Just "comparison") -> Comp reverse <$> comp <*>
                DB.lookup "attribute" doc <*> DB.lookup "compared_with" doc
      _ -> Nothing
  where
    reverse = case DB.lookup "reverse" doc of
                (Just True) -> True
                _ -> False
    comp = read . T.unpack <$> DB.lookup "comparator" doc

{-
toPhrase :: Text -> Map Text Document -> Maybe PPhrase
toPPhrase name map = PPhrase name <$> M.lookup "en" map <*>
                      (mapWithKey toSPhrase . filterWithKey notEn $ map)
  where notEn k _ = k /= "en"
-}
