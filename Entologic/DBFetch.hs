
{-# LANGUAGE OverloadedStrings #-}

module Entologic.DBFetch where

import Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Map(Map(..))
import Data.Text(Text(..))
import Data.Int

import Control.Applicative
import Control.Monad

import System.Environment

import Database.MongoDB hiding (lookup)
import qualified Database.MongoDB as DB

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
    return ()

sortPhrases :: [Document] -> Maybe (Map Text (Map Text (Map Text [Document])))
sortPhrases = foldM sortPhrase M.empty
  where
    sortPhrase map phrase =
        insertPhrase map phrase <$> DB.lookup "node_name" phrase
                                <*> DB.lookup "plang" phrase
                                <*> DB.lookup "nlang" phrase
    insertPhrase map phrase node plang slang =
        case M.lookup node map of
          Nothing -> M.insert node (M.singleton plang $ M.singleton slang [phrase]) map
          Just nMap ->
            flip (M.insert node) map $ case M.lookup plang nMap of
              Nothing -> M.insert plang (M.singleton slang [phrase]) nMap
              Just pMap ->
                flip (M.insert plang) nMap $ case M.lookup slang pMap of
                  Nothing -> M.insert slang [phrase] pMap
                  Just nlPhrases -> M.insert slang (phrase:nlPhrases) pMap

topVotedNodes = fmap.fmap.fmap $ maximumBy cmpVotes
  where
    cmpVotes d1 d2 = compare (votes d1) (votes d2)
    votes phrase = foldl count 0 (DB.at "votes" phrase)
    count acc (k := (Doc vote)) = acc + (DB.at "value" vote :: Int32)
