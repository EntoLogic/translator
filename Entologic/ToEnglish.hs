
{-# LANGUAGE OverloadedStrings,ExtendedDefaultRules #-}

module Entologic.ToEnglish where

import Data.Map as M
import Entologic.AST
import Control.Applicative ((<$>))
import Database.MongoDB.Query
import Data.Bson
import Data.Text as T


langNode :: Phrase -> Lang -> Phrase
langNode phrase lang = case M.lookup $ phLangs phrase of
                      Nothing -> phrase
                      Just lp -> lp

replace :: [Text] -> [(Text, Text)] -> Text
replace template repls = map (replace' repls) template
  where
    replace' repls toRepl
      | "$$" `T.isPrefixOf` toRepl =
          case lookup rest repls of
          Nothing -> toRepl 
          Just r -> r
      | otherwise = toRepl

instance ASTNode Program where
    toEng node lang = do
        nodeDoc <- (!!0) <$> (find (select ["node" =: "program"] "nodes") >>= rest)
        let langDoc = langNode nodeDoc lang
        let english = replace (look "contents" langDoc) replacements
        return ""

      where
        contents = [("contents", foldl (T.append) $ map toEng $ pEntries node)]
        
