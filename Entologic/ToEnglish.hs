
{-# LANGUAGE OverloadedStrings,ExtendedDefaultRules #-}

module Entologic.ToEnglish where

import Data.Map as M
import Entologic.AST
import Control.Applicative ((<$>))
import Database.MongoDB.Query
import Data.Bson

type DB = M.Map String String

langNode :: Document -> Lang -> Document
langNode doc lang = case look lang doc of
                      Nothing -> doc
                      Just (Doc d) -> d

replace :: [Text] -> [(Text, Text)] -> Text

instance ASTNode Program where
    toEng node lang = do
        nodeDoc <- (!!0) <$> (find (select ["node" =: "program"] "nodes") >>= rest)
        let langDoc = langNode nodeDoc lang
        let english = replace (look "contents" langDoc) replacements
        return ""

      where
        contents = []
        
