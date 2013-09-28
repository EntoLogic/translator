
{-# LANGUAGE OverloadedStrings,ExtendedDefaultRules #-}

module Entologic.ToEnglish where

import qualified Data.Map as M
import Control.Applicative ((<$>))
import qualified Data.Text as T
import Data.Text(Text(..))

import Entologic.Ast
import Entologic.Phrase


langPhrase :: Lang -> Phrase -> Phrase
langPhrase lang phrase = case M.lookup lang $ phLangs phrase of
                           Nothing -> phrase
                           Just lp -> lp

nLangPhrase :: NLang -> Phrase -> [Text]
nLangPhrase nLang phrase = case M.lookup nLang $ phValNLangs phrase of
                             Nothing -> phVal phrase
                             Just val -> val

replace :: [Text] -> [(Text, Text)] -> Text
replace template repls = T.concat $ map (replace' repls) template
  where
    replace' repls toRepl
      | "$$" `T.isPrefixOf` toRepl =
          case lookup (T.drop 2 toRepl) repls of
          Nothing -> toRepl 
          Just r -> r
      | otherwise = toRepl

instance AstNode Program where
    toEng phrases node lang = replace phraseContents replacements
      where
        phraseContents = maybe ["Error"] (nLangPhrase "en" . langPhrase lang) $ M.lookup "program" phrases
        replacements = [("contents", foldl (T.append) $ map toEng $ pEntries node)]
        
