
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
    toEng node = do
        phrases <- cPhrases <$> get
        replace phraseContents replacements
      where
        phraseContents = maybe ["Error"] (nLangPhrase n . langPhrase lang) $ M.lookup "program" phrases
        replacements = [("contents", foldl (T.append) "" $ map (toEng phrases lang) $ pEntries node)]

phrase :: Phrases -> Text -> Lang -> NLang -> [Text]
phrases phrases name l nl = maybe ["Error"] (nLangPhrase nl . langPhrase l) $ M.lookup name phrases

        
instance AstNode ProgramEntry where
    toEng (PEFunc f) = toEng f
--    toEng p l (PECls c) = toEng p l c
--    toEng p l (PEStm s) = toEng p l s

instance AstNode Statement where
    toEng p l n (VarDecl typ nm init) = replace contents replacements
      where
        contents = case init of
                     Nothing -> phrase p "vardecl" l n
                     Just _ -> phrase p "vardecl.init" l n
        replacements = [("type", toEng typ), ("name", nm), ("init", toEng $ fromJust init)]

instance AstNode Expression where
    toEng p l n (
