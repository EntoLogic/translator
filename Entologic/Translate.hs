
{-# LANGUAGE OverloadedStrings,ExtendedDefaultRules,GeneralizedNewtypeDeriving,RankNTypes #-}

module Entologic.Translate where

import qualified Data.Map as M
import Control.Applicative ((<$>))
import qualified Data.Text as T
import Data.Text(Text(..))

import Entologic.Ast
import Entologic.Phrase
import Control.Lens

langPhrase :: PLang -> Getter Phrase PPhrase
langPhrase lang = to $ _langPhrase lang

_langPhrase :: PLang -> Phrase -> PPhrase
_langPhrase lang phrase = case M.lookup lang $ phrase ^. phLangs of
                           Nothing -> phrase ^. phDefault
                           Just lp -> lp

sLangPhrase :: SLang -> Getter PPhrase SPhrase
sLangPhrase lang = to $ _sLangPhrase lang

_sLangPhrase :: SLang -> PPhrase -> SPhrase
_sLangPhrase nLang pphrase = case M.lookup nLang $ pphrase ^. pSLangs of
                             Nothing -> pphrase ^. pSEnglish
                             Just val -> val

phraseClauses :: PLang -> SLang -> Getter Phrase [Clause]
phraseClauses pl sl = langPhrase pl . sLangPhrase sl . spClauses

getClause :: Text -> PLang -> SLang -> Fold Phrases [Clause]
getClause node pl sl = (at node . _Just) . phraseClauses pl sl

type Variables = M.Map Text Text
type Conditions = [Text]
type CompConditions = M.Map Text Int

insertClauses :: [Clause] -> Variables -> Conditions -> CompConditions -> Text
insertClauses clauses vars conds cconds = T.concat . map insertClause $ clauses
  where
    insertClause :: Clause -> Text
    insertClause (DefClause pieces) = replaceVars pieces
    insertClause (CondClause cond pieces) = if evalCond cond
                                            then replaceVars pieces
                                            else ""
    evalCond cc = if ccNot cc
                  then evalCond' cc
                  else not $ evalCond' cc
    evalCond' (Present _ attr) = attr `elem` conds
    evalCond' (Comp _ comp attr value) = maybe False compCond $ M.lookup attr cconds
        where compCond :: Int -> Bool
              compCond attrVal = attrVal `compare` value == comp

    replaceVars pieces = T.concat $ map replaceVar pieces

    replaceVar :: Text -> Text
    replaceVar t
        | "$$" `T.isPrefixOf` t = maybe t id $ M.lookup (T.drop 2 t) vars
        | otherwise           = t
{-
instance AstNode Program where
    toEng node = do
       clause <- inner

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
-}
