
{-# LANGUAGE OverloadedStrings,ExtendedDefaultRules,GeneralizedNewtypeDeriving,RankNTypes #-}

module Entologic.Translate where

import qualified Data.Map as M
import Control.Applicative ((<$>))
import qualified Data.Text as T
import Data.Text(Text(..))
import Data.Maybe (fromJust)

import Entologic.Ast
import Entologic.Phrase
import Control.Lens
import Control.Monad.Reader

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

nodeClause :: Text -> PLang -> SLang -> Fold Phrases [Clause]
nodeClause node pl sl = (at node . _Just) . phraseClauses pl sl

getClauses :: Text -> TL (Maybe [Clause])
getClauses node = do
    (TLInfo phrases pl sl) <- ask
    return $ phrases ^? nodeClause node pl sl
    

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

instance AstNode Program where
    translate node = do
        clauses <- getClauses "program" 
        contents <- T.concat <$> (mapM translate $ pEntries node)
        let vars = M.fromList [("contents", contents)]
        let conds = []
        let cconds = M.empty
        return $ insertClauses (fromJust clauses) vars conds cconds
      where

instance AstNode ProgramEntry where
    translate (PEStm s) = toEng s
    -- TODO: Other ProgramEntry types

instance AstNode Statement where
    translate (StmExpr e) = toEng e
    -- TODO: Other Statement types

instance AstNode Expression where
    translate (BinOp op lexpr rexpr) = do
        clauses <- getClauses "expression"
        tOp <- translate op
        left <- translate lexpr
        right <- translate rexpr
        let vars = M.fromList [("op", tOp), ("left", left), ("right", right)]
        return $ insertClauses (fromJust clauses) vars conds cconds
        

{-
instance AstNode Program where
    translate node = do
        phrases <- cPhrases <$> get
        replace phraseContents replacements
      where
        phraseContents = maybe ["Error"] (nLangPhrase n . langPhrase lang) $ M.lookup "program" phrases
        replacements = [("contents", foldl (T.append) "" $ map (translate phrases lang) $ pEntries node)]

phrase :: Phrases -> Text -> Lang -> NLang -> [Text]
phrases phrases name l nl = maybe ["Error"] (nLangPhrase nl . langPhrase l) $ M.lookup name phrases

        
instance AstNode ProgramEntry where
    translate (PEFunc f) = toEng f
--    translate p l (PECls c) = toEng p l c
--    translate p l (PEStm s) = toEng p l s

instance AstNode Statement where
    translate p l n (VarDecl typ nm init) = replace contents replacements
      where
        contents = case init of
                     Nothing -> phrase p "vardecl" l n
                     Just _ -> phrase p "vardecl.init" l n
        replacements = [("type", translate typ), ("name", nm), ("init", toEng $ fromJust init)]
-}
