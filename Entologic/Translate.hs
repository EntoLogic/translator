
{-# LANGUAGE OverloadedStrings,
             ExtendedDefaultRules,
             GeneralizedNewtypeDeriving,
             RankNTypes #-}

module Entologic.Translate where

import qualified Data.Map as M
import Control.Applicative ((<$>))
import qualified Data.Text as T
import Data.Text(Text(..))
import Data.Maybe (fromJust)
import Control.Monad.Error.Class

import Entologic.Ast
import Entologic.Phrase
import Control.Lens
import Control.Monad.Reader

bracket x y = T.cons x . flip T.snoc y

(<$$>) :: Functor f => f a -> (a -> b) -> f b
(<$$>) = flip (<$>)
infixl 4 <$$>

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

eFromJust :: Maybe a -> TL a
eFromJust (Just a) = return a
eFromJust Nothing = throwError noMsg

errFromJust :: String -> Maybe a -> TL a
errFromJust _ (Just a) = return a
errFromJust err Nothing = throwError err

getClauses :: Text -> TL [Clause]
getClauses node = do
    (TLInfo phrases pl sl) <- ask
    errFromJust ("clauses for " ++ T.unpack node) $ phrases ^? nodeClause node pl sl
    

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
        return $ insertClauses clauses vars conds cconds
      where

instance AstNode ProgramEntry where
    translate (PEStm s) = translate s
    -- TODO: Other ProgramEntry types

instance AstNode Statement where
    translate (StmExpr e) = translate e
    -- TODO: Other Statement types

instance AstNode Expression where
    translate (BinOp op lexpr rexpr) = do
        clauses <- getClauses "BinaryExpr"
        sOp <- iOpSym op
        tOp <- translate op
        lOp <- iOpLong op
        left <- translate lexpr
        right <- translate rexpr
        let vars = M.fromList [("opSymbol", sOp), ("opText", tOp), ("opTextLong", lOp), ("left", left), ("right", right)]
        let conds = []
        let cconds = M.empty
        return $ insertClauses clauses vars conds cconds
    translate (IntLit val) = do
        clauses <- getClauses "IntLit"
        let vars = M.fromList [("value", T.pack $ show val)]
        return $ insertClauses clauses vars [] M.empty

iOpSym = const $ return ""
iOpLong = const $ return undefined

instance AstNode InfixOp where
    translate node = do
        node <- eFromJust $ M.lookup node translations
        clauses <- getClauses node
        parens <- (use sInSubExpr) <$$> \x ->
                     if x
                     then bracket '(' ')'
                     else id
        return $ insertClauses clauses M.empty [] M.empty
      where
        translations = M.fromList [(Plus, "add"), (Minus, "subtract"),
            (Mult, "multiply"), (Div, "divide"), (Mod, "modulo"),
            (LOr, "logicalOr"), (LAnd, "logicalAnd"), (BOr, "bitOr"), (BAnd, "bitAnd"),
            (Xor, "xor"), (RShift, "rShift"),
            (LShift, "lShift"), (RUShift, "ruShift")]

