
{-# LANGUAGE OverloadedStrings,
             ExtendedDefaultRules,
             GeneralizedNewtypeDeriving,
             RankNTypes #-}

module Entologic.Translate where

import qualified Data.Map as M
import Control.Applicative ((<$>))
import qualified Data.Text as T
import Data.Text(Text(..))
import Data.Maybe (fromJust, mapMaybe)
import Control.Monad.Error.Class

import Entologic.Ast
import Entologic.Phrase
import Entologic.Output
import Entologic.Error
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State.Class

bracket x y = T.cons x . flip T.snoc y

bracket' :: Char -> Char -> [OutputClause] -> [OutputClause]
bracket' x y = cons (OCString $ T.pack [x]) . flip snoc (OCString $ T.pack [y])

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


getClauses :: Text -> TL [Clause]
getClauses node = do
    (TLInfo phrases pl sl) <- ask
    errFromJust ("clauses for " ++ T.unpack node) $ phrases ^? nodeClause node pl sl
    

type Variables = M.Map Text OutputClause
type Conditions = [Text]
type CompConditions = M.Map Text Int



insertClauses :: [Clause] -> Variables -> Conditions -> CompConditions
                          -> [OutputClause]
insertClauses clauses vars conds cconds = concat $ mapMaybe insertClause clauses
  where
    insertClause :: Clause -> Maybe [OutputClause]
    insertClause (DefClause pieces) = Just $ replaceVars pieces
    insertClause (CondClause cond pieces) = if evalCond cond
                                            then Just $ replaceVars pieces
                                            else Nothing

    evalCond cc = if ccNot cc
                  then evalCond' cc
                  else not $ evalCond' cc
    evalCond' (Present _ attr) = attr `elem` conds
    evalCond' (Comp _ comp attr value) = maybe False compCond $ M.lookup attr cconds
        where compCond :: Int -> Bool
              compCond attrVal = attrVal `compare` value == comp

    replaceVars :: [Text] -> [OutputClause]
    replaceVars = map replaceVar

    replaceVar :: Text -> OutputClause
    replaceVar t
        | "$$" `T.isPrefixOf` t = maybe (OCString t) id $ M.lookup (T.drop 2 t) vars
        | otherwise = OCString t

chooseM :: Functor m => m Bool -> a -> a -> m a
chooseM cond x y = cond <$$> \c -> if c then x else y

chooseL :: (MonadState s m, Functor m) => Getter s Bool -> a -> a -> m a
chooseL getter = chooseM (use getter)

localS :: MonadState s m => (s -> s) -> m a -> m a
localS mod action = do
    before <- get
    put $ mod before
    a <- action
    put before
    return a


result node translation = return . OCNode $ OutputNode (name node) translation
                            False (Area Nothing Nothing)

on2Text (OutputNode _ ((OCString t):_) _ _) = t
oc2Text (OCNode x) = on2Text x

instance AstNode Program where
    name = const "program"
    translate node = do
        clauses <- getClauses "program" 
        contents <- OCNodes <$> (mapM (fmap ocNode . translate) $ pEntries node)
        let vars = M.fromList [("contents", contents)]
            conds = []
            cconds = M.empty
            translation = insertClauses clauses vars conds cconds
        return . OCNode $ OutputNode (name node) translation False (Area Nothing Nothing)

instance AstNode ProgramEntry where
    name (PEStm s) = name s
    translate (PEStm s) = translate s
    -- TODO: Other ProgramEntry types

instance AstNode Statement where
    translate (StmExpr e) = translate e
    -- TODO: Other Statement types

instance AstNode Expression where
    name (BinOp {}) = "BinaryExpr"
    name (IntLit _) = "IntLit"

    translate node@(BinOp op lexpr rexpr) = do
        clauses <- getClauses "BinaryExpr"
        sOp <- iOpSym op
        tOp <- translate op
        lOp <- iOpLong op
        left <- subexpr $ translate lexpr
        right <- subexpr $ translate rexpr

        parens <- chooseL sInSubExpr (bracket' '(' ')') id
        let vars = M.fromList [("opSymbol", sOp), ("opText", tOp), ("opTextLong", lOp), ("left", left), ("right", right)]
            conds = []
            cconds = M.empty
            translation = parens $ insertClauses clauses vars conds cconds
        result node translation
            
      where
        subexpr = localS (sInSubExpr .~ True)

    translate node@(IntLit val) = do
        clauses <- getClauses "IntLit"
        let vars = M.fromList [("value", OCString . T.pack $ show val)]
        result node $ insertClauses clauses vars [] M.empty

iOpSym = const $ return undefined
iOpLong = const $ return undefined

instance AstNode InfixOp where
    translate node = do
        node_ <- eFromJust $ M.lookup node translations
        clauses <- getClauses node_
        result node $ insertClauses clauses M.empty [] M.empty
      where
        translations = M.fromList [(Plus, "add"), (Minus, "subtract"),
            (Mult, "multiply"), (Div, "divide"), (Mod, "modulo"),
            (LOr, "logicalOr"), (LAnd, "logicalAnd"), (BOr, "bitOr"), (BAnd, "bitAnd"),
            (Xor, "xor"), (RShift, "rShift"),
            (LShift, "lShift"), (RUShift, "ruShift")]

