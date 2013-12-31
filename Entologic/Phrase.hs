
{-# LANGUAGE TemplateHaskell,
             OverloadedStrings,
             Rank2Types,
             ExistentialQuantification #-}

module Entologic.Phrase
    ( Phrase(..)
    , phNode
    , phDefault
    , phLangs
    , PPhrase(..)
    , ppLang
    , pSEnglish
    , pSLangs
    , SPhrase(..)
    , spLang
    , spClauses
    , Clause(..)
    , ClauseCond(..)
    , Phrases
    , PLang
    , SLang
    , SPhrases
    , PPhrases
    , Variable(..)
    , AnyVariable(..)
    , AnyVariables
    , insertClauses
    , getClauses
    ) where

import qualified Data.Text as T
import Data.Text (Text(..))
import qualified Data.Map as M
import Data.Maybe

import Control.Monad.Reader.Class
import Control.Lens

import Entologic.Base
import Entologic.Output
import Entologic.Error

{-
Some type that can be used as a phrase variable, either in a phrase, as a
    presence condition or as a comparison condition.
-}
class Variable a where
    present :: a -> Bool
    comparison :: a -> Int
--    inPhrase :: a -> TL OutputClause
    inPhrase :: a -> OutputClause


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




data AnyVariable = forall a. Variable a => AV { unAV :: a}

instance Variable AnyVariable where
    present (AV x) = present x
    comparison (AV x) = comparison x
    inPhrase (AV x) = inPhrase x

type AnyVariables = M.Map Text AnyVariable

vars_ :: AnyVariables
vars_ = undefined

insertClauses :: [Clause] -> AnyVariables -> [OutputClause]
insertClauses clauses vars = concat $ mapMaybe insertClause clauses
  where
    insertClause :: Clause -> Maybe [OutputClause]
    insertClause (DefClause pieces) = Just $ replaceVars pieces
    insertClause (CondClause cond pieces) = if evalCond cond
                                            then Just $ replaceVars pieces
                                            else Nothing

    evalCond :: ClauseCond -> Bool
    evalCond cc = if ccNot cc
                  then evalCond' cc
                  else not $ evalCond' cc
    evalCond' (Present _ attr) = maybe False present $ M.lookup attr vars
    evalCond' (Comp _ comp attr value) = maybe False (compCond . comparison) $
                                            M.lookup attr vars
        where compCond :: Int -> Bool
              compCond attrVal = attrVal `compare` value == comp

    replaceVars :: [Text] -> [OutputClause]
    replaceVars = map replaceVar

    replaceVar :: Text -> OutputClause
    replaceVar t
        | "$$" `T.isPrefixOf` t = maybe (OCString t) inPhrase $
                                    M.lookup (T.drop 2 t) vars
        | otherwise = OCString t
