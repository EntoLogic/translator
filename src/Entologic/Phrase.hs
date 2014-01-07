
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
import Data.Semigroup ((<>))

import Control.Monad.Reader.Class
import Control.Monad.Error.Class
import Control.Lens
import Control.Applicative ((<$>))

import qualified Database.MongoDB as DB

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
    inPhrase :: a -> TL [OutputClause]


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

phraseSPhrase :: PLang -> SLang -> Getter Phrase SPhrase
phraseSPhrase pl sl = langPhrase pl . sLangPhrase sl

nodeSPhrase :: Text -> PLang -> SLang -> Fold Phrases SPhrase
nodeSPhrase node pl sl = (at node . _Just) . phraseSPhrase pl sl

nodeClause :: Text -> PLang -> SLang -> Fold Phrases [Clause]
nodeClause node pl sl = (at node . _Just) . phraseClauses pl sl


getClauses :: Text -> TL (Maybe ([Clause], DB.ObjectId))
getClauses node = do
    (TLInfo phrases pl sl) <- ask
    let sPhrase = phrases ^? nodeSPhrase node pl sl
    case sPhrase of
      (Just (SPhrase _ clauses id)) -> return $ Just (clauses, id)
      Nothing ->
        case sl of
          "en" -> throwError $ "Missing phrase for "
                               ++ T.unpack (pl <> ":" <> sl <> ":" <>node)
          _ -> return Nothing
    

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

mapMaybeM :: (Monad m, Functor m) => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f = fmap catMaybes . mapM f

insertClauses :: [Clause] -> AnyVariables -> TL [OutputClause]
insertClauses clauses vars = concat <$> mapMaybeM insertClause clauses
  where
    insertClause :: Clause -> TL (Maybe [OutputClause])
    insertClause (DefClause pieces) = Just <$> replaceVars pieces
    insertClause (CondClause cond pieces) = if evalCond cond
                                            then Just <$> replaceVars pieces
                                            else return Nothing

    evalCond :: ClauseCond -> Bool
    evalCond cc = if ccNot cc
                  then not $ evalCond' cc
                  else evalCond' cc
    evalCond' (Present _ attr) = maybe False present $ M.lookup attr vars
    evalCond' (Comp _ comp attr value) = maybe False (compCond . comparison) $
                                            M.lookup attr vars
        where compCond :: Int -> Bool
              compCond attrVal = attrVal `compare` value == comp

    replaceVars :: [Text] -> TL [OutputClause]
    replaceVars = fmap concat . mapM replaceVar

    replaceVar :: Text -> TL [OutputClause]
    replaceVar t
        | "$$" `T.isPrefixOf` t = maybe (return [OCString t]) inPhrase $
                                    M.lookup (T.drop 2 t) vars
        | otherwise = return [OCString t]
