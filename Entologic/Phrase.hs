
{-# LANGUAGE TemplateHaskell,OverloadedStrings #-}

module Entologic.Phrase where

import Data.Text
import qualified Data.Map as M
import Control.Lens

type PLang = Text
type SLang = Text

type SPhrases = M.Map SLang SPhrase
type PPhrases = M.Map PLang PPhrase

data Phrase = Phrase { _phNode :: Text
                     , _phDefault :: PPhrase
                     , _phLangs :: PPhrases
                     }
              deriving (Ord, Eq, Show)

data PPhrase = PPhrase { _ppLang :: Text
                       , _pSEnglish :: SPhrase
                       , _pSLangs :: SPhrases
                       }
               deriving (Ord, Eq, Show)

data SPhrase = SPhrase { _spLang :: Text
                       , _spClauses :: [Clause]
                       }
               deriving (Ord, Eq, Show)

data Clause = DefClause Text
            | CondClause { cond :: ClauseCond, clause :: [Text] }
              deriving (Ord, Eq, Show)

data ClauseCond = Present { ccNot :: Bool
                          , ccAttribute :: Text
                          }
                | Comp { ccNot :: Bool
                       , ccComparison :: Ordering
                       , ccAttribute :: Text
                       , ccValue :: Int
                       }
                  deriving (Ord, Eq, Show)

type Phrases = M.Map Text Phrase

$(makeLenses ''Phrase)
$(makeLenses ''PPhrase)
$(makeLenses ''SPhrase)

