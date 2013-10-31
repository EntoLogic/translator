
{-# LANGUAGE TemplateHaskell,OverloadedStrings #-}

module Entologic.Phrase where

import Data.Text
import qualified Data.Map as M
import Control.Lens

type Lang = Text
type NLang = Text

data Phrase = Phrase { _phNode :: Text, _phDefault :: [NPhrase], _phLangs :: M.Map NLang PPhrase }
                  deriving (Ord, Eq, Show)

data PPhrase = PPhrase { _lpLang :: Text, _lpNLangs :: [NPhrase] }
                  deriving (Ord, Eq, Show)

data NPhrase = NPhrase { _npLang :: Text, _npClauses :: [Clause] }
                   deriving (Ord, Eq, Show)

data Clause = DefClause Text
            | CondClause { cond :: ClauseCond, clause :: [Text] }
              deriving (Ord, Eq, Show)

data ClauseCond = Present { not :: Bool, attribute :: Text }
                | Comp { not :: Bool, comparison :: Ordering, attribute :: Text, value :: Text}
                  deriving (Ord, Eq, Show)

type Phrases = M.Map Text Phrase

$(makeLenses ''Phrase)
$(makeLenses ''PPhrase)
$(makeLenses ''NPhrase)

