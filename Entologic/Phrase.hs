
{-# LANGUAGE TemplateHaskell,OverloadedStrings #-}

module Entologic.Phrase where

import Data.Text
import qualified Data.Map as M
import Control.Lens

type Lang = Text
type NLang = Text

data Phrase = Phrase { _phName :: Text, _phVal :: [Text], _phValNLangs :: M.Map NLang [Text],
                       _phLangs :: M.Map Lang Phrase }
            | LangPhrase { _phName :: Text, _phVal :: [Text], _phValNLangs :: M.Map NLang [Text], _phx :: Int}
              deriving (Show, Ord, Eq)

type Phrases = M.Map Text Phrase

$(makeLenses ''Phrase)

exampleLP = LangPhrase "meow" ["a phrase"] M.empty 3
exampleP = Phrase "meow" ["a phrase"] M.empty M.empty
