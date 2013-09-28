
module Entologic.Phrase where

import Data.Text
import qualified Data.Map as M

type Lang = Text
type NLang = Text

data Phrase = Phrase { phName :: Text, phVal :: [Text], phValNLangs :: M.Map NLang [Text],
                       phLangs :: M.Map Lang Phrase }
            | LangPhrase { phName :: Text, phVal :: [Text], phValNLangs :: M.Map NLang [Text]}
              deriving (Show, Ord, Eq)

type Phrases = M.Map Text Phrase


