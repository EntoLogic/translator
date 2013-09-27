
module Entologic.Phrase where

type Lang = Text

type Phrase = Phrase { phName :: Text, phDefs :: M.Map Text [Text], phLangs :: M.Map Lang Phrase }
            | LangPhrase { phName :: Text, phDefs :: M.Map Text [Text] }
              deriving (Show, Ord, Eq)

type Phrases = M.Map Text Phrase


