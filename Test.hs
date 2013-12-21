
{-# LANGUAGE OverloadedStrings #-}

import Entologic.Ast
import Entologic.JsonAst
import Entologic.JsonPhrase
import Entologic.Translate
import Entologic.JsonOutput
import Text.Show.Pretty
import Data.Aeson
import Data.ByteString.Lazy.Char8 as L

main = do
    phrases <- readPhrases "phrase.json"
    ast <- readAst "example.json"
    x <- runTL (TLInfo phrases "ruby" "en") (TLState False) (translate . (!!0) . pEntries $ uProg ast)
    case x of
        (Right y) -> L.putStrLn . encode $ y
        _ -> return ()

