
{-# LANGUAGE OverloadedStrings #-}

import Entologic.Ast
import Entologic.Ast.Json
import Entologic.Phrase.Json
import Entologic.Translate
import Entologic.Output.Json
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

