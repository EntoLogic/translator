
{-# LANGUAGE OverloadedStrings #-}

import Entologic.Ast
import Entologic.JsonAst
import Entologic.JsonPhrase
import Entologic.Translate

main = do
    phrases <- readPhrases "phrase.json"
    ast <- readAst "example.json"
    runTL (TLInfo phrases "ruby" "en") (TLState False) (translate . (!!0) . pEntries $ uProg ast)
