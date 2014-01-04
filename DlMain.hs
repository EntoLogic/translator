
module Main where

import Control.Monad.Error

import Entologic.DB.Phrases
import Entologic.DB
import Entologic.Error

main' :: ErrorT String IO ()
main' = do
    login <- readJson "login.json"
    phrases <- dlPhrases login
    liftIO $ writeToJson phrases

main :: IO ()
main = errorTToIO main'
