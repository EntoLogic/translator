
import Control.Concurrent
import Control.Monad.Trans.Error

import Entologic.DB.Translations

main :: IO ()
main = do
    main'
    threadDelay 1000
    main

main' :: IO ()
main' = do
    x <- runErrorT dbInteract
    case x of
      Left err -> putStrLn $ "Error: " ++ err
      Right x -> return x
