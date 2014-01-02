
import Control.Concurrent
import Control.Monad.Error

import Entologic.DB.Translations

import qualified Database.MongoDB as DB

import Data.Endian (swapEndian)
import Data.Word (Word16(..))
import Network.Socket (PortNumber(..))
import qualified Data.ByteString.Char8 as L8

main :: IO ()
main = do
    res <- runErrorT main'
    case res of
        Right x -> return x
        Left err -> putStrLn $ "Error: " ++ err


main' :: ErrorT String IO ()
main' = do
    config <- loadConfigs
    dbInfo <- dbConnect config
    liftIO $ accessDb config dbInfo
    liftIO $ threadDelay 3000000
    main'

accessDb :: Config -> DBInfo -> IO ()
accessDb config pipe = do
    x <- runErrorT $ dbInteract config pipe
    case x of
      Left err -> putStrLn $ "Error: " ++ err
      Right x -> return x
