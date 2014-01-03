
import Control.Concurrent
import Control.Monad.Error
import Control.Lens ((^.))

import Entologic.Error
import Entologic.DB
import Entologic.DB.Translations

import qualified Database.MongoDB as DB

import Network.Socket (PortNumber(..))

import System.IO

import Data.Endian (swapEndian)
import Data.Word (Word16(..))
import qualified Data.ByteString.Char8 as L8

main :: IO ()
main = errorTToIO main'


main' :: ErrorT String IO ()
main' = do
    config <- loadConfigs
    dbInfo <- dbConnect (config ^. login)
    forever $ do
        liftIO $ accessDb config dbInfo
        liftIO $ threadDelay 3000000

accessDb :: Config -> DBInfo -> IO ()
accessDb config pipe = errorTToIO $ dbInteract config pipe

{-
 - testing
main = do
    handle <- openFile "login_example.json" WriteMode
    hLock handle
    getLine
    hUnlock handle
-}
