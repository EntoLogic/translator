
import Control.Concurrent
import Control.Monad.Error
import Control.Lens

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
    liftIO $ dlPhrasesT config
    liftIO $ loop config dbInfo
  where
    loop config dbInfo = do
        config' <- updateConfig config
        accessDb config' dbInfo
        threadDelay 3000000
        loop config' dbInfo

updateConfig :: Config -> IO Config
updateConfig config = do
    maybePhrases <- tryTakeMVar $ config ^. newPhrases
    case maybePhrases of
      Nothing -> return config
      Just ps -> return $ config & phrases .~ ps

accessDb :: Config -> DBInfo -> IO ()
accessDb config pipe = errorTToIO $ dbInteract config pipe
