
import Control.Concurrent
import Control.Monad.Error
import Control.Lens

import Entologic.Error
import Entologic.DB
import Entologic.DB.Translations
import Entologic.DB.Phrases

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
    pipe <- dbConnect (config ^. login)
    phrases' <- dlPhrases (config ^. login) pipe
    let config' = config & phrases .~ Just phrases'
    liftIO $ dlPhrasesT config pipe
    liftIO $ loop config pipe
  where
    loop config pipe = do
        config' <- updateConfig config
        accessDb config' pipe
        threadDelay 3000000
        loop config' pipe

updateConfig :: Config -> IO Config
updateConfig config = do
    maybePhrases <- tryTakeMVar $ config ^. newPhrases
    case maybePhrases of
      Nothing -> return config
      Just ps -> return $ config & phrases .~ Just ps

accessDb :: Config -> DB.Pipe -> IO ()
accessDb config pipe = errorTToIO $ dbInteract config pipe
