
{-# LANGUAGE TemplateHaskell
           , OverloadedStrings
           #-}

module Entologic.DB where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS.L8
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text (Text(..))
import Data.Word (Word16(..))
import Data.Endian (swapEndian)

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Error
import Control.Lens.TH
import Control.Concurrent.MVar

import Database.MongoDB as DB

import Network.Socket (PortNumber(..))

import System.IO
import System.Posix.IO

import Entologic.Phrase
import Entologic.Error

type DBInfo = DB.Pipe

data Config = Config { _login :: Login
                     , _astGens :: M.Map Text Text
                     , _phrases :: Phrases
                     , _newPhrases :: MVar Phrases
                     }

data Login = Login { _host :: String
                   , _port :: DB.PortID
                   , _username :: Maybe DB.Username
                   , _password :: Maybe DB.Password
                   , _db :: Text
                   }

$(makeLenses ''Config)
$(makeLenses ''Login)

toPortId :: Word16 -> DB.PortID
toPortId word = DB.PortNumber . PortNum $ swapEndian word

instance FromJSON Login where
    parseJSON (Object obj) = Login <$> obj .: "host"
                                   <*> (toPortId <$> obj .: "port")
                                   <*> obj .:? "username"
                                   <*> obj .:? "password"
                                   <*> obj .: "db"

readJson :: FromJSON a => String -> ErrorT String IO a
readJson filename = eFromRight =<< (liftIO $ eitherDecode <$>
                                                BS.L8.readFile filename)

txtToMaybe :: Text -> Maybe Text
txtToMaybe t = if T.length t > 0 then Just t else Nothing

remEmptyTxt :: Maybe Text -> Maybe Text
remEmptyTxt = (>>= txtToMaybe)

maybeAuth :: Login -> DBInfo -> ErrorT String IO ()
maybeAuth (Login _ _ user pass db) pipe = do
    let possAuth = access pipe master db <$> (auth <$> user <*> pass)
    case possAuth of
      Nothing -> return ()
      Just pa -> do
        authResult <- pa
        case authResult of
          Left err -> throwError $ show err
          Right False -> throwError "authentication failure!"
          _ -> liftIO $ putStrLn "authenticated!"


dbConnect :: Login -> ErrorT String IO DBInfo
dbConnect login@(Login hostname port user pass db) = do
    let host = Host hostname port
    liftIO $ putStrLn $ "connecting to " ++ hostname ++ ":" ++ show port
    liftIO $ putStrLn $ "connecting to " ++ showHostPort host
    pipe <- changeError show $ connect host
    maybeAuth login pipe
    return pipe

hLock :: Handle -> IO ()
hLock = hSetLock WriteLock

hSetLock :: LockRequest -> Handle -> IO ()
hSetLock typ handle = do
    fd <- handleToFd handle
    let fileLock = (typ, AbsoluteSeek, 0, 0)
    setLock fd fileLock

hUnlock :: Handle -> IO ()
hUnlock = hSetLock Unlock
