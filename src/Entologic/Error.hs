
module Entologic.Error where

import Control.Monad.Error.Class
import Control.Monad.Error

eFromJust :: (Error e, MonadError e m) => Maybe a -> m a
eFromJust (Just a) = return a
eFromJust Nothing = throwError noMsg

errFromJust :: (MonadError e m) => e -> Maybe a -> m a
errFromJust _ (Just a) = return a
errFromJust err Nothing = throwError err

mFromJust :: Monad m => Maybe a -> m a
mFromJust = msgFromJust ""

msgFromJust :: Monad m => String -> Maybe a -> m a
msgFromJust msg = maybe (fail msg) return

eFromRight :: (MonadError e m) => Either e a -> m a
eFromRight (Right a) = return a
eFromRight (Left e) = throwError e

efFromRight :: (MonadError e m) => (b -> e) -> Either b a -> m a
efFromRight _ (Right a) = return a
efFromRight f (Left b) = throwError $ f b

errFromRight :: (MonadError e m) => e -> Either b a -> m a
errFromRight _ (Right a) = return a
errFromRight err (Left _) = throwError err

changeError :: (Error e, Error f, Monad m) =>
                   (e -> f) -> ErrorT e m a -> ErrorT f m a
changeError f a = ErrorT $ changeError' f a

changeError' :: (Error e, Error f, Monad m) =>
                    (e -> f) -> ErrorT e m a -> m (Either f a)
changeError' func action = do
    result <- runErrorT action 
    case result of
        Right x -> return $ Right x
        Left e -> return . Left $ func e

errorTToIO :: (Error e, Show e) => ErrorT e IO a -> IO ()
errorTToIO errorT = do
    result <- runErrorT errorT
    case result of
      Left err -> putStrLn $ "Error: " ++ show err
      Right _ -> return ()

throwErrorT :: (Error e, Show e) => ErrorT e IO a -> IO a
throwErrorT errorT = do
    result <- runErrorT errorT
    case result of
      Left err -> return . error $ "Error: " ++ show err
      Right r -> return r
