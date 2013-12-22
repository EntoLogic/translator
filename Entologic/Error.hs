
module Entologic.Error where

import Control.Monad.Error.Class

eFromJust :: (Error e, MonadError e m) => Maybe a -> m a
eFromJust (Just a) = return a
eFromJust Nothing = throwError noMsg

errFromJust :: (MonadError e m) => e -> Maybe a -> m a
errFromJust _ (Just a) = return a
errFromJust err Nothing = throwError err

eFromRight :: (MonadError e m) => Either e a -> m a
eFromRight (Right a) = return a
eFromRight (Left e) = throwError e

efFromRight :: (MonadError e m) => (b -> e) -> Either b a -> m a
efFromRight _ (Right a) = return a
efFromRight f (Left b) = throwError $ f b

errFromRight :: (MonadError e m) => e -> Either b a -> m a
errFromRight _ (Right a) = return a
errFromRight err (Left _) = throwError err
