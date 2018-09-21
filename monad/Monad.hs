{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}   -- Enable the language extensions.

module Monad where

{-
 -monad trans
 -}
class MonadTrans t where
    lift :: (MonadTrans t, Monad m) => m a -> t m a

liftM :: Monad m => (a -> b) -> m a -> m b
--liftM f m = do
    --v <- m
    --return . f $ v
liftM f m = m >>= (\a -> return . f $ a)

--monad reader
class Monad m => MonadReader r m | m -> r  where
    ask :: m r
    local :: (r -> r) -> m a -> m a
    reader :: (r -> a) -> m a

