module Transformer where

import Monad

{-
 -MaybeT monad transformer
 -}
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
    --fmap f m = MaybeT (fmap (fmap f) (runMaybeT m))
    fmap f = mapMaybeT (fmap (fmap f))
        where mapMaybeT f = MaybeT . f . runMaybeT

--instance (Applicative m, Monad m) => Applicative (MaybeT m) where
    ----pure m = MaybeT $ do
        ----return . Just $ m
    ----pure = MaybeT . return . return
    ----pure = MaybeT . return . Just
    ----mf <*> m = MaybeT $ do
        ----maybe_f <- runMaybeT mf
        ----maybe_v <- runMaybeT m
        ----return $ maybe_f <*> maybe_v
    --pure = MaybeT . return . Just
    --mf <*> m = MaybeT $ do
        --maybe_f <- runMaybeT mf
        --case maybe_f of
            --Nothing -> return Nothing
            --Just f -> do
                --maybe_v <- runMaybeT m
                --case maybe_v of
                    --Nothing -> return Nothing
                    --Just v -> return . Just . f $ v

handleFunc :: Applicative f => f (Maybe (a -> b)) -> f (Maybe a) -> f (Maybe b)
handleFunc mf m = Just <$> (
        (pure (\(Just f) -> f) <*> mf)
        <*>
        (pure (\(Just v) -> v) <*> m)
    )

instance (Applicative m) => Applicative (MaybeT m) where
    pure = MaybeT . pure . Just
    mf <*> m = MaybeT $ handleFunc (runMaybeT mf) (runMaybeT m)

instance Monad m => Monad (MaybeT m) where
    return = pure
    m >>= f = MaybeT $ do
        maybe_v <- runMaybeT m
        case maybe_v of
            Nothing -> return Nothing
            Just v -> runMaybeT . f $ v
    fail _ = MaybeT (return Nothing)

instance MonadTrans MaybeT where
    --lift m = MaybeT $ do
        --v <- m
        --return . Just $ v
    lift m = MaybeT $ liftM Just m

{-
 -identity monad transformer
 -}
newtype Identity a = Identity { runIdentity :: a } deriving Show

instance Functor Identity where
    --fmap f m = do
        --v <- m
        --return $ f v
    fmap f = Identity . f . runIdentity

instance Applicative Identity where
    pure = Identity
    --mf <*> m = do
        --f <- mf
        --v <- m
        --return $ f v
    mf <*> m = Identity $ (runIdentity mf) (runIdentity m)
    --

instance Monad Identity where
    return = Identity
    m >>= f = f . runIdentity $ m

newtype IdentityT m a = IdentityT { runIdentityT :: m (Identity a) }

instance Functor m => Functor (IdentityT m) where
    fmap f m = IdentityT $ (fmap (fmap f)) (runIdentityT m)

instance (Functor m, Monad m) => Applicative (IdentityT m) where
    pure = IdentityT . return . Identity
    mf <*> m = IdentityT $ do
        f <- runIdentityT mf
        v <- runIdentityT m
        return $ mapIdentity f v
        where mapIdentity (Identity f) (Identity v) = Identity $ f v

instance (Monad m) => Monad (IdentityT m) where
    return = IdentityT . return . Identity
    m >>= f = IdentityT $ do
        v <- runIdentityT m
        mapIdentity f v
        where mapIdentity f (Identity m) = runIdentityT $ f m

-- reader
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
    fmap f m = ReaderT (\r -> fmap f $ runReaderT m r)

instance Applicative m => Applicative (ReaderT r m) where
    pure m = ReaderT $ \_ -> pure m
    mf <*> m = ReaderT $ \r ->
        runReaderT mf r <*> runReaderT m r

-- two ReaderT should recive same type param
-- x type should equal to f param type
--
instance Monad m => Monad (ReaderT r m) where
    return x = ReaderT $ \_ -> return x
    m >>= f = ReaderT $ \r -> runReaderT m r
            >>= \x -> runReaderT (f x) r
    --m >>= f = ReaderT $ \r -> do
       --x <- runReaderT m r 
       --runReaderT (f x) r


type Reader r = ReaderT r Identity
-- state
-- writer

