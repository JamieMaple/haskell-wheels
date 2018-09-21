module Transformer where

import Monad

import Control.Applicative

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

mapIdentity :: (a -> a) -> Identity a -> Identity a
mapIdentity f = Identity . f . runIdentity

instance Num a => Num (Identity a) where
    (Identity a) * (Identity b) = Identity $ a * b
    (Identity a) + (Identity b) = Identity $ a + b
    (Identity a) - (Identity b) = Identity $ a - b
    abs = mapIdentity abs
    signum = mapIdentity signum
    negate = mapIdentity negate
    fromInteger = Identity . fromInteger

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

instance MonadTrans IdentityT where
    lift = IdentityT . liftM Identity

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
       --

instance MonadTrans (ReaderT r) where
    lift m = ReaderT $ \r -> m

type Reader r = ReaderT r Identity

runReader :: Reader r a -> (r -> a)
runReader m = runIdentity . runReaderT m

ask :: Monad m => ReaderT r m r
ask = ReaderT return

local :: Monad m => (r -> r') -> ReaderT r' m a -> ReaderT r m a
--local f m = ReaderT $ runReaderT m . f
local f m = ReaderT $ \r -> runReaderT m $ f r

reader :: (r -> a) -> Reader r a
reader f = ReaderT $ \r -> return $ f r -- return == Identity

-- state

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where
    fmap f m = StateT $ \ s -> 
        fmap (\(a, s') -> (f a, s')) $ runStateT m s

instance (Functor m, Monad m) => Applicative (StateT s m) where
    pure a = StateT $ \s -> pure (a,s)
    mf <*> m = StateT $ \s -> do
        (f,s') <- runStateT mf s
        (v,s'') <- runStateT m s'
        return (f v, s'')

instance Monad m => Monad (StateT s m) where
    return a = StateT $ \s -> return (a, s)
    m >>= f = StateT $ \s -> do
        (a,s') <- runStateT m s
        runStateT (f a) s'
        --(b,s'') <- runStateT (f a) s'
        --return (b, s'')
        --

instance MonadTrans (StateT s) where
    lift m = StateT $ \s -> do
        v <- m
        return (v, s)

type State s = StateT s Identity

runState :: State s a -> (s -> (a,s))
--runState m = \s -> runIdentity $ runStateT m s
runState m = runIdentity . runStateT m

state :: (s -> (a,s)) -> StateT s Identity a
state f = StateT $ \s -> Identity $ f s -- Identity == return

get :: Monad m => StateT s m s
get = StateT $ \s -> return (s,s)

-- writer
newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }

instance Functor m => Functor (WriterT w m) where
    fmap f m = WriterT $ fmap mapWriter (runWriterT m)
        where mapWriter = (\(a,w) -> (f a, w))

instance (Applicative m, Monoid w) => Applicative (WriterT w m) where
    pure a = WriterT $ pure (a, mempty)
    --mf <*> m = WriterT $ do
        --(f, w) <- runWriterT mf
        --(v, w') <- runWriterT m
        --return (f v, w `mappend` w')
    mf <*> m = WriterT $ liftA2 g (runWriterT mf) (runWriterT m)
        where g (f,w) (a,w') = (f a, w `mappend` w')

instance (Monad m, Monoid w) => Monad (WriterT w m) where
    return a = WriterT $ return (a, mempty)
    m >>= f = WriterT $ do
        (a, w) <- runWriterT m
        (a', _) <- runWriterT $ f a
        return (a', w)

instance Monoid w => MonadTrans (WriterT w) where
    lift m = WriterT $ liftM (\a -> (a, mempty)) m

