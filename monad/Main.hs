import Transformer

maybeFoo :: MaybeT [] Int
maybeFoo = MaybeT [Just 3, Just 1, Nothing]

type Foo = ReaderT String (MaybeT [])

innerMaybe :: a -> Foo a
innerMaybe x = lift . MaybeT $ ([Just x])

type Bar = MaybeT (ReaderT String [])

innerReader :: a -> Bar a
innerReader x = lift . ReaderT $ \r -> return x

main :: IO ()
main = putStrLn "hello"
