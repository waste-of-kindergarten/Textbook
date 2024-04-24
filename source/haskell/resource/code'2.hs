{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
import Data.Char (isAlpha)
string1 = Just "My name is "

string2 = Just "L fried."
-- just joking (Alfred)
string3 = Just "The textbook seems a mess 2 readers !"

maybeMonadDemo :: Maybe Int
maybeMonadDemo =
    string1 >>= \x ->
        (x ++) <$> string2 >>=
            \y ->
                (y ++) <$> string3 >>=
                    \z ->
                        Just ( foldr (+) 0 (((\x -> if x then 0 else 1) . isAlpha) <$> z))

maybeMonadDemo' :: Maybe Int
maybeMonadDemo' =
    string1 >>= \x ->
        string2 >>= (
            \y ->
                string3 >>= (
                    \z ->
                        Just ( foldr (+) 0 (((\x -> if x then 0 else 1) . isAlpha) <$> z))) . (y ++)) . (x ++)

maybeMonadDoDemo :: Maybe Int
maybeMonadDoDemo =
    do
        x <- string1
        y <- string2
        z <- string3
        return (foldr (+) 0 (((\x -> if x then 0 else 1) . isAlpha) <$> x ++ y ++ z))

newtype State s a = State {runState :: s -> (a, s)} deriving (Functor)

instance Applicative (State s) where
    pure x = State (x,)
    State f <*> State x = State $ \s -> (fst (f s) (fst (x s)),snd (x s))

instance Monad (State s) where 
    State x >>= f = State $ \s -> let (v,s') = x s in runState (f v) s'

class Monad m => MonadState s m | m -> s where 
    get :: m s
    get = state (\s -> (s,s)) 
    put :: s -> m ()
    put s = state (\_ -> ((),s))
    state :: (s -> (a, s)) -> m a
    state f = do
        s <- get 
        let (a, s') = f s 
        put s'
        return a
    state' :: (s -> (a,s)) -> m a
    state' f = get >>= (\s -> let (a,s') = f s in 
        put s' >>= \_ -> return a)
    {-# MINIMAL state | get, put #-}

instance Num s => MonadState s (State s) where 
    get = State $ \s -> (s,s)
    put s = State $ \_ -> ((),s + 1)

