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

-- 等效于
maybeMonadDoDemo' :: Maybe Int
maybeMonadDoDemo' =
    do {    x <- string1;
            y <- string2;
            z <- string3;
            return (foldr (+) 0 (((\x -> if x then 0 else 1) . isAlpha) <$> x ++ y ++ z))
    }
newtype State s a = State {runState :: s -> (a, s)} deriving (Functor)

instance Applicative (State s) where
    pure x = State (x,)
    State f <*> State x = State $ \s -> (fst (f s) (fst (x s)),snd (x s))

instance Monad (State s) where
    State x >>= f = State $ \s -> let (v,s') = x s in runState (f v) s'

evalState :: State s a -> s -> a
evalState act = fst . runState act

execState :: State s a -> s -> s
execState act = snd . runState act

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
    {-# MINIMAL state | get, put #-}

instance MonadState s (State s) where
    get = State $ \s -> (s,s)
    put s = State $ \_ -> ((),s)

type Stack = [Int]

push :: Int -> State Stack ()
push x = state $ \xs -> ((),x : xs)

pop :: State Stack Int
pop = state $ \(x:xs) -> (x, xs)

peek :: State Stack Int
peek = state $ \(x:xs) -> (x, x:xs)

makestack :: State Stack ()
makestack = do
    push 1
    push 2

add :: State Stack ()
add = do
    a <- pop
    b <- pop
    let c = a + b
    push c

newtype Reader e a = Reader {runReader :: e -> a} deriving (Functor)

instance Applicative (Reader e) where
  pure a = Reader $ \e -> a
  Reader f <*> Reader x = Reader $ \e -> f e (x e)

instance Monad (Reader e) where
    (Reader r) >>= f = Reader $ \e -> runReader (f (r e)) e

class Monad m => MonadReader e m | m -> e where 
    ask :: m e 
    ask = reader id 
    local :: (e -> e) -> m a -> m a 
    reader :: (e -> a) -> m a 
    reader f = do 
        r <- ask 
        return (f r)
    {-# MINIMAL (ask | reader), local #-}

instance MonadReader e (Reader e) where 
    local f c = Reader $ \e -> runReader c (f e)
    ask = Reader id 

newtype Writer w a = Writer {runWriter :: (a,w) } deriving (Functor)

instance Monoid w => Applicative (Writer w) where 
    pure x = Writer (x,mempty)
    Writer (fa,w') <*> Writer (a,w) = Writer (fa a,w' `mappend` w) 

instance Monoid w => Monad (Writer w) where 
    Writer (a,w) >>= f = let (a',w') = runWriter (f a) in 
        Writer (a', w `mappend` w') 

class (Monoid w, Monad m) => MonadWriter w m | m -> w where 
    pass :: m (a, w -> w) -> m a 
    listen :: m a -> m (a,w)
    tell :: w -> m () 
    tell w = writer ((),w)
    writer :: (a,w) -> m a
    writer (a, w) = do 
            tell w 
            return a  
    {-# MINIMAL (writer | tell), listen, pass #-}

instance Monoid w => MonadWriter w (Writer w) where 
  pass (Writer ((a,f),w)) = Writer (a,f w)
  listen (Writer (a,w)) = Writer ((a,w),w)
  tell s = Writer ((),s)

censor :: (MonadWriter w m) => (w -> w) -> m a -> m a
censor f m = pass $ do 
            a <- m
            return (a, f)

listens :: (MonadWriter w m) => (w -> b) -> m a -> m (a,b)
listens f m = do 
        (a,w) <- listen m 
        return (a,f w)

newtype IdentityT m a = IdentityT {runIdentityT :: m a} deriving (Functor)

instance Applicative m => Applicative (IdentityT m) where 
  pure a = IdentityT $ pure a
  IdentityT mf <*> IdentityT ma = IdentityT (mf <*> ma)

instance Monad m => Monad (IdentityT m) where 
  m >>= k = IdentityT $ do 
            a <- runIdentityT m 
            runIdentityT (k a)

type IdentityMaybe a = IdentityT Maybe a 

string1' = IdentityT string1

string2' = IdentityT string2

string3' = IdentityT string3


identityMaybeMonadTDemo :: IdentityMaybe Int 
identityMaybeMonadTDemo = 
    do 
        x <- string1'
        y <- string2'
        z <- string3'
        return (foldr (+) 0 (((\x -> if x then 0 else 1) . isAlpha) <$> x ++ y ++ z))