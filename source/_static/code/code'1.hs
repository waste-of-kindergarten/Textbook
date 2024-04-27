{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Control.Applicative

data Tree a =
  Empty
  | Node a (Tree a) (Tree a) deriving (Show)

instance Functor Tree where
  fmap f Empty = Empty
  fmap f (Node a lchild rchild) = Node (f a) (fmap f lchild) (fmap f rchild)

newtype L a = L [a]
  deriving newtype (Show,Functor,Semigroup,Monoid)

instance Applicative L where
  pure x = L (repeat x)
  (L []) <*> _ = mempty
  _ <*> (L []) = mempty
  (L (f:fs)) <*> (L (x:xs)) = L [f x] <> (L fs <*> L xs)

instance Applicative Tree where
  pure x = let tree = Node x tree tree
            in tree
  Empty <*> _ = Empty
  _ <*> Empty = Empty
  Node f flchild frchild <*> Node x lchild rchild = Node (f x) (flchild <*> lchild) (frchild <*> rchild)

return' :: Monad m => a -> m a
return' = pure 

(|>>) :: Monad m => m a -> m b -> m b 
ma |>> mb = ma >>= const mb 


