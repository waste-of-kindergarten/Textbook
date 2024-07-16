{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

import Data.Typeable

(=?) :: (Typeable a,Typeable b,Eq a,Eq b) => a -> b -> Bool
x =? y = case cast x of
    Just x' -> x' == y
    Nothing -> False

data Nat = Zero | Succ Nat


data ListN a (n :: Nat) where
    Empty :: ListN a Zero
    Cons :: a -> ListN a n -> ListN a (Succ n)

