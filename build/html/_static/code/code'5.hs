{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}


data Nat = Zero | Succ Nat 

data ListN a (n :: Nat) where 
    Empty :: ListN a Zero 
    Cons :: a -> ListN a n -> ListN a (Succ n)   

deriving instance Show a => Show (ListN a n)

data NumListN a (n :: Nat) where 
    NumEmpty :: Num a => NumListN a Zero 
    NumCons :: Num a => a -> NumListN a n -> NumListN a (Succ n)

data ListN' a (n :: Nat) where 
    Empty' :: ListN' a Zero
    Cons' :: {head' :: a , tail' :: ListN' a n} -> ListN' a (Succ n) 

data HeteNumListN (n :: Nat) where 
    HeteNumEmpty :: HeteNumListN Zero 
    HeteNumCons :: Num a => a -> HeteNumListN n -> HeteNumListN (Succ n)

data Nat' where 
    Zero' :: Nat' 
    Succ' :: Nat' -> Nat' 
