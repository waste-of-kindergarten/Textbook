{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE PolyKinds #-}
data Nat = Zero | Succ Nat 

data ListN a n where 
    Empty :: ListN a Zero 
    Cons :: a -> ListN a n -> ListN a (Succ n)   

rank2demo :: (forall a . a -> a) -> b -> b 
rank2demo f = f 

data PolyKindDemo :: forall k . k -> * 

type D1 = PolyKindDemo Int

type D2 = PolyKindDemo String 


