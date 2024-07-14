{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
data Nat = Zero | Succ Nat 

data ListN a n where 
    Empty :: ListN a Zero 
    Cons :: a -> ListN a n -> ListN a (Succ n)   

rank2demo :: (forall a . a -> a) -> b -> b 
rank2demo f = f 

data PolyKindDemo :: forall k . k -> * 

type D1 = PolyKindDemo Int

type D2 = PolyKindDemo String 

class Collects1 e ce | ce -> e where 
    empty1 :: ce
    insert1 :: e -> ce -> ce 
    member1 :: e -> ce -> Bool
    toList1 :: ce -> [e]

instance Eq e => Collects1 e [e] where 
    empty1 = []
    insert1 e l = e:l
    member1 e [] = False
    member1 e (x:xs) 
        | e == x = True 
        | otherwise = member1 e xs 
    toList1 l = l

class Collects2 ce where 
    type Elem2 ce :: *
    empty2 :: ce 
    insert2 :: Elem2 ce -> ce -> ce 
    member2 :: Elem2 ce -> ce -> Bool 
    toList2 :: ce -> [Elem2 ce]

instance Eq e => Collects2 [e] where 
    type Elem2 [e] = e 
    empty2 = []
    insert2 e l = e:l
    member2 e [] = False
    member2 e (x:xs) 
        | e == x = True 
        | otherwise = member2 e xs 
    toList2 l = l

type family Elem3 a :: *
type instance Elem3 [e] = e

class Collects3 ce where
    empty3 :: ce 
    insert3 :: Elem3 ce -> ce -> ce 
    member3 :: Elem3 ce -> ce -> Bool 
    toList3 :: ce -> [Elem3 ce]

instance Eq e => Collects3 [e] where 
    empty3 = []
    insert3 e l = e:l
    member3 e [] = False
    member3 e (x:xs) 
        | e == x = True 
        | otherwise = member3 e xs 
    toList3 l = l

class (Elem3 ce ~ e) => Collects3' e ce where
    empty3' :: ce 
    insert3' :: e -> ce -> ce 
    member3' :: e -> ce -> Bool 
    toList3' :: ce -> [e]

instance Eq e => Collects3' e [e] where 
    empty3' = []
    insert3' e l = e:l
    member3' e [] = False
    member3' e (x:xs) 
        | e == x = True 
        | otherwise = member3' e xs 
    toList3' l = l

type family (a :: Nat) + (b :: Nat) :: Nat 
type instance Zero + m = m 
type instance (Succ n) + m = Succ (n + m)

(++:) :: ListN a n -> ListN a m -> ListN a (n + m)
Empty ++: l = l 
Cons t l1 ++: l = Cons t (l1 ++: l)

instance (Show a) => Show (ListN a n) where 
    show Empty = ""
    show (Cons t l) = show t ++ " " ++ show l