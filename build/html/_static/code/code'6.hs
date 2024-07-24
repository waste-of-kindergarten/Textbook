{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
import GHC.Generics

data Tree a = 
    Leaf a 
    | Node (Tree a) (Tree a) 
    deriving (Generic)

class Show' a where 
    show' :: a -> String 
    default show' :: (Generic a,GenericShow (Rep a)) => a -> String 
    show' x = showsDefault x ""

showsDefault :: (Generic a, GenericShow (Rep a)) => a -> ShowS 
showsDefault x = gshow False (from x)

class GenericShow a where 
    gshow :: Bool -> a x -> ShowS 

instance GenericShow V1 where 
    gshow _ _ = error "cannot show void type"

instance GenericShow U1 where 
    gshow _ U1 = id 

instance (GenericShow a, GenericShow b) => 
    GenericShow (a :+: b) where 
    gshow b (L1 l) = gshow b l 
    gshow b (R1 r) = gshow b r 

instance (GenericShow a, GenericShow b) => 
    GenericShow (a :*: b) where 
    gshow b (x :*: y) = gshow b x . showChar ' ' . gshow b y

instance (Show' a) => GenericShow (K1 i a) where 
    gshow _ (K1 a) = \x -> show' a ++ x 

instance (GenericShow a, Datatype b) => 
    GenericShow (D1 b a) where 
    gshow b (M1 a) = gshow b a 

instance (GenericShow a, Constructor g) => 
    GenericShow (C1 g a) where 
    gshow _ c@(M1 a) = showString "(" . 
                       showString (conName c) . 
                       showString " " . 
                       wrapRecord (gshow (conIsRecord c) a) . 
                       showString ")"
        where 
            wrapRecord :: ShowS -> ShowS 
            wrapRecord s 
                |   conIsRecord c = 
                            showString "{ " . s . showString " }"
                |   otherwise = s    

instance (GenericShow a, Selector g) => 
    GenericShow (S1 g a) where 
    gshow b s@(M1 a) 
        | null (selName s) = gshow b a 
        | otherwise = showString (selName s) . 
                      showString " = " . 
                      gshow b a . 
                      showChar ' '

instance Show' Char where 
    show' = show 

instance Show' Int where 
    show' = show 

instance Show a => Show' [a] where 
    show' = show 

data Book = Book {name :: String, year :: Int} deriving (Generic,Show')

data Book' = Book' String Int deriving (Generic,Show')

data Nat = Zero | Succ Nat deriving (Generic,Show')

