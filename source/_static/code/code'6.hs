{-# LANGUAGE DeriveGeneric #-}
import GHC.Generics (Generic)

data Tree a = 
    Leaf a 
    | Node (Tree a) (Tree a) 
    deriving (Generic)