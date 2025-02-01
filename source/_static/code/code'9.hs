{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Redundant lambda" #-}
import Test.QuickCheck
import Control.Monad (liftM2)


data Person = Person {
    name :: String,
    sex :: Bool,
    age :: Int
    }

instance Arbitrary Person where
    arbitrary = do
        name <- arbitrary
        sex <- arbitrary
        age <- arbitrary
        return $ Person name sex age

data Rose a = MkRose a [Rose a] deriving (Show)

instance Arbitrary a => Arbitrary (Rose a) where
    arbitrary = sized rose
        where rose = \n -> case n of
                0 -> liftM2 MkRose arbitrary (pure [])
                _ -> do
                    numtrees <- choose (0, max 0 (n - 1))
                    if numtrees == 0
                        then liftM2 MkRose arbitrary (pure [])
                        else
                            liftM2 MkRose arbitrary (replicate numtrees <$> rose ((n - 1) `div` numtrees))

arbitrary' :: Arbitrary a => Gen (Rose a)
arbitrary' = oneof [
    liftM2 MkRose arbitrary (pure []),
    liftM2 MkRose arbitrary (liftM2 replicate arbitrary arbitrary') ]

