{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
import Control.Applicative (Applicative)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeHead' :: [a] -> Either String a
safeHead' [] = Left "Cannot cope with emptyList"
safeHead' (x:_) = Right x

newtype Except e a = Except (Either e a)
    deriving (Functor,Applicative,Monad,Show)

safeTail :: [a] -> Except String [a]
safeTail [] = Except $ Left "Cannot extract a tail from an empty list"
safeTail (_:xs) = return xs

safeDrop3 :: [a] -> Except String [a]
safeDrop3 xs = do
  xsdrop1 <- safeTail xs
  xsdrop2 <- safeTail xsdrop1
  safeTail xsdrop2

capture :: Except e a -> (e -> Except e a) -> Except e a 
capture ex@(Except e) = case e of 
  Right t ->  const ex 
  Left e' -> \f -> f e' 

safeDrop3OrRev :: [a] -> Except String [a]
safeDrop3OrRev xs = capture (safeDrop3 xs) (\_ -> return $ reverse xs)