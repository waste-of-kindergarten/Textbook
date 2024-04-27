{-# LANGUAGE BangPatterns #-}

length' :: [a] -> Int 
length' [] = 0
length' (x:xs) = 1 + length' xs

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs) = a == x || elem' a xs

infixr 5 |++|
(|++|) :: [a] -> [a] -> [a]
[] |++| ys = ys 
(x:xs) |++| ys = x : xs |++| ys 

-- reverse
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

reverse'' :: [a] -> [a]
reverse'' l = helper l []
    where
        helper [] ys = ys
        helper (x:xs) ys = helper xs (x:ys)

factorial :: Int -> Int 
factorial n = if n == 0 then 1 else n * factorial (n - 1)

factorial' :: Int -> Int 
factorial' n = if n == 0 then 1 else helper n 1 
    where 
        helper 1 m = m 
        helper n !m = helper (n - 1) (n * m)

          
factorial'' :: Int -> Int
factorial'' n = if n == 0 then 1 else helper n 1
    where 
        helper 1 m = m
        helper n m = helper (n - 1) $! (n * m) 

even' :: Int -> Bool
even' 0 = True
even' n = odd' (n - 1)

odd' :: Int -> Bool
odd' 0 = True 
odd' n = even' (n - 1)


fix :: (a -> a) -> a
fix f = let x = f x in x

-- 其他等价版本
fix' :: (a -> a) -> a
fix' f = let x = fix' f in f x

fix'' :: (a -> a) -> a
fix'' f = f (fix'' f)

-- curry
curry' :: ((a,b) -> c) -> a -> b -> c
curry' f a b = f (a,b)
-- uncurry
uncurry' :: (a -> b -> c) -> (a,b) -> c
uncurry' f (a,b) = f a b

-- map
map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs 

-- filter
filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (x:xs) = if f x 
    then x : filter' f xs 
    else filter' f xs