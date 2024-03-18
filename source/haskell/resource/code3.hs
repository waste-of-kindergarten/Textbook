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