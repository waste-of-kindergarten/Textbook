-- code1.hs

data Figure =
    Circle Double
  | Rectangle Double Double
  | Triangle Double Double Double 
  deriving (Show)

-- 圆形直径
type Diameter = Double
-- 长方形长宽
type Length = Double
type Width = Double
-- 三角形三条边长
type Side1 = Double
type Side2 = Double
type Side3 = Double

data Figure' = 
  Circle' Diameter
  | Rectangle' Length Width
  | Triangle' Side1 Side2 Side3
  deriving (Show)

data Figure'' = 
  Circle'' { getDiameter :: Diameter}
  | Rectangle'' { getLength :: Length, getWidth :: Width}
  | Triangle'' {
    getSide1 :: Side1,
    getSide2 :: Side2,
    getSide3 :: Side3
  } deriving (Show)

newtype NewInt = NewInt Int 
newtype NewInt' = NewInt' { getInt :: Int }

data Nat = Zero | Succ Nat deriving (Show)

add :: (Int, Int) -> Int
add (a,b) = a + b 

add' :: Int -> Int -> Int 
add' a b = a + b

add'' :: Int -> Int -> Int
add'' = \ x y -> x + y

addone' :: Int -> Int 
addone' = (\x y -> x + y) 1
-- 等同于
addone'' :: Int -> Int 
addone'' = \y -> 1 + y

(|+|) :: Int -> Int -> Int 
(|+|) x y = x + y

infixl 6 |+|

(|*|) :: Int -> Int -> Int 
x |*| y = x * y

infixl 7 |*|

addone''' :: Int -> Int 
addone''' = (1+)

divide :: Int -> Int -> Maybe Int 
divide a b = if b == 0 then  Nothing else Just (a `div` b) 