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
