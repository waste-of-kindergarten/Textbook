{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveAnyClass #-}


-- 圆形直径
type Diameter = Double
-- 长方形长宽
type Length = Double
type Width = Double
-- 三角形三条边长
type Side1 = Double
type Side2 = Double
type Side3 = Double

data Figure'' =
  Circle'' { getDiameter :: Diameter}
  | Rectangle'' { getLength :: Length, getWidth :: Width}
  | Triangle'' {
    getSide1 :: Side1,
    getSide2 :: Side2,
    getSide3 :: Side3
  } deriving (Show)

type Height = Double

data SolidFigure = 
    Sphere {getDiameter :: Diameter}
    | Cuboid {getLength :: Length, getWidth :: Width, getHeight :: Height}
    | Cylinder {getDiameter :: Diameter, getHeight :: Height}
  deriving (Show)

class Judgeable a where 
    judgeType :: a -> String
    tellShape :: a -> String

instance Judgeable Figure'' where  
    judgeType :: Figure'' -> String
    judgeType _ = "A Figure"
    tellShape :: Figure'' -> String
    tellShape (Circle'' _) = "A Circle"
    tellShape (Rectangle'' {}) = "A Rectangle"
    tellShape (Triangle'' {}) = "A Triangle"

instance Judgeable SolidFigure where 
    judgeType :: SolidFigure -> String
    judgeType _ = "A SolidFigure"
    tellShape :: SolidFigure -> String
    tellShape (Sphere _) = "A Sphere"
    tellShape (Cuboid {}) = "A Cuboid"
    tellShape (Cylinder {}) = "A Cylinder" 

class Judgeable' a where 
  judgeType' :: a -> String 
  judgeType' x = "A Default Judgeable Type"
  tellShape' :: a -> String 
  tellShape' x = "A Default Judgeable Shape"

instance Judgeable' SolidFigure

data SolidFigure' = 
  Sphere' {getDiameter :: Diameter}
    | Cuboid' {getLength :: Length, getWidth :: Width, getHeight :: Height}
    | Cylinder' {getDiameter :: Diameter, getHeight :: Height}
  deriving (Show,Judgeable')

