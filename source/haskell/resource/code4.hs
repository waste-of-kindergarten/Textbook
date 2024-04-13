{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
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

newtype Fig = Fig SolidFigure deriving newtype (Judgeable)

type SynFig = Fig 

instance Judgeable' SynFig 

deriving instance Eq Figure''
deriving instance Eq SolidFigure
deriving instance Eq SolidFigure'

class Same a b where
  same :: a -> b -> Bool 
  same _ _ = False

instance Same Figure'' SolidFigure  

instance Same SolidFigure Figure'' 

instance Same SolidFigure SolidFigure where 
  same = (==) 

instance Same Figure'' Figure'' where 
  same = (==)

class DefaultVal where 
  unit :: Double
  unit = 1
  info :: String 
  info = "global information"
  needinstantiation :: ()

instance DefaultVal where
  needinstantiation = ()

-- ordinary instances
instance Eq a => Same (Maybe a) (Maybe a) where 
  same Nothing Nothing = True 
  same (Just a) (Just b) = a == b 
  same _ _ = False 

-- flexible instances
instance Same [Char] [Char] where 
  same [] [] = True 
  same (x:xs) (y:ys) = x == y && same xs ys 
  same _ _ = False

class WithoutDep a b c where 
  func :: a -> b -> c 

instance WithoutDep Int Int Int where 
  func = \ x y -> 0

instance WithoutDep Int Int Double where
  func = \ x y -> 0

class WithDep a b c | a b -> c where 
  func' :: a -> b -> c

instance WithDep Int Int Int where 
  func' = \ x y -> 0

{-
-- illegal because a definition has already existed.
instance WithDep Int Int Double where 
  func' = \ x y -> 0
-}

