{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Dynamic



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


class Judgeable a => Dimension a where
  dim :: a -> Int

instance Dimension Figure'' where
  dim = const 2

instance Dimension SolidFigure where
  dim = const 3

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


class Empty a where
  isEmpty :: a -> Bool

-- ordinary instance 
instance Empty (Maybe a) where
  isEmpty Nothing = True
  isEmpty (Just _) = False


-- flexible instance
instance Empty [Char] where
  isEmpty [] = True
  isEmpty (x:xs) = False

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

class Assoc a b where
  type FuncType a b :: *
  assocfunc :: a -> b -> FuncType a b

instance Assoc Int Int where
  type FuncType Int Int = Int
  assocfunc a b = 0

-- code4.hs
data HeteData =
  HeteInt Int
  | HeteChar Char
  | HeteBool Bool

  -- code4.hs
a :: [HeteData]
a = [HeteChar 'a',HeteInt 1,HeteBool True]

b :: [Dynamic]
b = [toDyn 'a',toDyn (1 :: Int),toDyn True]

geta :: Char
geta = case (fromDynamic . head) b of
  Nothing -> error "Type mismatch"
  Just x -> x

data ExistData = forall a. (Show a) => ExistHeteData a

c :: [ExistData]
c = [ExistHeteData 'a',ExistHeteData (1 :: Int),ExistHeteData True]

showc :: String
showc = show c'
  where f (ExistHeteData t) = show t
        c' = map f c

newtype BoolA = BoolA Bool 

newtype BoolO = BoolO Bool 

instance Semigroup BoolA where 
  BoolA a <> BoolA b = BoolA (a && b)


instance Semigroup BoolA => Monoid BoolA where 
  mempty = BoolA True 
  
instance Semigroup BoolO where 
  BoolO a <> BoolO b = BoolO (a || b)

instance Semigroup BoolO => Monoid BoolO where 
  mempty = BoolO False





  