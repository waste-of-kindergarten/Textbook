{-# HLINT ignore "Redundant if" #-}
{-# LANGUAGE MultiWayIf #-}



import Debug.Trace ( trace )

heron's_formula :: Double -> Double -> Double -> Double
heron's_formula a b c = let p = (a + b + c)/2
                            in sqrt (p * (p - a) * (p - b) * (p - c))

heron's_formula' :: Double -> Double -> Double -> Double
heron's_formula' a b c = sqrt (p * (p - a) * (p - b) * (p - c))
    where p = (a + b + c)/2

divide :: Int -> Int -> Maybe Int
divide a b = if b == 0 then Nothing else Just (a `div` b)

divide' :: Int -> Int -> Maybe Int
divide' a b = if b == 0 then Nothing
    else Just (a `div` b)

divide'' :: Int -> Int -> Maybe Int
divide'' a b = if b == 0
    then Nothing
    else Just (a `div` b)

bothzero :: Int -> Int -> Bool
bothzero a b =
    if a == 0 then
        if b == 0 then True
                  else False
              else False

bothzero' :: Int -> Int -> Bool
bothzero' a b =
    if a == 0
        then if b == 0
            then True
            else False
        else False

compareToInt :: Int -> Int -> Int
compareToInt a b = if a > b then 1
    else if a == b then 0
    else -1

compareToInt' :: Int -> Int -> Int
compareToInt' a b = if a > b
    then 1
    else if a == b
        then 0
        else -1

and' :: Bool -> Bool -> Bool
and' a b
    | a == True = b
    | otherwise = False


and'' :: Bool -> Bool -> Bool
and'' a b
    | a = trace "first condition" b
    | b = trace "second condition" a
    | otherwise = trace "otherwise condition" False


bothzero'' :: Int -> Int -> Bool
bothzero'' a b =
    if | a == 0 ->
        if | b == 0 -> True
           | otherwise -> False
       | otherwise -> False

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

judgeShape :: Figure'' -> String
judgeShape (Circle'' diameter) = "This is a Circle"
judgeShape (Rectangle'' length width) = "This is a Rectangle"
judgeShape (Triangle'' side1 side2 side3) = "This is a Triangle"


judgeShape' :: Figure'' -> String
judgeShape' (Circle'' _) = "This is a Circle"
judgeShape' (Rectangle'' _ _) = "This is a Rectangle"
judgeShape' (Triangle'' _ _ _) = "This is a Triangle"

judgeShape'' :: Figure'' -> String
judgeShape'' (Circle'' _) = "This is a Circle"
judgeShape'' (Rectangle'' {}) = "This is a Rectangle"
judgeShape'' (Triangle'' {}) = "This is a Triangle"

judgeShape''' :: Figure'' -> String
judgeShape''' x =
    case x of
        Circle'' _ -> "This is a Circle"
        Rectangle'' {} -> "This is a Rectangle"
        Triangle'' {} -> "This is a Triangle"

filterCircle :: [Figure''] -> [Figure'']
filterCircle ls =
    case ls of
        [] -> []
        (x@(Circle'' _):xs) -> x : filterCircle xs
    

judgeShape_4 :: Figure'' -> String 
judgeShape_4 x 
    | Circle'' r <- x, r > 0 = "This is a Circle"
    | Rectangle'' l w <- x, l > 0, w > 0 = "This is a Rectangle"
    | Triangle'' s1 s2 s3 <- x, s1 > 0, s2 > 0, s3 > 0, 
        s1 + s2 > s3, s1 + s3 > s2, s2 + s3 > s3 = "This is A Triangle" 
    | otherwise = "Invalid Figure"

judgeShape_5 :: Figure'' -> String
judgeShape_5 x =
  case x of 
      Circle'' r | r > 0 -> "This is a Circle"
      Rectangle'' l w | l > 0, w > 0 -> "This is a Rectangle"
      Triangle'' s1 s2 s3 | s1 > 0 , s2 > 0 , s3 > 0 ,
              s1 + s2 > s3, s1 + s3 > s2, s2 + s3 > s3 -> "This is A Triangle" 
      _ -> "Invalid Figure"

judge2Shapes :: Figure'' -> Figure'' -> String
judge2Shapes x y 
    | Circle'' rx <- x, Circle'' ry <- y, 
            rx > 0, 
            ry > 0  = "Two Circles"
    | Circle'' rx <- x, Rectangle'' ly wy <- y, 
            rx > 0, 
            ly > 0 && wy > 0 ="A Circle and a Rectangle"
    | Rectangle'' lx wx <- x, Circle'' ry <- y, 
            lx > 0 && wx > 0, 
            ry > 0 = "A Circle and a Rectangle"
    | Circle'' rx <- x, Triangle'' s1y s2y s3y <- y, 
            rx > 0, 
            s1y > 0 && s2y > 0 && s3y > 0,
            s1y + s2y > s3y && s2y + s3y > s1y && s1y + s3y > s2y = "A Circle and a Triangle"
    | Triangle'' s1x s2x s3x <- x, Circle'' ry <- y,
            s1x > 0 && s2x > 0 && s3x > 0,
            s1x + s2x > s3x && s2x + s3x > s1x && s1x + s3x > s2x,
            ry > 0 = "A Circle and a Triangle"
    | Rectangle'' lx wx <- x, Rectangle'' ly wy <- y,
            lx > 0 && wx > 0,
            ly > 0 && wy > 0 = "Two Rectangles"
    | Rectangle'' lx wx <- x, Triangle'' s1y s2y s3y <- y,
            lx > 0 && wx > 0,
            s1y > 0 && s2y > 0 && s3y > 0,
            s1y + s2y > s3y && s2y + s3y > s1y && s1y + s3y > s2y = "A Rectangle and a Triangle"
    | Triangle'' s1x s2x s3x <- x, Rectangle'' ly wy <- y,
            s1x > 0 && s2x > 0 && s3x > 0,
            s1x + s2x > s3x && s2x + s3x > s1x && s1x + s3x > s2x,
            ly > 0 && wy > 0 = "A Rectangle and A Triangle"
    | Triangle'' s1x s2x s3x <- x, Triangle'' s1y s2y s3y <- y,
            s1x > 0 && s2x > 0 && s3x > 0,
            s1x + s2x > s3x && s2x + s3x > s1x && s1x + s3x > s2x,
            s1y > 0 && s2y > 0 && s3y > 0,
            s1y + s2y > s3y && s2y + s3y > s1y && s1y + s3y > s2y = "Two Triangles"
    | otherwise = "Invalid Figures"


