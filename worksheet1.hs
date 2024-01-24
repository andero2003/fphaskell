import Data.Semigroup (diff)

-- 1

timesTen :: Int -> Int
timesTen x = 10 * x

-- 2

sumThree :: Int -> Int -> Int -> Int
sumThree x y z = x + y + z

-- 3

areaOfCircle :: Float -> Float
areaOfCircle r = pi * r ^ 2

-- 4

volumeOfCylinder :: Float -> Float -> Float
volumeOfCylinder h r = pi * areaOfCircle r

-- 5

diffSquared :: Float -> Float -> Float
diffSquared a b = (a - b) ^ 2

distance :: Float -> Float -> Float -> Float -> Float
distance x1 y1 x2 y2 = sqrt (diffSquared y1 y2 + diffSquared x1 x2)

-- 6

threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent a b c = a /= b && a /= c && b /= c

-- 7

divisibleBy :: Int -> Int -> Bool
divisibleBy a b = mod a b == 0

-- 8

isEven :: Int -> Bool
isEven a = divisibleBy a 2

-- 9

averageThree :: Int -> Int -> Int -> Float
averageThree a b c = fromIntegral (sumThree a b c) / 3

-- 10

absolute :: Int -> Int
absolute a = if a >= 0 then a else -a
