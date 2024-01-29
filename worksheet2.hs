-- 1

absolute :: Int -> Int
absolute x
    | x < 0 = -x
    | otherwise = x

-- 2

sign :: Int -> Int
sign x
    | x == 0 = 0
    | x < 0 = -1
    | otherwise = 1

-- 3

howManyEqual :: Int -> Int -> Int -> Int
howManyEqual x y z
    | x == y && y == z = 3
    | x == y || y == z || x == z = 2
    | otherwise = 0

-- 4 

sumDiagonalLengths :: Float -> Float -> Float -> Float
sumDiagonalLengths x y z = diagonalLength x + diagonalLength y + diagonalLength z
    where diagonalLength x = x * sqrt 2

-- 5

taxiFare :: Int -> Float
taxiFare d
    | d <= 10 = 2.20 + fromIntegral d * 0.5
    | otherwise = 2.20 + 10*0.5 + (fromIntegral d-10)*0.3

-- 6

isAboveAverage :: Int -> Float -> Int
isAboveAverage a average
    | fromIntegral a > average = 1
    | otherwise = 0

howManyAboveAverage :: Int -> Int -> Int -> Int
howManyAboveAverage a b c = isAboveAverage a average + isAboveAverage b average + isAboveAverage c average
    where average = (fromIntegral a + fromIntegral b + fromIntegral c)/3

-- 7

getDaysInMonth :: Int -> Int
getDaysInMonth mm
    | mm == 2 = 28
    | (odd mm && mm < 8) || (even mm && mm >= 8) = 31
    | otherwise = 30

validDate :: Int -> Int -> Bool
validDate dd mm
    | mm < 1 || mm > 12 = False -- Invalid month
    | otherwise = dd > 0 && dd < getDaysInMonth mm + 1

-- 8

daysInMonth :: Int -> Int -> Int
daysInMonth mm yyyy
    | yyyy `mod` 4 == 0 && mm == 2 = getDaysInMonth mm + 1
    | otherwise = getDaysInMonth mm

-- Written Exercises

--1
{-
    sumThree 3 5 7
    = 3 + 5 + 7
    = 8 + 7
    = 15    

    sumThree 8 (1+3) 2
    = sumThree 8 4 2
    = 8 + 4 + 2
    = 12 + 2
    = 14
-}

--2 
{-
    threeDifferent 1 4 2
    = 1 /= 4 && 1 /= 2 && 4 /= 2
    = True && True && True
    = True && True
    = True

    threeDifferent 1 7 7
    = 1 /= 7 && 1 /= 7 && 7 /= 7
    = True && True && False
    = True && False
    = False
-}

--3 
{-
    | x == y && y == z = 3
    | x == y || y == z || x == z = 2
    | otherwise = 0

-}
