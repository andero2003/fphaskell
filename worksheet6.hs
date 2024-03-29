{- Week6.hs
 This module illustrates the use of functions as values
-}

import Data.Char

twice :: (Int -> Int) -> Int -> Int
twice f x = f (f x)

multiply :: Int -> Int -> Int
multiply x y = x * y

double :: Int -> Int
double = multiply 2

doubleAll :: [Int] -> [Int]
doubleAll = map (*2)

areDigits :: String -> [Bool]
areDigits = map isDigit

keepPositive :: [Int] -> [Int]
keepPositive = filter (>0)

keepDigits :: String -> String
keepDigits = filter isDigit

addUp :: [Int] -> Int
addUp = foldr (+) 0 

myConcat :: [[a]] -> [a]
myConcat = foldr (++) []

-- 1

mult10 :: [Int] -> [Int]
mult10 = map (*10)

-- 2

onlyLowerCase :: String -> String
onlyLowerCase = filter isLower 

-- 3

orAll :: [Bool] -> Bool
orAll = foldr (||) False

-- 4

sumSquares :: [Int] -> Int
sumSquares = sum . map (^2)

-- 5

zeroToTen :: [Int] -> [Int]
zeroToTen = filter (>=0) . filter (<=10)

-- 6

squareRoots :: [Float] -> [Float]
squareRoots = map sqrt . filter (>= 0)

-- 7

countBetween :: Float -> Float -> [Float] -> Int
countBetween m n = foldr (\x acc -> if x >= m && x <= n then acc + 1 else 0) 0

-- 8

andAll :: [Bool] -> Bool
andAll = foldr (&&) True

alwaysPositive :: (Float -> Float) -> [Float] -> Bool
alwaysPositive f = andAll . map ((>0) . f) 

-- 9 

productSquareRoots :: [Float] -> Float 
productSquareRoots list = foldr ((*) . sqrt) 0 (filter (>0) list) 

-- 10

removeFirst :: (a -> Bool) -> [a] -> [a]
removeFirst f [] = []
removeFirst f (x:xs)
    | f x = xs
    | otherwise = x : removeFirst f xs

-- 11

removeLast :: (a -> Bool) -> [a] -> [a]
removeLast f list = reverse(removeFirst f (reverse list))

-- 12

zeroToTenLambda :: [Int] -> [Int]
zeroToTenLambda = filter (\x -> x >= 0 && x <= 10)

-- 13

-- a)

alwaysPositiveFoldr :: (Float -> Float) -> [Float] -> Bool
alwaysPositiveFoldr f = foldr (\x acc -> (f x > 0) && acc) True

-- b)
productSquareRootsFoldr :: [Float] -> Float
productSquareRootsFoldr = foldr (\x acc -> if x > 0 then sqrt x * acc else acc) 1

-- c)
reverseFoldr :: [a] -> [a]
reverseFoldr = foldr (\x xs -> xs ++ [x]) []