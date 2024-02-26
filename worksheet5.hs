{- Week5.hs
 This file illustrates list patterns and recursion over lists.
-}

import Prelude hiding (concat, fst, head, reverse, snd, sum, tail, zip)

-- Definitions of the prelude functions fst and snd

fst (x, _) = x

snd (_, y) = y

-- Definitions of the prelude functions head and tail

head (x : _) = x

tail (_ : xs) = xs

absFirst :: [Int] -> Int
absFirst [] = -1
absFirst (x : xs) = abs x

sum :: [Int] -> Int
sum [] = 0
sum (x : xs) = x + sum xs

doubleAll :: [Int] -> [Int]
doubleAll [] = []
doubleAll (x : xs) = 2 * x : doubleAll xs

concat :: [[a]] -> [a]
concat [] = []
concat (x : xs) = x ++ concat xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = reverse xs ++ [x]

zip :: [a] -> [b] -> [(a, b)]
zip (x : xs) (y : ys) = (x, y) : zip xs ys
zip _ _ = []

-- 1

headPlusOne :: [Int] -> Int
headPlusOne [] = -1
headPlusOne (x : xs) = x + 1

-- 2

duplicateHead :: [a] -> [a]
duplicateHead [] = []
duplicateHead (x : xs) = x : (x : xs)

-- 3

rotate :: [a] -> [a]
rotate [] = []
rotate [a] = [a]
rotate (x : (y : ys)) = y : x : ys

-- 4

listLength :: [a] -> Int
listLength [] = 0
listLength (x : xs) = 1 + listLength xs

-- 5

multAll :: [Int] -> Int
multAll [] = 1
multAll (x : xs) = x * multAll xs

-- 6

andAll :: [Bool] -> Bool
andAll [] = True
andAll (x : xs) = x && andAll xs

-- 7

orAll :: [Bool] -> Bool
orAll [] = False
orAll (x : xs) = x || orAll xs

-- 8

countIntegers :: Int -> [Int] -> Int
countIntegers t [] = 0
countIntegers t (x : xs)
  | x == t = 1 + countIntegers t xs
  | otherwise = countIntegers t xs

countIntegersShort :: Int -> [Int] -> Int
countIntegersShort t list = sum [1 | x <- list, x == t]

-- 9

removeAll :: Int -> [Int] -> [Int]
removeAll target list = [x | x <- list, x /= target]

-- 10

removeAllButFirst :: Int -> [Int] -> [Int]
removeAllButFirst target (x : xs)
  | x == target = x : removeAll target xs
  | otherwise = x : removeAllButFirst target xs

-- 11

type StudentMark = (String, Int)

testData :: [StudentMark]
testData =
  [ ("John", 53),
    ("Sam", 16),
    ("Kate", 85),
    ("Jill", 65),
    ("Bill", 37),
    ("Amy", 22),
    ("Jack", 41),
    ("Sue", 71)
  ]

listMarks :: String -> [StudentMark] -> [Int]
listMarks targetName marks = [mark | (name, mark) <- marks, name == targetName]

--12 

sorted :: [Int] -> Bool
sorted [] = True
sorted [a,b] = a <= b
sorted (x:y:ys) = x <= y && sorted (y:ys)

-- 13

prefix :: [Int] -> [Int] -> Bool
prefix [] _ = True
prefix _ [] = False
prefix (x:xs) (y:ys) = x == y && prefix xs ys

-- 14

withoutLast :: [Int] -> [Int]
withoutLast [] = []
withoutLast [a] = []
withoutLast (x:xs) = x : withoutLast xs

subSequence :: [Int] -> [Int] -> Bool
subSequence list1 (x:xs) = prefix list1 xs || prefix list1 (withoutLast (x:xs))
