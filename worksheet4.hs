import Control.Arrow (Arrow(first))
-- 1

sumDifference :: Int -> Int -> (Int, Int)
sumDifference a b = (a+b, a-b)

-- 2

type StudentMark = (String, Int)

grade :: StudentMark -> Char
grade (_, mark)
    | mark >= 70 && mark <= 100 = 'A'
    | mark >= 60 = 'B'
    | mark >= 50 = 'C'
    | mark >= 40 = 'D'
    | mark >= 0 = 'F'
    | otherwise = error "Not a letter"

-- 3

capMark :: StudentMark -> StudentMark
capMark (name, mark) = (name, min mark 40)

-- 4

firstNumbers :: Int -> [Int]
firstNumbers n = if n <= 0 then [] else [1 .. n]

-- 5

firstSquares :: Int -> [Int]
firstSquares n = [x * x | x <- firstNumbers n]

-- 6
