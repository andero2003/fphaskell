import Data.Char

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
firstNumbers n = [1 .. n]

-- 5

firstSquares :: Int -> [Int]
firstSquares n = [x ^ 2 | x <- firstNumbers n]

-- 6

capitalise :: String -> String
capitalise str = [toUpper char | char <- str]

-- 7

onlyDigits :: String -> String
onlyDigits str = [char | char <- str, isDigit char]

-- 8

capMarks :: [StudentMark] -> [StudentMark]
capMarks marks = [capMark mark | mark <- marks]

-- 9

gradeStudents :: [StudentMark] -> [(String, Char)]
gradeStudents marks = [(fst mark, grade mark) | mark <- marks]

-- 10

-- duplicate :: String -> Int -> String
-- duplicate str 1 = str
-- duplicate str n = str ++ duplicate str (n-1)

duplicateWithList :: String -> Int -> String
duplicateWithList str n = concat [str | _ <- [1 .. n]]

-- 11

divisors :: Int -> [Int]
divisors n = [i | i <- [1 .. n], n `mod` i == 0]

-- 12

isPrime :: Int -> Bool
isPrime n = length (divisors n) == 2

-- 13

split :: [(a,b)] -> ([a], [b])
split list = ([fst elem | elem <- list], [snd elem | elem <- list])
