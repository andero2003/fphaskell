import Data.Bits
isEven :: Int -> Bool
isEven x = bitandOne x == 0
  where bitandOne x = (.&.) x 1

incrementList :: [Int] -> [Int]
incrementList x = [i + 1 | i <- x, i < 4]

double :: Int -> Int
double a = a * 2

myMap :: [a] -> (a -> b) -> [b]
myMap listA func = [func x | x <- listA]
