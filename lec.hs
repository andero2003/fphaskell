import Data.Bits
isEven :: Int -> Bool
isEven x = bitandOne x == 0
  where bitandOne x = (.&.) x 1

incrementList :: [Int] -> [Int]
incrementList x = [i + 1 | i <- x, i < 4]

