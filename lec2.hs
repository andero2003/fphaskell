import Data.Bits
isEven :: Int -> Bool
isEven x = bitandOne x == 0
  where bitandOne x = (.&.) x 1

