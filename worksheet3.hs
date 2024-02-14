-- We don't import '||' from the prelude, so that we can
-- define our own version

import Prelude hiding (gcd, (&&), (||))

-- The following line declares the || operator (which we are about to
-- re-define) to be right associative and to have precedence 2. This
-- is necessary in order for expressions such as False || x > 2 to be
-- valid (e.g. it sets the precedence of || to be lower than >).

infixr 2 ||

infixr 3 &&

-- A naive re-implementation of the Prelude operator ||
(||) :: Bool -> Bool -> Bool
True || True = True
False || True = True
True || False = True
False || False = False

-- 1

(&&) :: Bool -> Bool -> Bool
True && True = True
_ && _ = False

-- 2

exOr :: Bool -> Bool -> Bool
exOr False True = True
exOr True False = True
exOr _ _ = False

-- 3

ifThenElse :: Bool -> Int -> Int -> Int
ifThenElse True a _ = a
ifThenElse _ _ b = b

-- 4

daysInMonth :: Int -> Int
daysInMonth 2 = 28
daysInMonth 4 = 30
daysInMonth 6 = 30
daysInMonth 9 = 30
daysInMonth 11 = 30
daysInMonth _ = 31

validDate :: Int -> Int -> Bool
validDate dd mm = dd >= 1 && dd <= daysInMonth mm

-- 5

sumNumbers :: Int -> Int
sumNumbers 0 = 0
sumNumbers n = n + sumNumbers (n - 1)

sumNumbersGuard :: Int -> Int
sumNumbersGuard n
  | n == 0 = 0
  | otherwise = n + sumNumbersGuard (n - 1)

-- 6

sumSquares :: Int -> Int
sumSquares 0 = 0
sumSquares n = n ^ 2 + sumSquares (n - 1)

sumSquaresGuard :: Int -> Int
sumSquaresGuard n
  | n == 0 = 0
  | otherwise = n ^ 2 + sumSquaresGuard (n - 1)

-- 7

power :: Int -> Int -> Int
power _ 0 = 1
power m n = m * power m (n - 1)

powerGuard :: Int -> Int -> Int
powerGuard m n
  | n == 0 = 1
  | otherwise = m * power m (n - 1)

-- 8

sumFromTo :: Int -> Int -> Int
sumFromTo a b
  | a > b = 0
  | otherwise = a + sumFromTo (a + 1) b

-- 9

gcd :: Int -> Int -> Int
gcd a b
  | a == b = a
  | otherwise = gcd (min a b) (abs (a - b))

-- 10

findRoot :: Int -> Int -> Int
findRoot n s
  | s * s <= n = s
  | otherwise = findRoot n (s - 1)

intSquareRoot :: Int -> Int
intSquareRoot n = findRoot n n