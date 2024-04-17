helloWorld :: IO ()
helloWorld = putStrLn "Hello, World!"

displayFile :: IO ()
displayFile = do
  putStr "Enter the filename: "
  name <- getLine
  contents <- readFile name
  putStr contents

getInt :: IO Int
getInt = do
  str <- getLine
  return (read str :: Int)

isPalindrome :: String -> String
isPalindrome str
  | str == reverse str = str ++ " is a palindrome"
  | otherwise = str ++ " is not a palindrome"

pal :: IO ()
pal = do
  line <- getLine
  let response = isPalindrome line
  putStrLn response

palLines :: IO ()
palLines = do
  putStr "Enter a line: "
  str <- getLine
  if str == ""
    then return ()
    else do
      putStrLn (isPalindrome str)
      palLines

-- 1

greeting :: IO ()
greeting = do
  putStrLn "Enter your name: "
  name <- getLine
  putStrLn ("Hello " ++ name)

-- 2

addTwoNumbers :: IO ()
addTwoNumbers = do
  putStrLn "Enter first number: "
  a <- getInt
  putStrLn "Enter second number: "
  b <- getInt
  let result = show (a + b)
  putStrLn result

-- 3

copyFile :: IO ()
copyFile = do
  putStrLn "Enter file name (.txt): "
  fileName <- getLine
  contents <- readFile fileName
  putStrLn "Enter copy name (.txt): "
  copyName <- getLine
  writeFile copyName contents

-- 4

buildList :: [String] -> IO ()
buildList list = do
  putStr "Enter a line: "
  str <- getLine
  if str /= ""
    then do
      putStrLn ("List is now " ++ show (str : list))
      buildList (str : list)
    else return ()

listBuilder :: IO ()
listBuilder = do
  buildList []

-- 5

askInts :: Int -> Int -> IO Int
askInts n total
  | n == 0 = do
      return total
  | otherwise = do
      putStrLn "Enter number: "
      a <- getInt
      askInts (n - 1) (total + a)

sumIntegers :: IO ()
sumIntegers = do
  putStrLn "Enter total amount of numbers: "
  n <- getInt
  total <- askInts n 0
  putStrLn ("Total: " ++ show total)
