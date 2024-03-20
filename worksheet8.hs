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
  a <- getLine
  let intA = read a :: Int
  putStrLn "Enter second number: "
  b <- getLine
  let intB = read b :: Int
  let result = show (intA + intB)
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
