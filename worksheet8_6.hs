-- 6

-- Functional

addWord :: String -> [String] -> [String]
addWord word list = list ++ [word]

wordsToString :: [String] -> String
wordsToString [] = ""
wordsToString [a] = a -- prevent \n at last word
wordsToString (x : xs) = x ++ "\n" ++ wordsToString xs

wordsOfLength :: Int -> [String] -> [String]
wordsOfLength n = filter (\word -> length word == n)

-- UI
main :: IO ()
main = do
  contents <- readFile "words.txt"
  let words = read contents :: [String]

  getInput words

getInput :: [String] -> IO ()
getInput words = do
  putStrLn "(a) add a word to the list"
  putStrLn "(b) display all words"
  putStrLn "(c) display all words of a given length"
  putStrLn "(d) exit"

  putStr "Choose option: "

  input <- getLine
  if input == "a"
    then do
      putStr "Enter word to add: "
      word <- getLine
      let newWords = addWord word words
      getInput newWords
    else
      if input == "b"
        then do
          putStrLn (wordsToString words)
          getInput words
        else
          if input == "c"
            then do
              targetLength <- getLine
              let n = read targetLength :: Int
              putStrLn (wordsToString (wordsOfLength n words))
              getInput words
            else do
              writeFile "words.txt" (show words)
