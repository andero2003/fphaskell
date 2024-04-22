import Data.Time

type Title = String
type Description = String
type IsReviewed = Bool
type Date = Day

data DiaryEntry = DiaryEntry Title Description IsReviewed Date

diaryEntries :: [DiaryEntry]
diaryEntries = 
  [
    DiaryEntry "Learnt about GitHub" "I completed a tutorial on how to use GitHub" True (fromGregorian 2023 03 15),
    DiaryEntry "Met with the clients" "I got to meet with the clients of the project" True (fromGregorian 2023 03 17),
    DiaryEntry "Completed health and safety training" "I just completed the health and safety training" False (fromGregorian 2023 03 21),
    DiaryEntry "First staff meeting" "I attended my first staff meeting" True (fromGregorian 2023 03 23),
    DiaryEntry "Learnt about C" "Did some C stuff" False (fromGregorian 2023 03 24)
  ]

printEntry :: DiaryEntry -> IO ()
printEntry (DiaryEntry title desc reviewed date) = do
  putStrLn ("Title: " ++ title)
  putStrLn desc
  putStrLn ((if reviewed then "Reviewed" else "Not reviewed") ++ ", added on " ++ show date)

formatEntries :: [DiaryEntry] -> IO ()
formatEntries [] = do return ()
formatEntries (entry: rest) = do
  printEntry entry
  putStrLn ""
  formatEntries rest 