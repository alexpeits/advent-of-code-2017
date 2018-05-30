module Util (
  readInput,
  readInputWords,
  readInputLines,
  printResults
  ) where

import System.FilePath ((</>), (<.>))

getDayDisplay :: Int -> String
getDayDisplay day =  if day < 10 then "0" ++ show day else show day

getInputPath :: Int -> FilePath
getInputPath day = "src" </> "Day" ++ getDayDisplay day <.> "txt"

readInput :: Int -> IO String
readInput = readFile . getInputPath

readInputWords :: Int -> IO [String]
readInputWords = fmap words . readInput

readInputLines :: Int -> IO [String]
readInputLines = fmap lines . readInput

printResults :: Show a => Int -> [a] -> IO ()
printResults day results = do
  putStrLn $ "Day " ++ getDayDisplay day
  putStrLn "------"
  mapM_ printParts $ zip [1..] results
  putStrLn "------"
  where printParts (part, res) = do
          putStr $ "Part " ++ show part ++ ": "
          print res
