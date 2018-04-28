module Day04 where

import Data.List (nub, sort)
import Util (readInputLines, printResults)

isValidPass :: [String] -> Bool
isValidPass = (==) . length <*> length . nub

isValidPassAnagram :: [String] -> Bool
isValidPassAnagram = (==) . length <*> length . nub . map sort

count :: Eq a => a -> [a] -> Int
count a = length . filter (== a)

main :: IO ()
main = do
  let day = 4
  input <- fmap words <$> readInputLines day
  printResults day [
    count True $ map isValidPass input,
    count True $ map isValidPassAnagram input
    ]
