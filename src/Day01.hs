module Day01 where

import Data.Char (digitToInt)
import Util (readInput, printResults)

day :: Int
day = 1

computeSum :: [Int] -> Int
computeSum xs = sum $ zipWith keepEquals xs xs'
  where keepEquals a b = if a == b then a else 0
        xs' = tail xs ++ [head xs]

computeSum' :: ([Int] -> [Int]) -> [Int] -> Int
computeSum' f xs = sum $ zipWith keepEquals xs (f xs)
  where keepEquals a b = if a == b then a else 0

shiftOne :: [Int] -> [Int]
shiftOne xs = tail xs ++ [head xs]

shiftMid :: [Int] -> [Int]
shiftMid xs = right ++ left
  where (left, right) = splitAt center xs
        center = length xs `div` 2

main :: IO ()
main = do
  input <- fmap digitToInt <$> readInput day
  printResults day [
    computeSum' shiftOne input,
    computeSum' shiftMid input
    ]
