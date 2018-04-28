module Day02 where

import Util (readInputLines, printResults)

computeChecksum :: [[Int]] -> Int
computeChecksum = sum . map ((-) <$> maximum <*> minimum)

computeChecksum' :: [[Int]] -> Int
computeChecksum' = sum . concatMap evenlyDiv
  where evenlyDiv xs = [a `div` b | a <- xs
                                  , b <- xs
                                  , b /= 1
                                  , a /= b
                                  , a `mod` b == 0]

main :: IO ()
main = do
  let day = 2
  input <- fmap (fmap read . words) <$> readInputLines day
  printResults day [
    computeChecksum input,
    computeChecksum' input
    ]
