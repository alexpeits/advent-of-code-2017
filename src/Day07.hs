module Day07 where

import Util (readInputLines, printResults)



main :: IO ()
main = do
  let day = 7
  input <- fmap words <$> readInputLines day
  printResults day [
    0
    ]
