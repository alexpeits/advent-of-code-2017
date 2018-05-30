module Day03 where

import Data.List (splitAt, elemIndex)
import Data.Maybe (fromJust)
import Control.Arrow (first, second, (***))

import Util (printResults)

getStartN :: Int -> Int
getStartN 0 = 1
getStartN r = 2 + sum (map (* 8) [0..r - 1])

getRingOfN :: Int -> Int
getRingOfN n = pred $ length $ takeWhile (n >=) $ map getStartN [0..]

getRing :: Int -> [Int]
getRing r = [start..end - 1]
  where start = getStartN r
        end = getStartN (r + 1)

solve1 :: Int -> Int
solve1 1 = 0
solve1 n = ringN + abs d
  where ringN = getRingOfN n
        ring  = getRing ringN
        l' = length ring `div` 4
        pos = fromJust (elemIndex n ring)
        d = pos `mod` l' - l' `div` 2 + 1

-- Part 2

data Direction = U | L | D | R deriving (Eq, Show)
type Pos = (Int, Int)

turn :: Direction -> Direction
turn U = L
turn L = D
turn D = R
turn R = U

move :: Direction -> Pos -> Pos
move U = second succ
move L = first pred
move D = second pred
move R = first succ

getNeighbors :: Pos -> [Pos]
getNeighbors p =
  [ pos
  | dx <- [-1..1]
  , dy <- [-1..1]
  , let pos = (***) (+ dx) (+ dy) p
  , pos /= p
  ]

spiralDirections :: [Direction]
spiralDirections = concat
  [ replicate x d
  | x <- [1..]
  , d <- [U, L, L, D, D, R, R, R]
  ]

main :: IO ()
main = do
  let day = 3
      input = 325489
  print $ solve1 input
