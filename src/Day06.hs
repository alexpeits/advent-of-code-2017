module Day06 where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (elem, elemIndex)
import Data.Maybe (fromJust)

import Util (readInputWords, printResults)

getIndices :: Int -> Int -> [Int]
getIndices start len = map (`mod` len) $ drop (start + 1) [0..]

maxIndex :: [Int] -> Int
maxIndex = fromJust . (flip elemIndex <*> maximum)
-- maxIndex xs = fromJust $ flip elemIndex xs $ maximum xs

replaceI :: Int -> (Int -> Int) -> [Int] -> [Int]
replaceI i f xs = left ++ f x : right
    where (left, x : right) = splitAt i xs

wipe :: Int -> [Int] -> [Int]
wipe = flip replaceI (const 0)

run :: [Int] -> S.Set [Int] -> Int
run xs seen = 1 + if S.member newXs seen then 0 else run newXs newSeen
  where newSeen = S.insert newXs seen
        mi = maxIndex xs
        m = maximum xs
        l = length xs
        newXs = foldr (\i xs' -> replaceI i succ xs') (wipe mi xs) (take m $ getIndices mi l)

type State = M.Map [Int] Int

run' :: [Int] -> Int -> [[Int]] -> [[Int]]
run' xs i seen = if newXs `elem` seen then newSeen else run' newXs (succ i) newSeen
  where newSeen = seen ++ [newXs]
        mi = maxIndex xs
        m = maximum xs
        l = length xs
        newXs = foldr (\i xs' -> replaceI i succ xs') (wipe mi xs) (take m $ getIndices mi l)

main :: IO ()
main = do
  let day = 6
  input <- fmap read <$> readInputWords day
  let sol = run' input 0 []
  printResults day [
    length sol,
    length sol - fromJust (elemIndex (last sol) sol) - 1
    ]
