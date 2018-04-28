module Day05 where

import qualified Data.Sequence as S
import qualified Data.Vector.Unboxed as V
import Util (readInputLines, printResults)

runSeq :: (Int -> Int) -> S.Seq Int -> Int
runSeq f xs = length $ takeWhile notDone $ iterate doRun (xs, 0)
  where notDone (l, p) = p >= 0 && p < S.length l
        doRun (l, p)   = (newL, newP)
          where newL = S.update p (f valP) l
                newP = p + valP
                valP = S.index l p

runVec :: (Int -> Int) -> V.Vector Int -> Int
runVec f xs = length $ takeWhile notDone $ iterate doRun (xs, 0)
  where notDone (l, p) = p >= 0 && p < V.length l
        doRun (l, p)   = (newL, newP)
          where newL = l V.// [(p, f valP)]
                newP = p + valP
                valP = l V.! p

main :: IO ()
main = do
  let day = 5
  input <- V.fromList . fmap read <$> readInputLines day
  printResults day [
    runVec succ input,
    -- second part takes a long time
    -- TODO: write it using Data.Vector.Mutable.MVector
    runVec (\x -> if x >= 3 then pred x else succ x) input
    ]
