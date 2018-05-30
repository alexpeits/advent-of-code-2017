module Day05 where

import System.IO.Unsafe (unsafePerformIO)

import Control.Concurrent (threadDelay)

import qualified Data.Sequence as S
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Mutable as MV
import Util (readInputLines, printResults)

runSeq :: (Int -> Int) -> S.Seq Int -> Int
runSeq f xs = length $ takeWhile notDone $ iterate doRun (xs, 0)
  where len = S.length xs
        notDone (l, p) = p >= 0 && p < len
        doRun (l, p)   = (newL, newP)
          where newL = S.update p (f valP) l
                newP = p + valP
                valP = S.index l p

runVec :: (Int -> Int) -> V.Vector Int -> Int
runVec f xs = length $ takeWhile notDone $ iterate doRun (xs, 0)
  where len = V.length xs
        notDone (l, p) = p >= 0 && p < len
        doRun (l, p)   = (newL, newP)
          where newL = l V.// [(p, f valP)]
                newP = p + valP
                valP = l V.! p

runMVec :: (Int -> Int) -> MV.IOVector Int -> Int
runMVec f xs = length $ takeWhile notDone $ iterate doRun 0
  where len = MV.length xs
        notDone p = p >= 0 && p < len
        doRun p = unsafePerformIO $ do
          valP <- MV.read xs p
          MV.write xs p (f valP)
          return $ p + valP

main :: IO ()
main = do
  -- compile with:
  -- stack ghc -- -o exe -O2 -main-is Day05 src/Day05.hs -isrc/
  let day = 5

  -- Part 1
  input <- V.fromList . fmap read <$> readInputLines day

  putStr "Part 1: "
  print $ runVec succ input
  -- 0.27s user 0.02s system 98% cpu 0.285 total

  -- Part 2
  inputMut <- fmap read <$> readInputLines day :: IO [Int]
  let len = length inputMut
  vec <- MV.new len
  mapM_ (uncurry $ MV.write vec) $ zip [0..] inputMut

  putStr "Part 2: "
  print $ runMVec (\x -> if x >= 3 then pred x else succ x) vec
  -- 0.92s user 0.01s system 99% cpu 0.937 total
