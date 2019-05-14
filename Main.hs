module Main where

import System.Environment
import Simpath.Border as B
import Simpath.CounterMap
import Simpath.Edge as E

gridEdges :: Int -> [Edge]
gridEdges size = upper ++ lower
  where
    upper = snd $ foldl (\(c, ts) n -> (c + n, ts ++ edgesAt n c)) (0, []) [1 .. size-1]
      where edgesAt n acc = map (+ acc) [1 .. n] >>= addPair
              where addPair i = let f = E.edge i . (i + n +) in map f [0, 1]
    lower = reverse $ map (modify (size^2 + 1 -)) upper

main :: IO ()
main = do args <- getArgs
          print $ countPaths $ borders $ gridEdges $ (read $ head args :: Int)
