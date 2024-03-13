{-# LANGUAGE AllowAmbiguousTypes #-}
module Main where

import qualified Day01 
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05

main :: IO ()
main = do
  Day01.printResult "input/day-01/full.txt"
  Day02.printResult "input/day-02/full.txt"
  Day03.printResult "input/day-03/full.txt"
  Day04.printResult "input/day-04/full.txt"
  Day05.printResult "input/day-05/full.txt"
