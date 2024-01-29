{-# LANGUAGE AllowAmbiguousTypes #-}
module Main where

import qualified Day01 
import qualified Day02

main :: IO ()
main = do
  Day01.printResult "input/day-01/full.txt"
  Day02.printResult "input/day-02/full.txt"
