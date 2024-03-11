module Day04 where

import Data.Digest.Pure.MD5 (md5)
import Data.List (isPrefixOf, find)
import Data.Attoparsec.ByteString.Char8 (letter_ascii, char)
import Data.Attoparsec.ByteString (Parser, manyTill, eitherResult, parse)
import qualified Data.ByteString.Char8 as B
import Control.Monad ((>=>))

solver :: Int -> Int -> String -> Maybe Int
solver limit n key = find predicate [0..limit]
	where
	predicate = (limitStr `isPrefixOf`) . show . md5 .
		B.fromStrict . B.pack . (key ++) .  show
	limitStr = replicate n '0'

part1 :: String -> Maybe Int
part1 = solver (maxBound :: Int) 5

part2 :: String -> Maybe Int
part2 = solver (maxBound :: Int) 6

inputParser :: Parser String
inputParser = manyTill letter_ascii (char '\n')

printResult :: FilePath -> IO ()
printResult = B.readFile >=> either
	(print . ("Error: " ++) . show)
	(\input -> do
		print $ part1 input
		print $ part2 input
	) .
	eitherResult . parse inputParser
