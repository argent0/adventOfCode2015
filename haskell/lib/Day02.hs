module Day02 (printResult) where

import Control.Monad ((>=>))
import Data.Attoparsec.ByteString.Lazy (
	Parser,
	many1,
	parse,
	eitherResult
	)
import Data.Attoparsec.ByteString.Char8 (
	decimal,
	char,
	)

import Data.ByteString.Lazy as B (readFile)
import Data.List (foldl')

data Dimensions = Dimensions Integer Integer Integer deriving (Show, Eq)

-- | Computes the total surface area of a list of dimensions
-- | The surface area of a box is the sum of the areas of its sides
-- | plus the area of the smallest side
--
-- Example:
-- >>> part1 [Dimensions 2 3 4]
-- 58
-- >>> part1 [Dimensions 1 1 10]
-- 43
part1 :: [Dimensions] -> Integer
part1 = foldl' folder 0
	where
	folder :: Integer -> Dimensions -> Integer
	folder acc (Dimensions l w h) = acc + foldl' (+) 0 ( (*2) <$> sideAreas ) + minimumArea
		where
		lw, wh, hl :: Integer
		lw = l * w
		wh = w * h
		hl = h * l
		sideAreas :: [Integer]
		sideAreas = [lw, wh, hl]
		minimumArea :: Integer
		minimumArea 
			| lw <= wh && lw <= hl = lw
			| wh <= lw && wh <= hl = wh
			| otherwise = hl

-- | Computes the total length of ribbon required to wrap a list of dimensionso
-- 
-- Example:
-- >>> part2 [Dimensions 2 3 4]
-- 34
-- >>> part2 [Dimensions 1 1 10]
-- 14
part2 :: [Dimensions] -> Integer
part2 = foldl' folder 0
	where
	folder :: Integer -> Dimensions -> Integer
	folder acc (Dimensions l w h) = acc + (2 * minimumPerimeter) + volume
		where
		lw, wh, hl :: Integer
		lw = l + w
		wh = w + h
		hl = h + l
		volume :: Integer
		volume = l * w * h
		minimumPerimeter :: Integer
		minimumPerimeter
			| lw <= wh && lw <= hl = lw
			| wh <= lw && wh <= hl = wh
			| otherwise = hl

inputParser :: Parser [Dimensions]
inputParser = many1 $ Dimensions <$> (decimal <* char 'x')  <*> (decimal <* char 'x')  <*> ( decimal <* char '\n' )

printResult :: FilePath -> IO ()
printResult = B.readFile >=> either
	(print . ("Error: " ++) . show)
	(\input -> do
		print $ part1 input
		print $ part2 input
	) .
	eitherResult . parse inputParser
