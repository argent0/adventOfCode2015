module Day03 (printResult) where

import Control.Monad ((>=>))
import Data.Attoparsec.ByteString.Lazy (
	Parser,
	many1,
	parse,
	eitherResult
	)
import Data.Attoparsec.ByteString.Char8 (
	char,
	)

import Data.ByteString.Lazy as B (readFile)
import Control.Applicative ((<|>))
import Data.Functor (($>))

-- import Data.Matrix (Matrix)
-- import qualified Data.Matrix as DM

import Data.List (foldl')
-- import Data.Vector (Vector, fromList, (!))

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Word (Word64)
import Data.Bits (setBit, testBit, popCount, zeroBits)

data Move = North | South | East | West deriving (Show, Eq)

-- | A Field is an associates a bit to a position
-- This is: Map y (Map x Word64)
newtype Field = Field (Map Int (Map Int Word64))

-- | Creates a fields with all bits set to 0
zero :: Field
zero = Field M.empty

-- | Sets the bit at the given position to 1
setElem :: Int -> Int -> Field -> Field
setElem x y (Field m) = Field $ M.alter alterY y m
	where
	storeX = x `div` 64
	bitX = x `mod` 64
	alterY :: Maybe (Map Int Word64) -> Maybe (Map Int Word64)
	alterY Nothing = Just $ M.singleton storeX (setBit zeroBits bitX)
	alterY (Just m') = Just $ M.alter alterX storeX m'
		where
		alterX :: Maybe Word64 -> Maybe Word64
		alterX Nothing = Just $ setBit zeroBits bitX
		alterX (Just w) = Just $ setBit w bitX

-- | Returns the value of the bit at the given position
-- If the position is not set, returns 0
-- >>> getElem 0 0 zero
-- False
-- >>> getElem 0 0 $ setElem 0 0 zero
-- True
-- >>> getElem 1 1 zero
-- False
-- >>> getElem 1 1 $ setElem 1 1 zero
-- True
-- >>> let p = (,) <$> [0..128] <*> [0..128]; field = foldl' (\f (x, y) -> setElem x y f) zero p in getElem 0 0 field
-- True
-- >>> let p = (,) <$> [0..128] <*> [0..128]; field = foldl' (\f (x, y) -> setElem x y f) zero p in all (\(x, y) -> getElem x y field) p
-- True
getElem :: Int -> Int -> Field -> Bool
getElem x y (Field m) = case M.lookup y m of
	Nothing -> False
	Just m' -> case M.lookup storeX m' of
		Nothing -> False
		Just w -> testBit w bitX
	where
	storeX = x `div` 64	
	bitX = x `mod` 64


-- | Returns the number of bits set to 1 in the field
-- >>> count zero
-- 0
-- >>> count $ setElem 0 0 zero
-- 1
-- >>> count $ setElem 1 0 zero
-- 1
-- >>> count $ setElem 0 1 zero
-- 1
-- >>> count $ setElem 1 1 zero
-- 1
-- >>> let p = (,) <$> [0..1] <*> [0..0]; field = foldl' (\f (x, y) -> setElem x y f) zero p in count field
-- 2
-- >>> let p = (,) <$> [0..1] <*> [0..1]; field = foldl' (\f (x, y) -> setElem x y f) zero p in count field
-- 4
count :: Field -> Int
count (Field m) = M.foldl' folder 0 m
	where
	folder :: Int -> Map Int Word64 -> Int
	folder acc m' = acc + M.foldl' (\acc' w -> acc' + popCount w) 0 m'

-- | Computes the number of houses visited by Santa
-- Santa starts at (0, 0) and moves according to the list of moves
-- >>> part1 [East]
-- 2
-- >>> part1 [North, South]
-- 2
-- >>> part1 [North, West, South, East]
-- 4
-- >>> part1 $ concat $ replicate 5 [North, South]
-- 2
part1 :: [Move] -> Int
part1 = count . snd . foldl' folder ((0, 0), setElem 0 0 zero)
	where
	folder :: ( (Int, Int), Field ) -> Move -> ( (Int, Int), Field )
	folder ( (x, y), f ) North = ( (x,   y + 1), setElem x       (y + 1) f )
	folder ( (x, y), f ) South = ( (x,   y - 1), setElem x       (y - 1) f )
	folder ( (x, y), f ) East  = ( (x + 1, y  ), setElem (x + 1) y       f )
	folder ( (x, y), f ) West  = ( (x - 1, y  ), setElem (x - 1) y       f )

-- | Computes the number of houses visited by Santa and Robo-Santa
-- >>> part2 [East]
-- 2
-- >>> part2 [North, South]
-- 3
-- >>> part2 [North, West, South, East]
-- 3
-- >>> part2 $ concat $ replicate 5 [North, South]
-- 11
part2 :: [Move] -> Int
part2 = count . snd . foldl' folder ( ((0, 0), (0, 0)), setElem 0 0 zero ) . zip [0..]
	where
	makeMove :: (Int, Int) -> Move -> (Int, Int)
	makeMove (x, y) North = (x,   y + 1)
	makeMove (x, y) South = (x,   y - 1)
	makeMove (x, y) East  = (x + 1, y  )
	makeMove (x, y) West  = (x - 1, y  )
	folder :: ( ( (Int, Int), (Int, Int) ), Field ) -> (Int, Move) -> ( ( (Int, Int), (Int, Int) ), Field )
	folder ( ( (x, y), (rx, ry) ), f ) (i, m) = if even i
		then ( ( (x', y'), (rx, ry  ) ), setElem x'   y' f )
		else ( ( (x,  y ), (rx', ry') ), setElem rx' ry' f )
		where
		(x', y') = makeMove (x, y) m
		(rx', ry') = makeMove (rx, ry) m

inputParser :: Parser [Move]
inputParser = many1 moveParser
	where
	moveParser :: Parser Move
	moveParser = char '^' $> North
		<|> char 'v' $> South
		<|> char '>' $> East
		<|> char '<' $> West

printResult :: FilePath -> IO ()
printResult = B.readFile >=> either
	(print . ("Error: " ++) . show)
	(\input -> do
		print $ part1 input
		print $ part2 input
	) .
	eitherResult . parse inputParser
