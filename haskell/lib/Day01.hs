module Day01 (printResult) where

import Data.Functor.Foldable
import Control.Monad ((>=>))
import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Lazy (
	Parser,
	many1,
	parse,
	eitherResult
	)
import Data.Attoparsec.ByteString.Char8 (
	char
	)

import Data.ByteString.Lazy as B (readFile)
import Data.List (find)

data Instruction = Down | Up deriving (Show, Eq, Enum)

-- | The cata function is a catamorphism, which is a generalization of a fold.
-- It takes a function that describes how to fold the structure, and a structure
-- to fold. The function is called for each constructor in the structure, and
-- the results are combined using the provided function.
--
-- Example:
-- >>> part1 [Down, Down, Up, Up]
-- 0
-- >>> part1 [Up, Up, Up]
-- 3
-- >>> part1 [Up, Up, Down, Up, Up, Down, Up ]
-- 3
-- >>> part1 [Down, Down, Up, Up, Up, Up, Up ]
-- 3
-- >>> part1 [ Up, Down, Down ]
-- -1
-- >>> part1 [ Down, Down, Up ]
-- -1
-- >>> part1 [ Down, Down, Down ]
-- -3
-- >>> part1 [ Down, Up, Down, Down, Up, Down, Down ]
-- -3
part1 :: [Instruction] -> Integer
part1 = cata go
	where
	go :: ListF Instruction Integer -> Integer
	go Nil = 0
	go (Cons Up n ) = n + 1
	go (Cons Down n ) = n - 1

-- | find the first position that causes the instructions to enter the basement
--
-- Example:
-- >>> part2 [Down]
-- Just 1
-- >>> part2 [Up, Down, Down]
-- Just 3
-- >>> part2 [Up, Down, Up, Down, Down]
-- Just 5
part2 :: [Instruction] -> Maybe Int
part2 = fmap ( (+ (-1)) . fst ) . find ( (<0) . snd) . zip [1..] . floorN
	where
	floorN :: [Instruction] -> [Int]
	floorN = scanl (+) 0 . fmap mapper
	mapper :: Instruction -> Int
	mapper Up = 1
	mapper Down = -1

inputParser :: Parser [Instruction]
inputParser = many1 $ (Up <$ char '(') <|> (Down <$ char ')')

printResult :: FilePath -> IO ()
printResult = B.readFile >=> either
	(print . ("Error: " ++) . show)
	(\input -> do
		print $ part1 input
		print $ part2 input
	) .
	eitherResult . parse inputParser
