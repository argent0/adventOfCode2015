module Day05 where

import Data.Attoparsec.ByteString.Char8 (letter_ascii, char)
import Data.Attoparsec.ByteString (Parser, sepBy1, many1, eitherResult, parse)
import qualified Data.ByteString.Char8 as B
import Data.List (find)
import Control.Monad ((>=>))
import Text.Regex.TDFA ((=~))
import Data.Maybe (isJust)
import Control.Applicative ((<|>))

two :: (a -> Bool) -> [a] -> Maybe (a, a)
two _ [] = Nothing
two _ [_] = Nothing
two f (x:xs) = if f x
	then (x, ) <$> find f xs
	else two f xs

three :: (a -> Bool) -> [a] -> Maybe (a, (a, a))
three _ [] = Nothing
three _ [_] = Nothing
three _ [_, _] = Nothing
three f (x:xs) = if f x
	then (x, ) <$> two f xs
	else three f xs

threeVowels :: String -> Maybe (Char, (Char, Char))
threeVowels = three (`elem` "aeiou")

twiceInARow :: String -> Maybe (Char, Char)
twiceInARow xs = find (uncurry (==)) . zip xs $ tail xs

-- | Returns True if the string has a bad string
-- >>> hasBadStrings ["ab", "cd", "pq", "xy"] "abc"
-- True
-- >>> hasBadStrings ["ab", "cd", "pq", "xy"] "haegwjzuvuyypxyu"
-- True
hasBadStrings :: [String] -> String -> Bool
hasBadStrings [] _ = False
hasBadStrings (s:ss) p = p =~ badsRegex
	where
	badsRegex = foldr1 (\a b -> a ++ "|" ++ b) (s:ss)

-- | Returns True if the string is nice
-- >>> nice "ugknbfddgicrmopn"
-- True
-- >>> nice "aaa"
-- True
-- >>> nice "jchzalrnumimnmhp"
-- False
-- >>> nice "haegwjzuvuyypxyu"
-- False
-- >>> nice "dvszwmarrgswjxmb"
-- False
nice :: String -> Bool
nice s = not (hasBadStrings ["ab", "cd", "pq", "xy"] s) && isJust ( threeVowels s *> twiceInARow s )

-- | True if the string has a pair of letters that appears at least twice without overlapping
-- >>> pairTwice "xyxy"
-- Just ('x','y')
-- >>> pairTwice "aaa"
-- Nothing
-- >>> pairTwice "qjhvhtzxzqqjkmpb"
-- Just ('q','j')
-- >>> pairTwice "xxyxx"
-- Just ('x','x')
-- >>> pairTwice "uurcxstgmygtbstg"
-- Just ('s','t')
-- >>> pairTwice "ieodomkazucvgmuy"
-- Nothing
pairTwice :: String -> Maybe (Char, Char)
pairTwice [] = Nothing
pairTwice [_] = Nothing
pairTwice (x:y:xs) = find (== (x, y)) (zip xs (tail xs)) <|> pairTwice (y:xs)

-- | Extracts the letter that repeats with exactly one letter between them
-- >>> repeatWithOne "xyx"
-- Just ('x','y')
-- >>> repeatWithOne "abcd"
-- Nothing
-- >>> repeatWithOne "aaa"
-- Just ('a','a')
-- >>> repeatWithOne "qjhvhtzxzqqjkmpb"
-- Just ('h','v')
-- >>> repeatWithOne "xxyxx"
-- Just ('x','y')
-- >>> repeatWithOne "uurcxstgmygtbstg"
-- Nothing
-- >>> repeatWithOne "ieodomkazucvgmuy"
-- Just ('o','d')
repeatWithOne :: String -> Maybe (Char, Char)
repeatWithOne [] = Nothing
repeatWithOne [_] = Nothing
repeatWithOne [_, _] = Nothing
repeatWithOne (x:x':x'':xs)
	| x == x'' = Just (x, x')
	| otherwise = repeatWithOne (x':x'':xs)

part1 :: [String] -> Int
part1 = length . filter nice

part2 :: [String] -> Int
part2 = length . filter (\s -> isJust (pairTwice s *> repeatWithOne s))

inputParser :: Parser [String]
inputParser = sepBy1 (many1 letter_ascii) (char '\n')

printResult :: FilePath -> IO ()
printResult = B.readFile >=> either
	(print . ("Error: " ++) . show)
	(\input -> do
		print $ part1 input
		print $ part2 input
	) .
	eitherResult . parse inputParser
