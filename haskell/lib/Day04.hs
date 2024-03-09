module Day04 where

import Crypto.Hash.MD5 (hash)
import Data.ByteString (ByteString, pack, unpack)
import Data.List (isPrefixOf, find)

part1 :: String -> Int -> Maybe Int
part1 secretKey limit = find predicate [0..limit]
	where
	predicate :: Int -> Bool
	predicate i = "00000" `isPrefixOf` (take 5 $ unpack _)
