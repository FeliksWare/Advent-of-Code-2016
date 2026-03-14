module Main where

import qualified Data.List as List
import qualified Data.Map as Map

parse :: String -> [String]
parse = filter (/="") . lines

mostCommon :: Ord a => [a] -> a
mostCommon xs = extractChar $ List.minimumBy compareFrequences (Map.toList $ Map.fromListWith (+) [(x, 1) | x <- xs])
    where
        extractChar (c, f) = c
        compareFrequences (c1, f1) (c2, f2) = case compare f2 f1 of
            EQ -> compare c1 c2
            t -> t

part1 :: [String] -> String
part1 = map mostCommon . List.transpose

main :: IO ()
main = do
    source <- parse <$> getContents
    putStrLn $ "Part 1: " ++ show (part1 source)
