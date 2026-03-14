module Main where

import qualified Data.List as List
import qualified Data.Map as Map

parse :: String -> [String]
parse = List.transpose . filter (/="") . lines

frequences :: Ord a => [a] -> Map.Map a Int
frequences xs = Map.fromListWith (+) [(x, 1) | x <- xs]

mostCommon :: Ord a => [a] -> a
mostCommon = extractValue . List.minimumBy compareFrequences . Map.toList . frequences
    where
        extractValue (v, f) = v
        compareFrequences (v1, f1) (v2, f2) = case compare f2 f1 of
            EQ -> compare v1 v2
            t -> t

leastCommon :: Ord a => [a] -> a
leastCommon = extractValue . List.minimumBy compareFrequences . Map.toList . frequences
    where
        extractValue (v, f) = v
        compareFrequences (v1, f1) (v2, f2) = case compare f1 f2 of
            EQ -> compare v1 v2
            t -> t

main :: IO ()
main = do
    source <- parse <$> getContents
    putStrLn $ "Part 1: " ++ show (map mostCommon source)
    putStrLn $ "Part 2: " ++ show (map leastCommon source)
