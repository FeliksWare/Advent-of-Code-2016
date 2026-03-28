module Main where

import Data.List

parseLine :: String -> (Int, Int)
parseLine l = let (a, b) = break (=='-') l in (read a, read $ drop 1 b)

parse :: String -> [(Int, Int)]
parse = map parseLine . filter (/="") . lines

findSmallest :: [(Int, Int)] -> Int -> Int
findSmallest bounds value = case find (\(min, max) -> min <= value && value <= max) bounds of
    Just (min, max) -> findSmallest bounds $ max + 1
    Nothing -> value

part1 :: [(Int, Int)] -> Int
part1 bounds = findSmallest bounds 0

findCount :: [(Int, Int)] -> Int -> Int -> Int
findCount bounds value count | value > 4294967295 = count
findCount bounds value count = case find (\(min, max) -> min <= value && value <= max) bounds of
    Just (min, max) -> findCount bounds (max + 1) count
    Nothing -> findCount bounds (value + 1) (count + 1)

part2 :: [(Int, Int)] -> Int
part2 bounds = findCount bounds 0 0

main :: IO ()
main = do
    source <- parse <$> getContents
    putStrLn $ "Part 1: " ++ show (part1 source)
    putStrLn $ "Part 2: " ++ show (part2 source)
