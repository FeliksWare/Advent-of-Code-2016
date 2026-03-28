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

main :: IO ()
main = do
    source <- parse <$> getContents
    putStrLn $ "Part 1: " ++ show (part1 source)
