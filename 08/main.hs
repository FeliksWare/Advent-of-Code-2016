module Main where

import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import Data.Char

type Grid = Set.Set (Int, Int)

gridWidth :: Int
gridWidth = 50

gridHeight :: Int
gridHeight = 6

data Instruction = Rect Int Int | RotateRow Int Int | RotateColumn Int Int
    deriving (Show)

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy f s =  case dropWhile f s of
    [] -> []
    s' -> w : splitBy f s''
        where (w, s'') = break f s'

parseInstruction :: (Int -> Int -> Instruction) -> String -> Instruction
parseInstruction f s = let [a, b] = splitBy (not . isDigit) s in f (read a) (read b)

parseLine :: String -> Instruction
parseLine s | "rect " `List.isPrefixOf` s = parseInstruction Rect s
parseLine s | "rotate row y=" `List.isPrefixOf` s = parseInstruction RotateRow s
parseLine s | "rotate column x=" `List.isPrefixOf` s = parseInstruction RotateColumn s

executeRect :: Int -> Int -> Grid -> Grid
executeRect w h s = foldl insert s [(x, y) | x <- [0..w-1], y <- [0..h-1]]
    where
        insert s' k = Set.insert k s'

executeRotateRow :: Int -> Int -> Grid -> Grid
executeRotateRow y n m = foldl insert m [0..gridWidth-1]
    where
        insert m' x | Set.member ((x - n) `mod` gridWidth, y) m = Set.insert (x, y) m'
        insert m' x = Set.delete (x, y) m'

executeRotateColumn :: Int -> Int -> Grid -> Grid
executeRotateColumn x n m = foldl insert m [0..gridHeight-1]
    where
        insert m' y | Set.member (x, (y - n) `mod` gridHeight) m = Set.insert (x, y) m'
        insert m' y = Set.delete (x, y) m'

execute ::Grid -> Instruction -> Grid
execute m (Rect w h) = executeRect w h m
execute m (RotateRow y n) = executeRotateRow y n m
execute m (RotateColumn x n) = executeRotateColumn x n m

parse :: String -> [Instruction]
parse = map parseLine . filter (/="") . lines

solve :: [Instruction] -> Grid
solve = foldl execute Set.empty

part1 :: [Instruction] -> Int
part1 = length . solve

part2 :: [Instruction] -> String
part2 instructions = let grid = solve instructions in
    unlines $ [[displayChar $ Set.member (x, y) grid | x <- [0..gridWidth-1]] | y <- [0..gridHeight-1]]
        where
            displayChar True = '#'
            displayChar False = ' '

main :: IO ()
main = do
    source <- parse <$> getContents
    putStrLn $ "Part 1: " ++ show (part1 source)
    putStrLn "Part 2: "
    putStrLn $ part2 source
