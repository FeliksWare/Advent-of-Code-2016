module Main where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Char

type Grid = Map.Map (Int, Int) Bool

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

executeRect :: Int -> Int -> (Int, Int) -> Bool -> Bool
executeRect w h (x, y) _ | x < w && y < h = True
executeRect _ _ _ b = b

executeRotateRow :: Grid -> Int -> Int -> (Int, Int) -> Bool -> Bool
executeRotateRow m y' n (x, y) _ | y == y' = Maybe.fromJust $ Map.lookup ((x - n) `mod` 50, y) m
executeRotateRow _ _ _ _ b = b

executeRotateColumn :: Grid -> Int -> Int -> (Int, Int) -> Bool -> Bool
executeRotateColumn m x' n (x, y) _ | x == x' =  Maybe.fromJust $ Map.lookup (x, (y - n) `mod` 6) m
executeRotateColumn _ _ _ _ b = b

execute :: Grid -> Instruction -> Grid
execute m (Rect w h) = Map.mapWithKey (executeRect w h) m
execute m (RotateRow y n) = Map.mapWithKey (executeRotateRow m y n) m
execute m (RotateColumn x n) = Map.mapWithKey (executeRotateColumn m x n) m

parse :: String -> [Instruction]
parse = map parseLine . filter (/="") . lines

part1 :: [Instruction] -> Int
part1 = length . Map.filter id . foldl execute (Map.fromList [((x, y), False) | x <- [0..49], y <-[0..5]])

main :: IO ()
main = do
    source <- parse <$> getContents
    putStrLn $ "Part 1: " ++ show (part1 source)
