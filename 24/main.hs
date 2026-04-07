module Main where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Char

import Debug.Trace

type Tiles = Set.Set (Int, Int)

parseLine :: (Int, String) -> [((Int, Int), Char)]
parseLine (y, s) = zip (map (, y) [0..]) s

parse :: String -> Map.Map (Int, Int) Char
parse = Map.fromList . concatMap parseLine . zip [0..] . filter (/="") . lines

toGrid :: Map.Map (Int, Int) Char -> Tiles
toGrid = Map.keysSet . Map.filter (/='#')

toGoals :: Map.Map (Int, Int) Char -> Tiles
toGoals = Map.keysSet . Map.filter (/='0') . Map.filter isDigit

toStart :: Map.Map (Int, Int) Char -> (Int, Int)
toStart = head . Map.keys . Map.filter (=='0')

adjacent :: (Int, Int) -> [(Int, Int)]
adjacent (x, y) = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

solve :: Tiles -> Int -> Set.Set ((Int, Int), Tiles) -> Set.Set ((Int, Int), Tiles) -> Int
solve grid steps visited current | any (Set.null . snd) current = steps
solve grid steps visited current = solve grid (steps + 1) (Set.union visited current) $ Set.difference nextStates visited
    where
        nextStates = Set.foldr (Set.union . Set.fromList . \(pos, goals) -> map (\pos' -> (pos', Set.delete pos' goals)) $ filter (`Set.member` grid) $ adjacent pos) Set.empty current

part1 :: Map.Map (Int, Int) Char -> Int
part1 map = solve (toGrid map) 0 Set.empty (Set.singleton (toStart map, toGoals map))

main :: IO ()
main = do
    source <- parse <$> getContents
    putStrLn $ "Part 1: " ++ show (part1 source)
