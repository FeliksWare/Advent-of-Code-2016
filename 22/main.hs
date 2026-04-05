module Main where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Char

import Debug.Trace

type Grid = Map.Map (Int, Int) (Int, Int, Int)

type Nodes = Set.Set (Int, Int)

type State = ((Int, Int), (Int, Int))

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy f s =  case dropWhile f s of
    [] -> []
    s' -> w : splitBy f s''
        where (w, s'') = break f s'

parseNode :: [String] -> ((Int, Int), (Int, Int, Int))
parseNode [x, y, size, used, avail, use] = ((read x, read y), (read size, read used, read avail))

parseLine :: String -> ((Int, Int), (Int, Int, Int))
parseLine = parseNode . splitBy (not . isDigit)

parse :: String -> Grid
parse = Map.fromList . map parseLine . drop 2 . filter (not . null) . lines

part1 :: Grid -> Int
part1 grid = Map.foldrWithKey (\k (_, used, _) b -> Map.foldrWithKey (\k' (_, _, avail) b' -> if k /= k' && used /= 0 && used <= avail then b' + 1 else b') b grid) 0 grid

initialState :: Grid -> State
initialState grid = (fst $ head $ Map.toList $ Map.filter (\(_, used, _) -> used == 0) grid, maximum $ filter ((==0) . snd)$ Map.keys grid)

initialNodes :: Grid -> Nodes
initialNodes grid = Map.keysSet $ Map.filter (\(size, used, avail) -> any (\(size', used', avail') -> used <= avail') grid) grid

adjacent :: (Int, Int) -> [(Int, Int)]
adjacent (x, y) = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

nextStates :: Nodes -> State -> [State]
nextStates nodes (empty, goal) = map (\pos -> if pos == goal then (pos, empty) else (pos, goal)) $ filter (`Set.member` nodes) $ adjacent empty

solve :: Nodes -> Int -> Set.Set State -> Set.Set State -> Int
solve nodes steps visited current | any ((==(0,0)) . snd) current = steps 
solve nodes steps visited current =
    solve nodes (steps + 1) (Set.union visited current) $ Set.difference (Set.foldr (Set.union . Set.fromList . nextStates nodes) Set.empty current) visited

part2 :: Grid -> Int
part2 grid = solve (initialNodes grid) 0 Set.empty (Set.singleton $ initialState grid)

main :: IO ()
main = do
    source <- parse <$> getContents
    putStrLn $ "Part 1: " ++ show (part1 source)
    putStrLn $ "Part 2: " ++ show (part2 source)
