module Main where

import qualified Data.Set as Set
import Debug.Trace

readInt :: String -> Int
readInt = read

countBits :: Int -> Int
countBits 0 = 0
countBits n = mod n 2 + countBits (div n 2)

isOpen :: Int -> (Int, Int) -> Bool
isOpen n (x, y) | x < 0 || y < 0 = False
isOpen n (x, y) = even $ countBits (x*x + 3*x + 2*x*y + y + y*y + n)

adjacent :: (Int, Int) -> [(Int, Int)]
adjacent (x, y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)] 

solve :: Int -> ((Int, Int) -> Bool) -> (Int, Int) -> Set.Set (Int, Int) -> Int
solve steps isOpen destination current | Set.member destination current = steps
solve steps isOpen destination current = solve (steps + 1) isOpen destination $ Set.foldr (Set.union . Set.fromList . filter isOpen . adjacent) current current

part1 :: Int -> Int
part1 n = solve 0 (isOpen n) (31, 39) $ Set.singleton (1, 1)

main :: IO ()
main = do
    source <- readInt <$> getContents
    putStrLn $ "Part 1: " ++ show (part1 source)
