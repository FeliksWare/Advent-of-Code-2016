module Main where

import Data.List
import Data.Char

data Disc = Disc {
    number :: Int,
    positions :: Int,
    time :: Int,
    position :: Int
} deriving (Show)

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy f s =  case dropWhile f s of
    [] -> []
    s' -> w : splitBy f s''
        where (w, s'') = break f s'

parseDisc :: [String] -> Disc
parseDisc [number, positions, time, position] = Disc (read number) (read positions) (read time) (read position)

parseLines :: String -> Disc
parseLines = parseDisc . splitBy (not . isDigit)

parse :: String -> [Disc]
parse = map parseLines . filter (/= "") . lines

moveDisc :: Int -> Disc -> Disc
moveDisc n (Disc number positions time position) = Disc number positions (time + n) ((position + n) `mod` positions)

solve :: Int -> Int -> [Disc] -> Int
solve steps n [] = steps
solve steps n discs | any ((==0) . position) discs = let (solved, unsolved) = partition ((==0) . position) discs in solve steps (foldr (lcm . positions) n solved) unsolved
solve steps n discs = solve (steps + n) n $ map (moveDisc n) discs

part1 :: [Disc] -> Int
part1 = solve 0 1 . map (\d -> moveDisc (number d) d)

part2 :: [Disc] -> Int
part2 discs = solve 0 1 $ map (\d -> moveDisc (number d) d) $ discs ++ [Disc (maximum (map number discs) + 1) 11 0 0]

main :: IO ()
main = do
    source <- parse <$> getContents
    putStrLn $ "Part 1: " ++ show (part1 source)
    putStrLn $ "Part 2: " ++ show (part2 source)
