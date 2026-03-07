module Main where

import Data.List

data Direction = NORTH | EAST | SOUTH | WEST
    deriving (Show, Eq)

turn :: Char -> (Direction, Int, Int) -> (Direction, Int, Int)
turn 'R' (NORTH, x, y) = (EAST, x, y)
turn 'R' (EAST, x, y) = (SOUTH, x, y)
turn 'R' (SOUTH, x, y) = (WEST, x, y)
turn 'R' (WEST, x, y) = (NORTH, x, y)
turn 'L' (NORTH, x, y) = (WEST, x, y)
turn 'L' (EAST, x, y) = (NORTH, x, y)
turn 'L' (SOUTH, x, y) = (EAST, x, y)
turn 'L' (WEST, x, y) = (SOUTH, x, y)
turn _  (d, x, y) = (d, x, y)

move :: Int -> (Direction, Int, Int) -> (Direction, Int, Int)
move d (NORTH, x, y) = (NORTH, x, y + d)
move d (EAST, x, y) = (EAST, x + d, y)
move d (SOUTH, x, y) = (SOUTH, x, y - d)
move d (WEST, x, y) = (WEST, x - d, y)

followInstruction :: (Direction, Int, Int) -> (Char, Int) -> (Direction, Int, Int)
followInstruction s (t, d) = (move d) . (turn t) $ s

followInstructionAll :: (Direction, Int, Int) -> (Char, Int) -> [(Direction, Int, Int)]
followInstructionAll s (t, 0) = []
followInstructionAll s (t, d) = let ns = (move 1) . (turn t) $ s in followInstructionAll ns ('_', d - 1) ++ [ns]

breakOn :: Eq a => [a] -> [a] -> ([a],[a])
breakOn d xs@[] = (xs, xs)
breakOn d xs@(x:xs')
    | take (length d) xs == d   =  ([],xs)
    | otherwise  =  let (ys,zs) = breakOn d xs' in (x:ys,zs)

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn d [] = [[]]
splitOn d s  = let (x, xs) = breakOn d s in
    case xs of
        []        -> [x]
        otherwise -> x : splitOn d (drop (length d) xs)

parseExpr :: String -> (Char, Int)
parseExpr (x:xs) = (x, read xs)

parse :: String -> [(Char, Int)]
parse = map parseExpr . splitOn ", "

part1 :: [(Char, Int)] -> Int
part1 instructions = let (d, x, y) = foldl followInstruction (NORTH, 0, 0) instructions in abs x + abs y

part2 :: [(Char, Int)] -> Int
part2 instructions = let Just (d, x, y) = firstDuplicate [] $ reverse $ foldl followInstructionAppend [(NORTH, 0, 0)] instructions in abs x + abs y
    where
        followInstructionAppend xs@(x:xs') y = (followInstructionAll x y) ++ xs
        firstDuplicate _ [] = Nothing
        firstDuplicate seen ((d, x, y):xs) = case find (\ (d', x', y') -> (x == x') && (y == y')) seen of
            Nothing -> firstDuplicate ((d, x, y):seen) xs
            otherwise -> Just (d, x, y)

main :: IO ()
main = do
    source <- getLine
    putStrLn $ "Part 1: " ++ (show $ part1 $ parse source)
    putStrLn $ "Part 2: " ++ (show $ part2 $ parse source)
