module Main where

data Direction = NORTH | EAST | SOUTH | WEST
    deriving Show

turn :: Char -> (Direction, Int, Int) -> (Direction, Int, Int)
turn 'R' (NORTH, x, y) = (EAST, x, y)
turn 'R' (EAST, x, y) = (SOUTH, x, y)
turn 'R' (SOUTH, x, y) = (WEST, x, y)
turn 'R' (WEST, x, y) = (NORTH, x, y)
turn 'L' (NORTH, x, y) = (WEST, x, y)
turn 'L' (EAST, x, y) = (NORTH, x, y)
turn 'L' (SOUTH, x, y) = (EAST, x, y)
turn 'L' (WEST, x, y) = (SOUTH, x, y)

move :: Int -> (Direction, Int, Int) -> (Direction, Int, Int)
move d (NORTH, x, y) = (NORTH, x, y + d)
move d (EAST, x, y) = (EAST, x + d, y)
move d (SOUTH, x, y) = (SOUTH, x, y - d)
move d (WEST, x, y) = (WEST, x - d, y)

followInstruction :: (Direction, Int, Int) -> (Char, Int) -> (Direction, Int, Int)
followInstruction s (t, d) = (move d) . (turn t) $ s

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

main :: IO ()
main = do
    x <- getLine
    print $ part1 $ parse x
