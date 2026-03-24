module Main where

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

solve :: Int -> [Disc] -> Int
solve steps discs | all ((==0) . position) discs = steps
solve steps discs = solve (steps + 1) $ map (moveDisc 1) discs

part1 :: [Disc] -> Int
part1 = solve 0 . map (\d -> moveDisc (number d) d)

main :: IO ()
main = do
    source <- parse <$> getContents
    putStrLn $ "Part 1: " ++ show (part1 source)
