module Main where

import Data.Char

split :: (a -> Bool) -> [a] -> ([a], [a])
split _ [] = ([], [])
split p (x:xs')
    | p x = ([],xs')
    | otherwise = let (ys,zs) = split p xs' in (x:ys,zs)

parse :: String -> String
parse = filter (not . isSpace)

part1 :: String -> Int
part1 [] = 0
part1 (x:xs) | x /= '(' = 1 + part1 xs
part1 (_:xs) = length * repeat + part1 (drop length xs')
    where
        (marker, xs') = split (==')') xs
        (length, repeat) = let (a, b) = split (=='x') marker in (read a, read b)

part2 :: String -> Int
part2 [] = 0
part2 (x:xs) | x /= '(' = 1 + part2 xs
part2 (_:xs) = repeat * part2 (take length xs') + part2 (drop length xs')
    where
        (marker, xs') = split (==')') xs
        (length, repeat) = let (a, b) = split (=='x') marker in (read a, read b)

main :: IO ()
main = do
    source <- parse <$> getContents
    putStrLn $ "Part 1: " ++ show (part1 source)
    putStrLn $ "Part 2: " ++ show (part2 source)
