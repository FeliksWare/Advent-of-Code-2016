module Main where

import qualified Data.Map as Map

import Data.Char

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy f s =  case dropWhile f s of
    [] -> []
    s' -> w : splitBy f s''
        where (w, s'') = break f s'

parseNode :: [String] -> ((Int, Int), (Int, Int, Int, Int))
parseNode [x, y, size, used, avail, use] = ((read x, read y), (read size, read used, read avail, read use))

parseLine :: String -> ((Int, Int), (Int, Int, Int, Int))
parseLine = parseNode . splitBy (not . isDigit)

parse :: String -> Map.Map (Int, Int) (Int, Int, Int, Int)
parse = Map.fromList . map parseLine . drop 2 . filter (not . null) . lines

part1 :: Map.Map (Int, Int) (Int, Int, Int, Int) -> Int
part1 m = Map.foldrWithKey (\k (_, used, _, _) b -> Map.foldrWithKey (\k' (_, _, avail, _) b' -> if k /= k' && used /= 0 && used < avail then b' + 1 else b') b m) 0 m

main :: IO ()
main = do
    source <- parse <$> getContents
    putStrLn $ "Part 1: " ++ show (part1 source)
