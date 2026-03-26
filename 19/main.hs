module Main where

parse :: String -> Int
parse = read

solve :: [Int] -> Int
solve [x] = x
solve x = let x' = map snd $ filter (even . fst) $ zip [0..] x in
    if even $ length x then solve x' else solve $ drop 1 x'

main :: IO ()
main = do
    source <- parse <$> getLine
    putStrLn $ "Part 1: " ++ show (solve [1..source])
