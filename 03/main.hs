module Main where

readInt :: String -> Int
readInt = read

parse :: String -> [(Int, Int, Int)]
parse = map ((\(a:b:c:_) -> (a, b, c)) . map readInt . words) . lines

validTriangle :: (Int, Int, Int) -> Bool
validTriangle (a, b, c) = a + b > c && b + c > a && a + c > b

part1 :: [(Int, Int, Int)] -> Int
part1 = length . filter validTriangle

main :: IO ()
main = do
    source <- parse <$> getContents
    putStrLn $ "Part 1: " ++ (show $ part1 source)
