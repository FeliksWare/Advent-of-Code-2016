module Main where

readInt :: String -> Int
readInt = read

parse1 :: String -> [(Int, Int, Int)]
parse1 = map (parseExpr . map readInt . words) . filter (/="") . lines
    where parseExpr (a:b:c:_) = (a, b, c)

parse2 :: String -> [(Int, Int, Int)]
parse2 = parseExpr . map (map readInt . words) . filter (/="") . lines
    where
        parseExpr [] = []
        parseExpr ((a:b:c:_):(d:e:f:_):(g:h:i:_):r) = [(a, d, g), (b, e, h), (c, f, i)] ++ parseExpr r

validTriangle :: (Int, Int, Int) -> Bool
validTriangle (a, b, c) = a + b > c && b + c > a && a + c > b

solve :: [(Int, Int, Int)] -> Int
solve = length . filter validTriangle

main :: IO ()
main = do
    source <- getContents
    putStrLn $ "Part 1: " ++ (show $ solve $ parse1 source)
    putStrLn $ "Part 2: " ++ (show $ solve $ parse2 source)
