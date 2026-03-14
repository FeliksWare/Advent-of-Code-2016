module Main where
import Text.XHtml (sup)

parseExprL :: String -> ([String], [String]) -> ([String], [String])
parseExprL [] t = t 
parseExprL (x:xs) (l, r) | x == '[' = parseExprR xs (l, [] : r)
parseExprL (x:xs) ([], r) = parseExprL xs ([[x]], r)
parseExprL (x:xs) (l:ls, r) = parseExprL xs ((x : l) : ls, r)

parseExprR :: String -> ([String], [String]) -> ([String], [String])
parseExprR [] t = t 
parseExprR (x:xs) (l, r) | x == ']' = parseExprL xs ([] : l, r)
parseExprR (x:xs) (l, []) = parseExprR xs (l, [[x]])
parseExprR (x:xs) (l, r:rs) = parseExprR xs (l, (x : r) : rs)

parseExpr :: String -> ([String], [String])
parseExpr s = parseExprL s ([], [])

parse :: String -> [([String], [String])]
parse = map parseExpr . filter (/="") . lines

hasABBA' :: String -> Bool
hasABBA' [a, b, c, d] = a /= b && a == d && b == c
hasABBA' _ = False

hasABBA :: String -> Bool
hasABBA s | hasABBA' $ take 4 s = True
hasABBA [] = False
hasABBA s = hasABBA $ drop 1 s

supportsTLS :: ([String], [String]) -> Bool
supportsTLS (l, r) = any hasABBA l && not (any hasABBA r)

part1 :: [([String], [String])] -> Int
part1 = length . filter supportsTLS

main :: IO ()
main = do
    source <- parse <$> getContents
    putStrLn $ "Part 1: " ++ show (part1 source)
