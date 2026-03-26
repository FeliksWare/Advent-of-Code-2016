module Main where

adjacent :: String -> [(Char, Char, Char)]
adjacent s = zip3 ("." ++ init s) s (tail s ++ ".")

newTile :: (Char, Char, Char) -> Char
newTile ('^', '^', '.') = '^'
newTile ('.', '^', '^') = '^'
newTile ('^', '.', '.') = '^'
newTile ('.', '.', '^') = '^'
newTile _ = '.'

nextLine :: String -> String
nextLine = map newTile . adjacent

countSafeTiles :: String -> Int
countSafeTiles = length . filter (=='.')

buildFloor :: Int -> String -> [String]
buildFloor count line | count <= 1 = [line]
                      | otherwise = line : buildFloor (count - 1) (nextLine line)

part1 :: String -> Int
part1 = foldr ((+) . countSafeTiles) 0 . buildFloor 40

part2 :: String -> Int
part2 = foldr ((+) . countSafeTiles) 0 . buildFloor 400000

main :: IO ()
main = do
    source <- getLine
    putStrLn $ "Part 1: " ++ show (part1 source)
    putStrLn $ "Part 2: " ++ show (part2 source)
