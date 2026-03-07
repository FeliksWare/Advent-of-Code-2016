module Main where

move :: (Int, Int) -> Char -> (Int, Int)
move (x, y) 'U' | y > 0 = (x, y - 1)
move (x, y) 'R' | x < 2 = (x + 1, y)
move (x, y) 'D' | y < 2 = (x, y + 1)
move (x, y) 'L' | x > 0 = (x - 1, y)
move (x, y) _   = (max 0 $ min 2 x, max 0 $ min 2 y)

keypadDigit :: (Int, Int) -> Int
keypadDigit (x, y) = 3*y + x + 1

part1 :: [String] -> String
part1 = foldr (++) "" . map (show . keypadDigit . foldl move (1, 1))

main :: IO ()
main = do
    source <- lines <$> getContents
    putStrLn $ "Part 1: " ++ (show $ part1 source)
