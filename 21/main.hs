module Main where

import qualified Data.Sequence as Seq
import Data.Char

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy f s =  case dropWhile f s of
    [] -> []
    s' -> w : splitBy f s''
        where (w, s'') = break f s'

swapPosition :: Int -> Int -> Seq.Seq Char -> Seq.Seq Char
swapPosition i j s = Seq.update i (Seq.index s j) $ Seq.update j (Seq.index s i) s

swapLetter :: Char -> Char -> Seq.Seq Char -> Seq.Seq Char
swapLetter n m = fmap swapLetter'
    where
        swapLetter' a | a == n = m
        swapLetter' a | a == m = n
        swapLetter' a = a

rotateLeft :: Seq.Seq Char -> Seq.Seq Char
rotateLeft = uncurry (flip (Seq.><)) . Seq.splitAt 1

rotateRight :: Seq.Seq Char -> Seq.Seq Char
rotateRight s = uncurry (flip (Seq.><)) $ Seq.splitAt (length s - 1) s

reverseSubseq :: Int -> Int -> Seq.Seq Char -> Seq.Seq Char
reverseSubseq l r s | r < l = s
reverseSubseq l r s = reverseSubseq (l + 1) (r - 1) $ swapPosition l r s

move :: Int -> Int -> Seq.Seq Char -> Seq.Seq Char
move from to s = Seq.insertAt to (Seq.index s from) $ Seq.deleteAt from s 

parseIns :: [String] -> (Seq.Seq Char -> Seq.Seq Char)
parseIns ["swap", "position", x, "with", "position", y] = swapPosition (read x) (read y)
parseIns ["swap", "letter", x, "with", "letter", y] = swapLetter (head x) (head y)
parseIns ["rotate", "left", x, "steps"] = flip (!!) (read x) . iterate rotateLeft
parseIns ["rotate", "right", x, "steps"] = flip (!!) (read x) . iterate rotateRight
parseIns ["rotate", "left", "1", "step"] = rotateLeft
parseIns ["rotate", "right", "1", "step"] = rotateRight
parseIns ["rotate", "based", "on", "position", "of", "letter", x] =
    \s -> flip (!!) (let Just i = Seq.elemIndexL (head x) s in if i >= 4 then i + 2 else i + 1) $ iterate rotateRight s
parseIns ["reverse", "positions", x, "through", y] = reverseSubseq (read x) (read y)
parseIns ["move", "position", x, "to", "position", y] = move (read x) (read y)

parseLine :: String -> (Seq.Seq Char -> Seq.Seq Char)
parseLine = parseIns . splitBy isSpace

parse :: String -> [Seq.Seq Char -> Seq.Seq Char]
parse = map parseLine . filter (/="") . lines

part1 :: [Seq.Seq Char -> Seq.Seq Char] -> Seq.Seq Char
part1 = foldl (\a b -> b a) (Seq.fromList "abcdefgh")

main :: IO ()
main = do
    source <- parse <$> getContents
    putStrLn $ "Part 1: " ++ show (part1 source)
