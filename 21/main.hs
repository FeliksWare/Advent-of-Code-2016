module Main where

import qualified Data.Sequence as Seq
import Data.Char

import Debug.Trace

data Ins = SwapPosition Int Int | SwapLetter Char Char | RotateLeft Int | RotateRight Int | RotateBase Char | Reverse Int Int | MovePosition Int Int

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

movePosition :: Int -> Int -> Seq.Seq Char -> Seq.Seq Char
movePosition from to s = Seq.insertAt to (Seq.index s from) $ Seq.deleteAt from s 

parseIns :: [String] -> Ins
parseIns ["swap", "position", x, "with", "position", y] = SwapPosition (read x) (read y)
parseIns ["swap", "letter", x, "with", "letter", y] = SwapLetter (head x) (head y)
parseIns ["rotate", "left", x, "steps"] = RotateLeft $ read x
parseIns ["rotate", "right", x, "steps"] = RotateRight $ read x
parseIns ["rotate", "left", "1", "step"] = RotateLeft 1
parseIns ["rotate", "right", "1", "step"] = RotateRight 1
parseIns ["rotate", "based", "on", "position", "of", "letter", x] = RotateBase $ head x
parseIns ["reverse", "positions", x, "through", y] = Reverse (read x) (read y)
parseIns ["move", "position", x, "to", "position", y] = MovePosition (read x) (read y)

parseLine :: String -> Ins
parseLine = parseIns . splitBy isSpace

parse :: String -> [Ins]
parse = map parseLine . filter (/="") . lines

execIns :: Seq.Seq Char -> Ins -> Seq.Seq Char
execIns s (SwapPosition x y) = swapPosition x y s
execIns s (SwapLetter x y) = swapLetter x y s
execIns s (RotateLeft x) = flip (!!) x $ iterate rotateLeft s
execIns s (RotateRight x) = flip (!!) x $ iterate rotateRight s
execIns s (RotateBase x) = flip (!!) (let Just i = Seq.elemIndexL x s in if i >= 4 then i + 2 else i + 1) $ iterate rotateRight s
execIns s (Reverse x y) = reverseSubseq x y s
execIns s (MovePosition x y) = movePosition x y s

undoIns :: Ins -> Seq.Seq Char -> Seq.Seq Char
undoIns (SwapPosition x y) s = swapPosition x y s
undoIns (SwapLetter x y) s = swapLetter x y s
undoIns (RotateLeft x) s = flip (!!) x $ iterate rotateRight s
undoIns (RotateRight x) s = flip (!!) x $ iterate rotateLeft s
undoIns (RotateBase x) s = head $ dropWhile ((/=s) . flip execIns (RotateBase x)) $ iterate rotateLeft s
undoIns (Reverse x y) s = reverseSubseq x y s
undoIns (MovePosition x y) s = movePosition y x s

part1 :: [Ins] -> Seq.Seq Char
part1 = foldl execIns (Seq.fromList "abcdefgh")

part2 :: [Ins] -> Seq.Seq Char
part2 = foldr undoIns (Seq.fromList "fbgdceah")

main :: IO ()
main = do
    source <- parse <$> getContents
    putStrLn $ "Part 1: " ++ show (part1 source)
    putStrLn $ "Part 2: " ++ show (part2 source)
