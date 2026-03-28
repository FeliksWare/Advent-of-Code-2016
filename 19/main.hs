module Main where

import qualified Data.Sequence as Seq
import qualified Data.Maybe as Maybe

import Debug.Trace

parse :: String -> Int
parse = read

rotate :: Seq.Seq a -> Seq.Seq a
rotate s = Seq.drop 1 s Seq.>< Seq.take 1 s

solve :: (Seq.Seq Int -> Int) -> Seq.Seq Int -> Maybe Int
solve f s | Seq.length s == 1 = s Seq.!? 0
solve f s = solve f $ rotate $ Seq.deleteAt (f s) s

part1 :: Int -> Int
part1 n = Maybe.fromJust $ solve (const 1) $ Seq.fromList [1..n]

part2 :: Int -> Int
part2 n = Maybe.fromJust $ solve (flip div 2 . length) $ Seq.fromList [1..n]

main :: IO ()
main = do
    source <- parse <$> getLine
    putStrLn $ "Part 1: " ++ show (part1 source)
    putStrLn $ "Part 2: " ++ show (part2 source)
