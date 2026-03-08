module Main where

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Maybe as Maybe

import Data.Char

extractName (name, _) = name
extractId (_, (id, _)) = id

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy f s =  case dropWhile f s of
    [] -> []
    s' -> w : splitBy f s''
        where (w, s'') = break f s'

wordsBy :: Char -> String -> [String]
wordsBy c = splitBy (==c)

parse :: String -> [([String], (Int, String))]
parse = map (parseExpr . wordsBy '-') . filter (/="") . lines
    where
        parseExpr s = (init s, parseIdAndChecksum $ splitIdAndChecksum $ last s)
        splitIdAndChecksum = splitBy (\x -> x == '[' || x == ']')
        parseIdAndChecksum (a:b:_) = (read a, b)

isReal :: ([String], (Int, String)) -> Bool
isReal (name, (_, checksum)) = (map extractChar $ take 5 $ List.sortBy compareFrequences $ charFrequence $ concat name) == checksum
    where
        charFrequence s = Map.toList $ Map.fromListWith (+) [(c, 1) | c <- s]
        compareFrequences (c1, f1) (c2, f2) = case compare f2 f1 of
            EQ -> compare c1 c2
            a -> a
        extractChar (c, _) = c

part1 :: [([String], (Int, String))] -> Int
part1 = sum . map extractId . filter isReal

decrypt :: Int -> String -> String
decrypt key = map (shiftN key)
    where
        shiftN n c | isAlpha c && isLower c = chr $ mod (ord c - ord 'a' + n) 26 + ord 'a' 
        shiftN _ c = c

part2 :: [([String], (Int, String))] -> Int
part2 = extractId . findRoom . map decryptName . filter isReal
    where
        decryptName (name, (id, checksum)) = (decrypt id $ unwords name, (id, checksum))
        findRoom = Maybe.fromJust . List.find ((=="northpole object storage") . extractName)

main :: IO ()
main = do
    source <- getContents
    putStrLn $ "Part 1: " ++ (show $ part1 $ parse source)
    putStrLn $ "Part 2: " ++ (show $ part2 $ parse source)
