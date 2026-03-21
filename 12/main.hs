module Main where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Char

import Debug.Trace

data Ins = CpyR String String | CpyI Int String | Inc String | Dec String | JnzR String Int | JnzI Int Int
    deriving (Show)

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy f s =  case dropWhile f s of
    [] -> []
    s' -> w : splitBy f s''
        where (w, s'') = break f s'

parseIns :: [String] -> Ins
parseIns ["cpy",v,r] | all isAlpha v = CpyR v r
                     | otherwise = CpyI (read v) r
parseIns ["inc",r] = Inc r
parseIns ["dec",r] = Dec r
parseIns ["jnz",v,v'] | all isAlpha v = JnzR v (read v')
                      | otherwise = JnzI (read v) (read v')

parseLine :: String -> Ins
parseLine = parseIns . splitBy isSpace

parse :: String -> [Ins]
parse = map parseLine . filter (/="") . lines

eval :: [Ins] -> (Int, Map.Map String Int) -> (Int, Map.Map String Int)
eval program (pc, state) | pc < length program = case program !! pc of
    CpyR r r' -> eval program (pc + 1, Map.insert r' (Maybe.fromMaybe 0 $ Map.lookup r state) state)
    CpyI v r -> eval program (pc + 1, Map.insert r v state)
    Inc r -> eval program (pc + 1, Map.insert r (Maybe.fromMaybe 0 (Map.lookup r state) + 1) state)
    Dec r -> eval program (pc + 1, Map.insert r (Maybe.fromMaybe 0 (Map.lookup r state) - 1) state)
    JnzR r v -> if Maybe.fromMaybe 0 (Map.lookup r state) /= 0 then eval program (pc + v, state) else eval program (pc + 1, state)
    JnzI v v' -> if v /= 0 then eval program (pc + v', state) else eval program (pc + 1, state)
eval program (pc, state) = (pc, state)

part1 :: [Ins] -> Int
part1 program = Maybe.fromMaybe 0 $ Map.lookup "a" $ snd $ eval program (0, Map.empty)

main :: IO ()
main = do
    source <- parse <$> getContents
    putStrLn $ "Part 1: " ++ show (part1 source)
