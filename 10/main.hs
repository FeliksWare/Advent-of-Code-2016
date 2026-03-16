module Main where

import qualified Data.Map as Map
import Data.Char

data Entity = Bot Int | Output Int | Void
    deriving (Show, Eq, Ord)

data Instruction = Goes Entity Int | Give Entity Entity Entity
    deriving (Show)

type State = Map.Map Entity [Int]

type Commands = Map.Map Entity (Entity, Entity)

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy f s =  case dropWhile f s of
    [] -> []
    s' -> w : splitBy f s''
        where (w, s'') = break f s'

parseEntity :: String -> Int -> Entity
parseEntity "bot" id = Bot id
parseEntity "output" id = Output id
parseEntity s _ = error s

parseInstruction :: [String] -> Instruction
parseInstruction ["value", value, "goes", "to", eType, eId] =
    Goes (parseEntity eType (read eId)) (read value)
parseInstruction [eType0, eId0, "gives", "low", "to", eType1, eId1, "and", "high", "to", eType2, eId2] =
    Give (parseEntity eType0 (read eId0)) (parseEntity eType1 (read eId1)) (parseEntity eType2 (read eId2))
parseInstruction s = error $ show s

parseLine :: String -> Instruction
parseLine = parseInstruction . splitBy isSpace

parse :: String -> [Instruction]
parse = map parseLine . filter (/="") . lines

initialState :: [Instruction] -> State
initialState = foldl initialState' Map.empty
    where
        initialState' s (Goes e v) = Map.insertWith (++) e [v] s
        initialState' s _ = s

initialCommands :: [Instruction] -> Commands
initialCommands = foldl initialCommands' Map.empty
    where
        initialCommands' c (Give e e' e'') = Map.insert e (e', e'') c
        initialCommands' c _ = c

botsWith2 :: State -> [Entity]
botsWith2 = map fst . filter ((==2) . length . snd) . Map.toList

simulateEntity :: Commands -> Entity -> State -> State
simulateEntity c e s = case Map.lookup e s of
    (Just [l, r]) -> case Map.lookup e c of
        (Just (e', e'')) -> Map.insert e [] $ Map.insertWith (++) e' [min l r] $ Map.insertWith (++) e'' [max l r] $ simulateEntity c e' $ simulateEntity c e'' s
        _ -> s
    _ -> s

simulateStep :: Commands -> State -> State
simulateStep c s = simulateEntity c (head $ botsWith2 s) s

part1'' :: Commands -> State -> [Entity] -> Entity
part1'' c s [] = part1' c $ simulateStep c s
part1'' c s (e:es) = case Map.lookup e s of
    Just [61, 17] -> e
    Just [17, 61] -> e
    _ -> part1'' c s es

part1' :: Commands -> State -> Entity
part1' c s = part1'' c s $ botsWith2 s

part1 :: [Instruction] -> Entity
part1 i = part1' (initialCommands i) (initialState i)

part2' :: Commands -> State -> Int
part2' c s = case [Map.lookup (Output 0) s, Map.lookup (Output 1) s, Map.lookup (Output 2) s] of
    [Just [a], Just [b], Just [c]] -> a * b * c
    _ -> part2' c $ simulateStep c s

part2 :: [Instruction] -> Int
part2 i = part2' (initialCommands i) (initialState i)

main :: IO ()
main = do
    source <- parse <$> getContents
    putStrLn $ "Part 1: " ++ show (part1 source)
    putStrLn $ "Part 2: " ++ show (part2 source)
