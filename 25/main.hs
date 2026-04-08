module Main where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import Data.Char

data Operand = Data Int | Reg String
    deriving (Show)

data Ins = Cpy Operand Operand 
         | Inc Operand 
         | Dec Operand 
         | Jnz Operand Operand 
         | Out Operand 
         | Nop
         | Add Operand Operand
         | Mul Operand Operand
    deriving (Show)

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy f s =  case dropWhile f s of
    [] -> []
    s' -> w : splitBy f s''
        where (w, s'') = break f s'

parseOperand :: String -> Operand
parseOperand operand | all isAlpha operand = Reg operand
                     | otherwise = Data $ read operand

parseIns :: [String] -> Ins
parseIns ["cpy",a,b] = Cpy (parseOperand a) (parseOperand b)
parseIns ["inc",a] = Inc (parseOperand a)
parseIns ["dec",a] = Dec (parseOperand a)
parseIns ["jnz",a,b] = Jnz (parseOperand a) (parseOperand b)
parseIns ["out",a] = Out (parseOperand a)

parseLine :: String -> Ins
parseLine = parseIns . splitBy isSpace

parse :: String -> [Ins]
parse = map parseLine . filter (/="") . lines

optimisePorgram :: [Ins] -> [Ins]
optimisePorgram program@((Inc (Reg a)):(Dec (Reg b)):(Jnz (Reg b') (Data (-2))):(Dec (Reg c)):(Jnz (Reg c') (Data (-5))):rs)
    | b == b' && c == c' = Mul (Reg c) (Reg b) : Add (Reg b) (Reg a) : Cpy (Data 0) (Reg c) : Cpy (Data 0) (Reg b) : Nop : optimisePorgram rs
    | otherwise = take 1 program ++ optimisePorgram (drop 1 program)
optimisePorgram program@((Inc (Reg a)):(Dec (Reg b)):(Jnz (Reg b') (Data (-2))):rs)
    | b == b' = Add (Reg b) (Reg a) : Cpy (Data 0) (Reg b) : Nop : optimisePorgram rs
    | otherwise = take 1 program ++ optimisePorgram (drop 1 program)
optimisePorgram (i:is) = i : optimisePorgram is
optimisePorgram [] = []


eval :: [Ins] -> (Int, Map.Map String Int, [Int]) -> (Int, Map.Map String Int, [Int])
eval program (pc, reg, out) = case optimisePorgram program !! pc of
    Cpy a b -> (pc + 1, set b $ get a, out)
    Inc a -> (pc + 1, set a $ get a + 1, out)
    Dec a -> (pc + 1, set a $ get a - 1, out)
    Jnz a b -> if get a /= 0 then (pc + get b, reg, out) else (pc + 1, reg, out)
    Out a -> (pc + 1, reg, get a : out)
    Nop -> (pc + 1, reg, out)
    Add a b -> (pc + 1, set b $ get a + get b, out)
    Mul a b -> (pc + 1, set b $ get a * get b, out)
    where
        get (Data x) = x
        get (Reg x) = Maybe.fromMaybe 0 $ Map.lookup x reg
        set (Data x) y = reg
        set (Reg x) y = Map.insert x y reg

loop :: [Ins] -> Set.Set (Int, Map.Map String Int) -> (Int, Map.Map String Int, [Int]) -> (Int, Map.Map String Int, [Int])
loop program visited state@(pc, reg, [0,1]) = loop program visited (pc, reg, [0])
loop program visited state@(pc, reg, [1,0]) = loop program visited (pc, reg, [1])
--loop program visited state@(pc, reg, a:b:_) | (a /= 0 || b /= 1) && (a /= 1 || b /= 0) = state
loop program visited state@(pc, reg, out)
    | out /= [] && out /= [0] && out /= [1] = state
    | Set.member (pc, reg) visited = state
    | pc < length program = loop program (Set.insert (pc, reg) visited) $ eval program state
    | otherwise = state

reg :: Map.Map String Int
reg = Map.fromList [("a", 0), ("b", 0), ("c", 0), ("d", 0)]

solve :: [Ins] -> Int
solve program = fst $ head $ filter ((==1) . length . snd) $ map (\n -> (\(a, b, c) -> (n, c)) $ loop program Set.empty (0, Map.insert "a" n reg, [])) [0..]

main :: IO ()
main = do
    source <- parse <$> getContents
    putStrLn $ "Part 1: " ++ show (solve source)
