module Main where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Char

data Operand = Data Int | Reg String
    deriving (Show)

data Ins = Cpy Operand Operand 
         | Inc Operand 
         | Dec Operand 
         | Jnz Operand Operand 
         | Tgl Operand
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
parseIns ["tgl",a] = Tgl (parseOperand a)

parseLine :: String -> Ins
parseLine = parseIns . splitBy isSpace

parse :: String -> [Ins]
parse = map parseLine . filter (/="") . lines

toggleIns :: Ins -> Ins
toggleIns (Cpy a b) = Jnz a b
toggleIns (Inc a) = Dec a
toggleIns (Dec a) = Inc a
toggleIns (Jnz a b) = Cpy a b
toggleIns (Tgl a) = Inc a

toggleProgramAt :: [(Ins, Bool)] -> Int -> [(Ins, Bool)]
toggleProgramAt ((ins, toggled):rs) 0 = (ins, not toggled) : rs
toggleProgramAt (r:rs) i = r : toggleProgramAt rs (i - 1)
toggleProgramAt [] _ = []

toggleProgram :: [(Ins, Bool)] -> [Ins]
toggleProgram = map (\(ins, toggled) -> if toggled then toggleIns ins else ins)

optimisePorgram :: [Ins] -> [Ins]
optimisePorgram program@((Inc (Reg a)):(Dec (Reg b)):(Jnz (Reg b') (Data (-2))):(Dec (Reg c)):(Jnz (Reg c') (Data (-5))):rs)
    | b == b' && c == c' = Mul (Reg c) (Reg b) : Add (Reg b) (Reg a) : Cpy (Data 0) (Reg c) : Cpy (Data 0) (Reg b) : Nop : optimisePorgram rs
    | otherwise = take 1 program ++ optimisePorgram (drop 1 program)
optimisePorgram program@((Inc (Reg a)):(Dec (Reg b)):(Jnz (Reg b') (Data (-2))):rs)
    | b == b' = Add (Reg b) (Reg a) : Cpy (Data 0) (Reg b) : Nop : optimisePorgram rs
    | otherwise = take 1 program ++ optimisePorgram (drop 1 program)
optimisePorgram (i:is) = i : optimisePorgram is
optimisePorgram [] = []


eval :: (Int, [(Ins, Bool)], Map.Map String Int) -> (Int, [(Ins, Bool)], Map.Map String Int)
eval (pc, program, reg) | pc < length program = case optimisePorgram (toggleProgram program) !! pc of --case let (ins, toggled) = program !! pc in if toggled then toggleIns ins else ins of
    Cpy a b -> (pc + 1, program, set b $ get a)
    Inc a -> (pc + 1, program, set a $ get a + 1)
    Dec a -> (pc + 1, program, set a $ get a - 1)
    Jnz a b -> if get a /= 0 then (pc + get b, program, reg) else (pc + 1, program, reg)
    Tgl a -> (pc + 1, toggleProgramAt program $ pc + get a, reg)
    Nop -> (pc + 1, program, reg)
    Add a b -> (pc + 1, program, set b $ get a + get b)
    Mul a b -> (pc + 1, program, set b $ get a * get b)
    where
        get (Data x) = x
        get (Reg x) = Maybe.fromMaybe 0 $ Map.lookup x reg
        set (Data x) y = reg
        set (Reg x) y = Map.insert x y reg
eval (pc, program, reg) = (pc, program, reg)

loop :: (Int, [(Ins, Bool)], Map.Map String Int) -> (Int, [(Ins, Bool)], Map.Map String Int)
loop state@(pc, program, reg) | pc < length program = loop $ eval state
                              | otherwise = state

solve :: [Ins] -> Int -> Int
solve program n = Maybe.fromMaybe 0 $ Map.lookup "a" $ (\(pc, program, reg) -> reg) $ loop (0, zip program $ repeat False, Map.singleton "a" n)

part1 :: [Ins] -> Int
part1 program = solve program 7

part2 :: [Ins] -> Int
part2 program = solve program 12

main :: IO ()
main = do
    source <- parse <$> getContents
    putStrLn $ "Part 1: " ++ show (part1 source)
    putStrLn $ "Part 2: " ++ show (part2 source)
