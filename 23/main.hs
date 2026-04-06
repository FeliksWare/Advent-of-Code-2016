module Main where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Char

import Debug.Trace

data Operand = Data Int | Reg String
    deriving (Show)

data Ins = Cpy Operand Operand 
         | Inc Operand 
         | Dec Operand 
         | Jnz Operand Operand 
         | Tgl Operand
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

toggleProgram :: [(Ins, Bool)] -> Int -> [(Ins, Bool)]
toggleProgram ((ins, toggled):rs) 0 = (ins, not toggled) : rs
toggleProgram (r:rs) i = r : toggleProgram rs (i - 1)
toggleProgram [] _ = []

eval :: [(Ins, Bool)] -> (Int, Map.Map String Int) -> (Int, Map.Map String Int)
eval program (pc, reg) | pc < length program = case let (ins, toggled) = program !! pc in if toggled then toggleIns ins else ins of
    Cpy a b -> eval program (pc + 1, set b $ get a)
    Inc a -> eval program (pc + 1, set a $ get a + 1)
    Dec a -> eval program (pc + 1, set a $ get a - 1)
    Jnz a b -> if get a /= 0 then eval program (pc + get b, reg) else eval program (pc + 1, reg)
    Tgl a -> eval (toggleProgram program $ pc + get a) (pc + 1, reg)
    where
        get (Data x) = x
        get (Reg x) = Maybe.fromMaybe 0 $ Map.lookup x reg
        set (Data x) y = reg
        set (Reg x) y = Map.insert x y reg
eval program (pc, reg) = (pc, reg)

part1 :: [Ins] -> Int
part1 program = Maybe.fromMaybe 0 $ Map.lookup "a" $ snd $ eval (zip program $ repeat False) (0, Map.singleton "a" 7)

main :: IO ()
main = do
    source <- parse <$> getContents
    putStrLn $ "Part 1: " ++ show (part1 source)
