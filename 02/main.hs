module Main where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe

type Keypad = Map.Map (Int, Int) Char

normalKeypad :: Keypad
normalKeypad = Map.fromList [
    ((0, 2), '1'), ((1, 2), '2'), ((2, 2), '3'), 
    ((0, 1), '4'), ((1, 1), '5'), ((2, 1), '6'), 
    ((0, 0), '7'), ((1, 0), '8'), ((2, 0), '9') 
    ]

diamondKeypad :: Keypad
diamondKeypad = Map.fromList [
                                  ((2, 4), '1'),                               
                   ((1, 3), '2'), ((2, 3), '3'), ((3, 3), '4'),                
    ((0, 2), '5'), ((1, 2), '6'), ((2, 2), '7'), ((3, 2), '8'), ((4, 2), '9'), 
                   ((1, 1), 'A'), ((2, 1), 'B'), ((3, 1), 'C'),                
                                  ((2, 0), 'D')                               
    ]

isValid :: Keypad -> (Int, Int) -> Bool
isValid keypad position = Set.member position $ Map.keysSet keypad

offset :: (Int, Int) -> Char -> (Int, Int)
offset (x, y) 'U' = (x, y + 1)
offset (x, y) 'R' = (x + 1, y)
offset (x, y) 'D' = (x, y - 1)
offset (x, y) 'L' = (x - 1, y)

move :: Keypad -> (Int, Int) -> Char -> (Int, Int)
move keypad position c = let n = offset position c in if isValid keypad n then n else position

keypadDigit :: Keypad -> (Int, Int) -> Char
keypadDigit keypad position = Maybe.fromJust $ Map.lookup position keypad

solve :: Keypad -> (Int, Int) -> [String] -> String
solve keypad position = foldr ((:) . keypadDigit keypad . foldl (move keypad) position) ""


part1 :: [String] -> String
part1 = solve normalKeypad (1, 1)

part2 :: [String] -> String
part2 = solve diamondKeypad (0, 2)

main :: IO ()
main = do
    source <- lines <$> getContents
    putStrLn $ "Part 1: " ++ show (part1 source)
    putStrLn $ "Part 2: " ++ show (part2 source)
