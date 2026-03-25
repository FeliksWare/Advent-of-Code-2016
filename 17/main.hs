module Main where

import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString as ByteString
import qualified Crypto.Hash.MD5 as MD5

import Debug.Trace

openDoors :: String -> String -> [Char]
openDoors passcode path = openDoors' $ ByteString.unpack $ MD5.finalize $ MD5.update (MD5.update MD5.init $ UTF8.fromString passcode) $ UTF8.fromString path
    where openDoors' (a:b:_) = ['U' | a `div` 16 > 10] ++ ['D' | a `mod` 16 > 10] ++ ['L' | b `div` 16 > 10] ++ ['R' | b `mod` 16 > 10]

nextCoordinate :: (Int, Int) -> Char -> (Int, Int)
nextCoordinate (x, y) 'U' = (x, y - 1)
nextCoordinate (x, y) 'D' = (x, y + 1)
nextCoordinate (x, y) 'L' = (x - 1, y)
nextCoordinate (x, y) 'R' = (x + 1, y)

nextPosition :: ((Int, Int), String) -> Char -> ((Int, Int), String)
nextPosition (coordinate, path) direction = (nextCoordinate coordinate direction, path ++ [direction])

validPosition :: ((Int, Int), String) -> Bool
validPosition ((x, y), _) = 1 <= x && x <= 4 && 1 <= y && y <= 4

nextPositions :: String -> ((Int, Int), String) -> [((Int, Int), String)]
nextPositions passcode position@(coordinate, path) = filter validPosition $ map (nextPosition position) $ openDoors passcode path

solveShortest :: String -> (Int, Int) -> [((Int, Int), String)] -> String
solveShortest passcode destination (position@(coordinate, path):positions) | coordinate == destination = path
                                                                           | otherwise = solveShortest passcode destination $ positions ++ nextPositions passcode position

part1 :: String -> String
part1 passcode = solveShortest passcode (4, 4) [((1, 1), "")]

solveLongest :: String -> (Int, Int) -> ((Int, Int), String) -> Int
solveLongest passcode destination position@(coordinate, path) | coordinate == destination = length path
                                                              | otherwise = foldr (max . solveLongest passcode destination) 0 $ nextPositions passcode position

part2 :: String -> Int
part2 passcode = solveLongest passcode (4, 4) ((1, 1), "")

main :: IO ()
main = do
    source <- getContents
    putStrLn $ "Part 1: " ++ show (part1 source)
    putStrLn $ "Part 2: " ++ show (part2 source)
