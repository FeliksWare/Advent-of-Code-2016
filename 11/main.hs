module Main where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import Data.Char

data Item = Generator String | Microchip String
    deriving (Show, Eq, Ord)

type Building = [Set.Set Item]

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy f s =  case dropWhile f s of
    [] -> []
    s' -> w : splitBy f s''
        where (w, s'') = break f s'

parseList :: [String] -> Set.Set Item
parseList ("a":element:"generator":list) = Set.insert (Generator element) $ parseList list
parseList ("a":element:"compatible":"microchip":list) = Set.insert (Microchip element) $ parseList list
parseList ["and", "a", element, "generator"] = Set.singleton $ Generator element
parseList ["and", "a", element, "compatible", "microchip"] = Set.singleton $ Microchip element
parseList ["nothing", "relevant"] = Set.empty
parseList [] = Set.empty

parseFloorName :: String -> Int
parseFloorName "first" = 1
parseFloorName "second" = 2
parseFloorName "third" = 3
parseFloorName "fourth" = 4

parseFloor :: [String] -> Set.Set Item
parseFloor ("The":floor:"floor":"contains":list) = parseList list

parseLine :: String -> Set.Set Item
parseLine = parseFloor . splitBy (not . isAlpha)

parse :: String -> Building
parse = map parseLine . filter (/="") . lines

isGenerator :: Item -> Bool
isGenerator (Generator _) = True
isGenerator _ = False

isSafe :: Set.Set Item -> Item -> Bool
isSafe s (Microchip e) | Set.member (Generator e) s = True
isSafe s (Microchip _) = not $ any isGenerator s
isSafe s _ = True

isValid :: Set.Set Item -> Bool
isValid s = all (isSafe s) s

isValidState :: (Int, Building) -> Bool
isValidState (_, building) = all isValid building

takeItems :: [Item] -> [Set.Set Item]
takeItems [] = []
takeItems (x:xs) = filter isValid (map (Set.insert x . Set.singleton) xs) ++ takeItems xs ++ [Set.singleton x]

-- horrendous
nextStates :: (Int, Building) -> Set.Set (Int, Building)
nextStates state@(1, [a, b, c, d]) = let items = takeItems $ Set.toList a in
    Set.fromList $ filter isValidState $ map (\e -> (2, [Set.difference a e, Set.union b e, c, d])) items
nextStates state@(2, [a, b, c, d]) = let items = takeItems $ Set.toList b in
    Set.fromList $ filter isValidState $ map (\e -> (3, [a, Set.difference b e, Set.union c e, d])) items ++ map (\e -> (1, [Set.union a e, Set.difference b e, c, d])) items
nextStates state@(3, [a, b, c, d]) = let items = takeItems $ Set.toList c in
    Set.fromList $ filter isValidState $ map (\e -> (4, [a, b, Set.difference c e, Set.union d e])) items ++ map (\e -> (2, [a, Set.union b e, Set.difference c e, d])) items
nextStates state@(4, [a, b, c, d]) = let items = takeItems $ Set.toList d in
    Set.fromList $ filter isValidState $ map (\e -> (3, [a, b, Set.union c e, Set.difference d e])) items

isSolved :: (Int, Building) -> Bool
isSolved (floor, [a, b, c, d]) = null a && null b && null c && not (null d)

solve :: Int -> Set.Set (Int, Building) -> ((Int, Building) -> Bool) -> Set.Set (Int, Building) -> Maybe Int
solve steps visited isSolved states | Set.null states = Nothing
solve steps visited isSolved states | any isSolved states = Just steps
solve steps visited isSolved states = solve (steps + 1) (Set.union visited states) isSolved $ foldr (Set.union . flip Set.difference visited . nextStates) Set.empty states

part1 :: Building -> Maybe Int
part1 building = solve 0 Set.empty (==(1, building)) $ Set.singleton (4, [Set.empty, Set.empty, Set.empty, foldr Set.union Set.empty building])

part2 :: Building -> Maybe Int
part2 (x:xs) = part1 $ Set.union x (Set.fromList [Generator "elerium", Microchip "elerium", Generator "dilithium", Microchip "dilithium"]) : xs

main :: IO ()
main = do
    source <- parse <$> getContents
    putStrLn $ "Part 1: " ++ show (part1 source)
    putStrLn $ "Part 2: " ++ show (part2 source)
