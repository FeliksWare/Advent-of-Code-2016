module Main where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import Data.Char

import Debug.Trace

data Item = Generator String | Microchip String
    deriving (Show, Eq, Ord)

type Building = Map.Map Int (Set.Set Item)

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

parseFloor :: [String] -> (Int, Set.Set Item)
parseFloor ("The":floor:"floor":"contains":list) = (parseFloorName floor, parseList list)

parseLine :: String -> (Int, Set.Set Item)
parseLine = parseFloor . splitBy (not . isAlpha)

parse :: String -> Building
parse = Map.fromList . map parseLine . filter (/="") . lines

isGenerator :: Item -> Bool
isGenerator (Generator _) = True
isGenerator _ = False

isSafe :: Set.Set Item -> Item -> Bool
isSafe s (Microchip e) | Set.member (Generator e) s = True
isSafe s (Microchip _) = not $ any isGenerator s
isSafe s _ = True

isValid :: Set.Set Item -> Bool
isValid s = all (isSafe s) s

takeItems :: [Item] -> [Set.Set Item]
takeItems [] = []
takeItems (x:xs) = filter isValid (map (Set.insert x . Set.singleton) xs) ++ takeItems xs ++ [Set.singleton x]

elevator :: (Int, Building) -> Int -> Set.Set Item -> Maybe (Int, Building)
elevator (floor, building) floor' items = case (Map.lookup floor building, Map.lookup floor' building) of
    (Just from, Just to) -> let (from', to') = (Set.difference from items, Set.union to items) in
        if isValid from' && isValid to' then Just (floor', Map.insert floor from' $ Map.insert floor' to' building) else Nothing
    _ -> Nothing

nextStates :: (Int, Building) -> Set.Set (Int, Building)
nextStates state@(floor, building) = let items = maybe [] (takeItems . Set.toList) (Map.lookup floor building) in
    Set.fromList $ Maybe.mapMaybe (elevator state $ floor + 1) items ++ Maybe.mapMaybe (elevator state $ floor - 1) items

isSolved :: (Int, Building) -> Bool
isSolved (floor, building) = all (\(k, a) -> if k == 4 then not $ Set.null a else Set.null a) $ Map.assocs building

solve :: Int -> Set.Set (Int, Building) -> Set.Set (Int, Building) -> Maybe Int
solve steps visited states | Set.null states = Nothing
solve steps visited states | any isSolved states = Just steps
solve steps visited states = trace (show $ length visited) $ solve (steps + 1) (Set.union visited states) $ foldr (Set.union . flip Set.difference visited . nextStates) Set.empty states

part1 :: Building -> Maybe Int
part1 building = solve 0 Set.empty $ Set.singleton (1, building)

main :: IO ()
main = do
    source <- parse <$> getContents
    putStrLn $ "Part 1: " ++ show (part1 source)
