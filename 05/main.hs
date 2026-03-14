module Main where

import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString as ByteString
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import Data.Word
import Data.Char

type ValidPassword a = a -> Bool
type NexPassword a = a -> ByteString.ByteString -> a

validHash :: ByteString.ByteString -> Bool
validHash hash = hash < ByteString.pack [0, 0, 16]

toHex :: Int -> Char
toHex a | a <= 9 = chr $ ord '0' + a
toHex a | a <= 16 = chr $ ord 'a' + a - 10

validPassword1 :: String -> Bool
validPassword1 s = length s >= 8

nextPassword1 :: String -> ByteString.ByteString -> String
nextPassword1 s h = passwordChar h : s
    where
        passwordChar b = toHex $ mod (fromEnum $ ByteString.head $ ByteString.drop 2 b) 16 

solve :: ValidPassword a -> NexPassword a -> MD5.Ctx -> Int -> a -> a
solve validPassword nextPassword ctx index password | validPassword password = password
solve validPassword nextPassword ctx index password = case MD5.finalize $ MD5.update ctx $ UTF8.fromString $ show index of
    hash | validHash hash -> solve validPassword nextPassword ctx (index + 1) (nextPassword password hash) 
    _ -> solve validPassword nextPassword ctx (index + 1) password

part1 :: String -> String
part1 s = reverse $ solve validPassword1 nextPassword1 (MD5.update MD5.init $ UTF8.fromString s) 0 ""

validPassword2 :: Map.Map Int Char -> Bool
validPassword2 m = Set.fromList [0..7] `Set.isSubsetOf` Map.keysSet m

nextPassword2 :: Map.Map Int Char -> ByteString.ByteString -> Map.Map Int Char
nextPassword2 m b = case passwordIndex b of
    index | (index < 8) && not (Map.member index m) -> Map.insert index (passwordChar b) m
    _ -> m
    where
        passwordIndex b = mod (fromEnum $ ByteString.head $ ByteString.drop 2 b) 16 
        passwordChar b = toHex $ div (fromEnum $ ByteString.head $ ByteString.drop 3 b) 16 

part2 :: String -> String
part2 s = toPassword $ solve validPassword2 nextPassword2 (MD5.update MD5.init $ UTF8.fromString s) 0 Map.empty
    where
        toPassword m = Maybe.mapMaybe (`Map.lookup` m) [0..7]

main :: IO ()
main = do
    source <- getContents
    putStrLn $ "Part 1: " ++ show (part1 source)
    putStrLn $ "Part 2: " ++ show (part2 source)
