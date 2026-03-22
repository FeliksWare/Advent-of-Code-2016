module Main where

import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString as ByteString
import qualified Crypto.Hash.MD5 as MD5
import Data.Char
import Data.Word

import Debug.Trace

findTuples :: Eq a => Int -> [a] -> [a]
findTuples n [] = []
findTuples n x | length x < n = []
findTuples n (x:xs) | all (==x) $ take (n-1) xs = x : findTuples n xs
findTuples n (x:xs) = findTuples n xs

parse :: String -> MD5.Ctx
parse = MD5.update MD5.init . UTF8.fromString

generateKey :: MD5.Ctx -> Int -> [Word8]
generateKey ctx index = concatMap (\w -> [w `div` 16, w `mod` 16]) $ ByteString.unpack $ MD5.finalize $ MD5.update ctx $ UTF8.fromString $ show index

keyList :: MD5.Ctx -> [[Word8]]
keyList ctx = [generateKey ctx index | index <- [0..]]

tupleList :: MD5.Ctx -> [([Word8], [Word8])]
tupleList ctx = [(findTuples 3 key, findTuples 5 key) | key <- keyList ctx]

solve :: Int -> Int -> [([Word8], [Word8])] -> Int
solve count index _ | count >= 64 = index - 1
solve count index ((key:_,_):keys) | any (elem key . snd) $ take 999 keys = solve (count + 1) (index + 1) keys
solve count index (_:keys) = solve count (index + 1) keys

part1 :: MD5.Ctx -> Int
part1 ctx = solve 0 0 (tupleList ctx)

main :: IO ()
main = do
    source <- parse <$> getContents
    putStrLn $ "Part 1: " ++ show (part1 source)
