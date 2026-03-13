module Main where

import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString as ByteString
import qualified Crypto.Hash.MD5 as MD5
import Data.Word
import Data.Char
import qualified GHC.Base as MD5

validHash :: ByteString.ByteString -> Bool
validHash hash = hash < ByteString.pack [0, 0, 16]

passwordChar :: ByteString.ByteString -> Char
passwordChar b = case mod (fromEnum $ ByteString.head $ ByteString.drop 2 b) 16 of
    c | c < 10 -> chr (c + ord '0')
    c -> chr (c - 10 + ord 'a')

solve :: MD5.Ctx -> String -> Int -> String
solve ctx password index | length password >= 8 = reverse password
solve ctx password index = case MD5.finalize $ MD5.update ctx $ UTF8.fromString $ show index of
    hash | validHash hash -> solve ctx (passwordChar hash : password) (index + 1)
    _ -> solve ctx password (index + 1)

part1 :: String -> String
part1 s = solve (MD5.update MD5.init $ UTF8.fromString s) [] 0

main :: IO ()
main = do
    source <- getContents
    putStrLn $ "Part 1: " ++ show (part1 source)
