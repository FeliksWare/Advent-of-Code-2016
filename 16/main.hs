module Main where

randomData :: Int -> String -> String
randomData n s | n <= length s = take n s
randomData n s = randomData n $ s ++ "0" ++ map swap (reverse s)
    where
        swap '0' = '1'
        swap '1' = '0'

checksum :: String -> String
checksum s = let s' = checksum' s in if even $ length s' then checksum s' else s'
    where
        checksum' (a:b:s) = (if a == b then '1' else '0') : checksum' s
        checksum' [] = []

part1 :: String -> String
part1 = checksum . randomData 272

main :: IO ()
main = do
    source <- getContents
    putStrLn $ "Part 1: " ++ part1 source
