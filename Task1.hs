module Task1 where

import System.IO
import Data.List

main :: IO ()
main = do
    handle <- openFile "test1.txt" ReadMode
    contents <- hGetContents handle
    --print $ lines contents
    let parsed = parseLines $ lines contents
        
        res = sum $ take 3 $ reverse . sort $ map sum parsed
    print res
    hClose handle

parseLines :: [String] -> [[Int]]
parseLines strs =  helper strs [] [] where
    helper :: [String] -> [[Int]] -> [Int] -> [[Int]]
    helper [] res curr = curr : res
    helper (x:xs) res curr | x == "" = helper xs (curr : res) []
                           | otherwise = helper xs res ((read x::Int) : curr)

