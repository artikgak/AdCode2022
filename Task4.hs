module Task4 where

import System.IO
import Data.Char (isLower, isUpper)
import Data.List
import Data.Function
import Data.List.Split

main :: IO ()
main = do
    --handle <- openFile "test1.txt" ReadMode
    handle <- openFile "input.a64rpt.txt" ReadMode
    contents <- hGetContents handle
    let parsed = map parseLines $ lines contents
        --part1
        --res =  length $ filter (\(x,y) -> isInfixOf x y || isInfixOf y x ) parsed
        --part2
        res =  length $ filter (\(x,y) -> 0 < (length $ intersect x y)) parsed
    print res
    hClose handle

--parseLines :: String -> [Char]
parseLines str = (one, sec) where
    twoParts = splitByChar str ','
    oneTwoParts = splitByChar (head twoParts) '-'
    twoTwoParts = splitByChar (last twoParts) '-'
    one = [read (head oneTwoParts)::Int .. read (last oneTwoParts)::Int]
    sec = [read (head twoTwoParts)::Int .. read (last twoTwoParts)::Int]

splitByChar :: String -> Char -> [String]
splitByChar str ch = helper str ch [] [] where
    helper [] _ temp res = if null temp then res else res ++[temp]
    helper (x:xs) ch temp res | x==ch = helper xs ch [] (res++[temp])
                              | otherwise = helper xs ch (temp++[x]) res