module Task3 where

import System.IO
import Data.Function
import Data.Char (isLower, isUpper)
import Data.List

main :: IO ()
main = do
    --handle <- openFile "test1.txt" ReadMode
    handle <- openFile "input.a64rpt.txt" ReadMode
    contents <- hGetContents handle
    let parsed = parseLines2 $ lines contents
        --part1

        res =  sum $ map (pooints . head) parsed
        --part2
        --res = parsed
    print parsed
    print res
    hClose handle

pooints :: Char -> Int
pooints ch | isLower ch = fromEnum ch - 96
          | otherwise = fromEnum ch - 65 + 27

parseLines :: String -> [Char]
parseLines str = nub $ intersect v1 v2 where
    v1 = take (length str `div` 2) str
    v2 = take (length str `div` 2) $ reverse str

parseLines2 :: [String] -> [String]
parseLines2 [] = []
parseLines2 (x : y : z : str) = helper (x : y : z : str) [] where
    helper [] res = res
    helper (x : y : z : str) res = helper str ((nub $ intersect x $ intersect y z) : res)

splitArrByCh :: [String] -> String -> [[String]]
splitArrByCh xs str = helper xs str [] [] where
    helper [] _ _ r = r
    helper (x:xs) str curr res | x/= str   = helper xs str (curr++[x]) res
                               | otherwise = helper xs str [] (res++[curr])

splitByChar :: String -> Char -> [String]
splitByChar str ch = helper str ch [] [] where
    helper [] _ temp res = if null temp then res else res ++[temp]
    helper (x:xs) ch temp res | x==ch = helper xs ch [] (res++[temp])
                              | otherwise = helper xs ch (temp++[x]) res
