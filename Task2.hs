module Task2 where

import System.IO
import Data.List

main :: IO ()
main = do
    handle <- openFile "input.a64rpt.txt" ReadMode
    contents <- hGetContents handle

    let parsed = parseLines $ lines contents
        
        res = sum $ map calcScore2 parsed
    print res
    hClose handle


parseLines strs = map (\str -> (head str, last str)) strs

calcScore (x,y) = your + (win x y) where
    your | y=='X' = 1
         | y=='Y' = 2
         | y=='Z' = 3
         | otherwise = 0
        
    win 'A' x | x=='X' = 3
              | x=='Y' = 6
              | x=='Z' = 0
    win 'B' x | x=='X' = 0
              | x=='Y' = 3
              | x=='Z' = 6
    win 'C' x | x=='X' = 6
              | x=='Y' = 0
              | x=='Z' = 3

calcScore2 (x,y) = calcScore (x,yourSign) where
    yourSign | x=='A' && y=='X' = 'Z'
             | x=='A' && y=='Y' = 'X'
             | x=='A' && y=='Z' = 'Y'
             | x=='B' && y=='X' = 'X'
             | x=='B' && y=='Y' = 'Y'
             | x=='B' && y=='Z' = 'Z'
             | x=='C' && y=='X' = 'Y'
             | x=='C' && y=='Y' = 'Z'
             | x=='C' && y=='Z' = 'X'