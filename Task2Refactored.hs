module Task2 where

import System.IO
import Data.Function

main :: IO ()
main = do
    --handle <- openFile "test1.txt" ReadMode
    handle <- openFile "input.a64rpt.txt" ReadMode
    contents <- hGetContents handle
    let parsed = parseLines $ lines contents
        --part1
        --res = sum $ map calcScore parsed
        --part2
        res = sum $ map (calcScore . chooseStrategy) parsed
    print res
    hClose handle

parseLines :: [String] -> [(Char,Char)]
parseLines = map (\str -> (head str, last str))

itemPoints :: Char -> Int
itemPoints 'X' = 1
itemPoints 'Y' = 2
itemPoints 'Z' = 3

gameResultPoints :: Char -> Char -> Int
gameResultPoints a b | on (==) (uncurry $ on (-) fromEnum) (a, 'A') (b,'X') = 3
                     | a=='A' = if b=='Y' then 6 else 0
                     | a=='B' = if b=='Z' then 6 else 0
                     | a=='C' = if b=='X' then 6 else 0

calcScore :: (Char,Char) -> Int
calcScore (x,y) = itemPoints y + gameResultPoints x y

chooseStrategy :: (Char,Char) -> (Char,Char)
chooseStrategy (x,y) = (x,yourSign) where
    yourSign | x=='A' && y=='X' = 'Z'
             | x=='A' = toEnum (fromEnum y - 1)
             | x=='B' = y
             | x=='C' && y=='Z' = 'X'
             | x=='C' = toEnum (fromEnum y + 1)