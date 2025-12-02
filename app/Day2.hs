module Day2 where

import Data.List.Split (splitOn)

-- Part 1 -- 
-- Given a range of ids in the form "n-m" find if there's any repeats in that range.
-- A good way to solve this would to simply convert "n-m" into a range of n .. m 
-- then for each n and m convert it to a string and divive it into 2 then compare them. 

-- Converts each n to a string then compares the first half to the last half
isRepeat :: Int -> Bool 
isRepeat n = 
    let s   = show n 
        len = length s 
    in even len &&
    let (fh, lh) = splitAt (len `div` 2) s 
    in fh == lh 

-- Converts each "n-m" into a range of n .. m
intoRange :: String -> [Int]
intoRange s = case span (/= '-') s of 
    (a, '-':b) -> [read a .. read b]
    _ -> error "Unexcpected error unreachable"

-- Get the ranges then filter out all non repeating numbers then sum all the duplicates
doRepeat :: [String] -> Int
doRepeat input =
    let ranges = concatMap (splitOn ",") input
        sumDups r = sum $ filter isRepeat $ intoRange r
    in sum (map sumDups ranges)

-- Part 2 -- 
-- Now if something repeats more than twice then its invalid so for instance 
-- 123123123 repeats "123" 3 times so its invalid. A good solution would be to check if
-- first half is equal to second half then if all thirds are equal then if all fourths are equal
-- repeating until you hit the length of characters. 

-- Get the windows from 1 .. (length string / 2) then return if any is a correct repeat
isRepeatWindow :: Int -> Bool 
isRepeatWindow n = 
    let s       = show n 
        windows = [1 .. (length s `div` 2)] 
    in any (checkWindow s) windows 

-- If its not a multiple then early return false otherwise take each window and * r then check if it equals the original string
-- then return true.
checkWindow :: String -> Int -> Bool
checkWindow s n 
    | len `mod` n /= 0 = False 
    | otherwise = 
        let window = take n s
            r = len `div` n
        in concat (replicate r window) == s
    where len = length s 

-- Same as doRepeat pretty much
doRepeatWindow :: [String] -> Int
doRepeatWindow input =
    let ranges = concatMap (splitOn ",") input
        sumDups r = sum $ filter isRepeatWindow $ intoRange r
    in sum (map sumDups ranges)