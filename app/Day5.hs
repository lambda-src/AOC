module Day5 where 

import Data.List (sortOn)

type Range = (Int, Int)

-- Part 1 -- 
-- For this problem we're given ranges of numbers and id's, we need to count the number of valid id's
-- and to do that they must be within the ranges, this could be easily done by getting all the ranges then making
-- sure each id is part of a range but with how big the numbers are that could be trillions to quaddrillions of
-- comparisons. Instead make a bunch of new rages to get rid of the overalapping ranges to vastly reduce the 
-- number of comparisons.


-- Merge overlapping ranges into a single range
mergeRanges :: [Range] -> [Range]
mergeRanges = foldr sortRange [] . sortOn fst
    where
        sortRange r [] = [r]
        sortRange (l,h) ((l',h'):rest)
            -- If no overlap then preappend
            | h < l'-1 = (l,h) : (l',h') : rest
            -- If overlap then preappend with the changed new range
            | otherwise = (min l l', max h h') : rest 

-- Check if a number is in any of the merged ranges
isFresh :: [Range] -> Int -> Bool
isFresh r n = any (\(l,h) -> n >= l && n <= h) r

-- Parse input into range same as Day2 into range pretty much
parseRange :: String -> Range
parseRange s = case break (== '-') s of
    (a, '-':b) -> (read a, read b)
    _          -> error $ "Invalid range: " ++ s

-- Parse the input into ranges and list of numbers then merge the ranges so there's no overlap then find the length of filter $ isFresh r n 
doRanges :: [String] -> Int
doRanges input = length $ filter (isFresh r') n'
  where
    (r, n) = break null input
    r'     = mergeRanges $ map parseRange r
    n'     = map read (drop 1 n)


-- Part 2 -- 
-- Now instead of finding all the valid numbers we instead sum the length of all the ranges 

-- I have no idea why this doesnt work pls help sob
doSumRanges :: [String] -> Int 
doSumRanges input = sum $ map rangeLength r'
    where 
        (r, _) = break null input
        r'     = mergeRanges $ map parseRange r 
        rangeLength (l, h) = h - l + 1


