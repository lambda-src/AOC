module Day3 where 

-- Part 1 -- 
-- Given a string of numbers like "12345" we must enable two of the nums to get the highest possible number
-- but they have to be in sequnce, so for "12345" the best we can do is "45" for "232953" the best possible 
-- num is "95". A good way to solve this would be to create a list of every possible combination then just 
-- choose the largest number from that list 

-- Create a list of every combination 
allCombos :: String -> [Int]
allCombos s = 
    [ read [a,b] 
    | (i,a) <- zip [0..] s
    , b <- drop (i+1) s
    ]

-- Sum up all the largest numbers of every combination
doAllCombos :: [String] -> Int 
doAllCombos input = sum $ map (maximum . allCombos) input

-- Part 2 -- 
-- Given strings of 15 characters made up of numbers we need to find the largest number possible by turning on
-- 12 of the digits. For instance the largest num possible of "939782123098756" would be "997823098756"  by 
-- not turning on the 3, 2, and 1. This means that the largest numbers need to be kept to the left side of the string 
-- where if string[i] < string[i+1] then string[i] needs to not be included for 3 digits