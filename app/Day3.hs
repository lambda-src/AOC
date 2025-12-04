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
