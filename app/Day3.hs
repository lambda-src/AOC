module Day3 where 

-- Part 1 -- 
-- Given a string of numbers like "12345" we must enable two of the nums to get the highest possible number
-- but they have to be in sequnce, so for "12345" the best we can do is "45" for "232953" the best possible 
-- num is "95". A good way to solve this would be to find the largest digit then set it to on then also flip 
-- the largest digit to the right of it with the caviot that if there is no digit to the right then find the 
-- second largest digit and choose something to the right. 

largestStr :: String -> String 
largestStr s = 
    