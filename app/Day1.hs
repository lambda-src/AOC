module Day1 where 

-- Part 1 -- 
-- We have a dial from 0-99.
-- If we start at 0 then do R80 it would move it to 80 then L50 would move it to 32.
-- Therefore L can be subtraction and R can be addtion with the caviot that.
-- L and R wrap back around to 99 and 0 respectively (modulo time :3).
rotate :: Int -> String -> Int 
rotate p ('L':n) = (p - read n) `mod` 100
rotate p ('R':n) = (p + read n) `mod` 100
rotate _ _ = error "Unexpected error unreachable"

-- After each rotation count the number of times we end up with 0
doRotates :: [String] -> Int 
doRotates rotates = go rotates 0 50 
    where 
        -- We need a rotate, accumlator, and current pos var
        go (x:xs) acc pos = 
            let newPos = rotate pos x
            in if newPos == 0 
            then go xs (acc + 1) newPos
            else go xs acc newPos
        go [] acc _ = acc

-- Part 2 -- 
-- We now also want to count any time during the rotation that it points at zero.
-- To do this every single number that gets rotated to can be added to a list with the
-- base case being dropped so a number isn't revistsed. Then you just count the number of 
-- 0's in the list to see how many times zero got hit. Space wise this is terrible but it's
-- very short and concise :D
rotateSteps :: Int -> String -> [Int]
rotateSteps p ('L':n) = drop 1 $ scanl (\x _ -> (x - 1) `mod` 100) p [1 .. read n :: Int]
rotateSteps p ('R':n) = drop 1 $ scanl (\x _ -> (x + 1) `mod` 100) p [1 .. read n :: Int]
rotateSteps _ _ = error "Unexpected error unreachable"

doRotateSteps :: [String] -> Int
doRotateSteps rotates = go rotates 50 0
    where
        go (x:xs) pos acc =
            let steps  = rotateSteps pos x
                -- Same as in doRotates except count the number of times 0 gets hit instead of == 0
                hits   = length $ filter (== 0) steps
                newPos = last steps
            in go xs newPos (acc + hits)
        go [] _ acc = acc