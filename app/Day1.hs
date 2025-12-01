module Day1 (doRotates) where 

-- We have a dial from 0-99
-- If we start at 0 then do R80 it would move it to 80 then L50 would move it to 32
-- Therefore L can be subtraction and R can be addtion with the caviot that
-- L and R wrap back around to 99 and 0 respectively (modulo time :3)
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