module Day4 where 

import qualified Data.Vector as V 

type Grid = V.Vector (V.Vector Char)
type Cell = (Int, Int)

-- Part 1 -- 
-- We're given a big string of "@" and "." organized in a big grid,
-- we then need to count the nuber of valid "@" where a valid "@" has 
-- fewer than 4 "@" surrounding it. So for instance:
-- "@.@@
--  .@@@
--  @@@.
--  ...@"
-- has these valid spaces where x is the valid space
-- "x.@x
--  .@@@
--  x@@.
--  ...x"
-- A sliding window would work but a cleaner solution would probably just be
-- to treat this as a array instead and just index around the cells for every
-- cell to make sure its valid 

-- Turn the input into the correct type
intoGrid :: [String] -> Grid 
intoGrid s = V.fromList $ map V.fromList s

-- Make sure that cell is in bounds
isInBounds :: Grid -> Cell -> Bool 
isInBounds g (r, c) = 
    r >= 0 && r < V.length g &&
    c >= 0 && c < V.length (g V.! 0)

-- Create a list of all the neigbors then create a new list of the bad ones then if the length of bad neighbors is under 4 the current cell is good
hasValidNeigbors :: Grid -> Int -> Int -> Bool 
hasValidNeigbors g r c = 
    let neighbors = 
            [ (r + nr, c + nc)
            | nr <- [-1..1]
            , nc <- [-1..1]
            , (nr, nc) /= (0, 0)
            ]
        badNeighbors = 
            [ () 
            | cord <- neighbors 
            , isInBounds g cord
            , (g V.! fst cord) V.! snd cord == '@' 
            ]
    in length badNeighbors < 4


-- Create a list of valid neighbors then get the length of it
doNeighbors :: [String] -> Int 
doNeighbors input = 
    let g = intoGrid input
        h = V.length g 
        w = V.length (g V.! 0)
        validSpots =
            [ ()
            | r <- [0..h-1]
            , c <- [0..w-1]
            , (g V.! r) V.! c == '@'
            , hasValidNeigbors g r c 
            ] 
    in length validSpots

-- Part 2 -- 
-- Now for part 2 intstead of counting all the total valid spots we now also remove the valid "@" then recount.
-- A easy solution would be to remove all the valid cells and count them up then keep track of that with an accumlator
-- then just keep recalling with the new grid until there was no changes

-- Creates a new grid with the cell removed
removeCell :: Grid -> Cell -> Grid
removeCell g (r, c) = g V.// [(r, (g V.! r) V.// [(c, '.')])]

-- Get the removable spots then fold over them and remove then get the length of the removed spots with the new grid
removeValidCell :: Grid -> (Grid, Int)
removeValidCell g =
    let h = V.length g
        w = V.length (g V.! 0)
        removableSpots =
            [ (r, c)
            | r <- [0..h-1]
            , c <- [0..w-1]
            , (g V.! r) V.! c == '@'
            , hasValidNeigbors g r c
            ]
        g' = foldl removeCell g removableSpots
    in (g', length removableSpots)

-- Same as doNeihbors except now there's an accumalator and the grid is being recursively passed through for every change
doRemoveNeighbors :: [String] -> Int 
doRemoveNeighbors input = go (intoGrid input) 0 
    where 
        go g acc = 
            let (g', removed) = removeValidCell g 
            in if removed == 0
            then acc 
            else go g' (acc + removed)

    