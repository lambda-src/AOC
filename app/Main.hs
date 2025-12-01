module Main where

import Day1
import System.Environment (getArgs)

main :: IO ()
main = do 
    (day : _)  <- getArgs
    input <- lines <$> readFile ("inputs/Day" ++ day ++ ".txt")
    print $ case read day :: Int of 
        1 -> doRotates input 
        _ -> error "Haven't done the problem yet" 

