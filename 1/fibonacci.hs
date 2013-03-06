module Main where

import System.IO
fibonacci :: Int -> Integer
fibonacci k | k == 0 = 0
            | k == 1 = 1
            | otherwise = fibonacci (k - 1) + fibonacci (k - 2)


main = do
    hSetBuffering stdout NoBuffering
    putStr "Factorial\nn = "
    userInput <- getLine
    putStrLn $ show $ fibonacci (read userInput :: Integer) 
 
