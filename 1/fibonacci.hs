module Main where

import System.IO

main = do
    hSetBuffering stdout NoBuffering
    putStr "Factorial\nn = "
    userInput <- getLine
    let fibonacci = \ n ->
                        \ fibo ->
                            fibo fibo n
                        \ (fb, k) ->
			    | k == 0 = 0
                            | k == 1 = 1
			    | otherwise = fb fb (k - 1) + fb fb (k - 2)

    putStrLn $ show $ fibonacci (read userInput :: Integer) 
 
