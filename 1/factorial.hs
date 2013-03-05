module Main where

import System.IO
import Unsafe.Coerce

main = do
    hSetBuffering stdout NoBuffering
    putStr "Factorial\nn = "
    userInput <- getLine
    let factorial = \ n ->
                      \ fact -> (unsafeCoerce fact fact n)
                       \ (ft, k) ->
                            if (k == 1)
                               then 1
                               else k * (unsafeCoerce ft ft (k - 1))

    putStrLn $ show $ factorial (read userInput :: Integer) 
