import Data.Char

sumItUp :: Integer -> Int
sumItUp n = foldr (+) 0 (map  digitToInt (show n))