signu = 1 : -1 : signu

something :: [Int] -> [Int] -> [Int]
something xs [] = xs
something [] ys = ys
something (x:xs) (y:ys) = x*y : something xs ys

result = something [1..] signu