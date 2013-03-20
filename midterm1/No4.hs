crazyTrain :: [Double] -> Double
crazyTrain [] = 0
crazyTrain list = (crazyTrain' list (length list) 0 1)
crazyTrain' :: [Double] -> Int -> Double -> Double -> Double
crazyTrain' [] _ sum' prod' =  sum' / prod'
crazyTrain' (x:xs) n sum' prod' =     if n <= 0 then sum' / prod'	else crazyTrain' xs (n - 1) (sum' + x) (prod' * cos(x))