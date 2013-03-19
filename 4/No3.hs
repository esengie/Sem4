evenNum1 :: (Integral a) => [a] -> a
evenNum1 xs = sum (map (\ x -> mod x 2) xs)

evenNum2 :: (Integral a) => [a] -> a
evenNum2 xs =  length (filter even xs)

evenNum3 :: (Integral a) => [a] -> Int
evenNum3 xs = foldr (\x acc -> if (odd x) then acc + 1 else acc) 0 xs