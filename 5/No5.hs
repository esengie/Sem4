serch :: [Int] -> Maybe Int
serch [] = Nothing
serch [x] = Just x
serch (x:xs) | x > xs!! 0 = Just x
             | otherwise  = serch' (Just x) xs
  where serch' x [] = x 
        serch' (Just x) (a:xs)
          | a > x && xs == [] = Just a
          | a > x && a > (xs !! 0) = Just a
          | otherwise = serch' (Just a) xs