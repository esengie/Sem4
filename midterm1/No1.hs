minus :: (Ord a) => [a] -> [a] -> [a]
minus = minusBy compare

minusBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
minusBy cmp = loop
  where
     loop [] _ys = []
     loop xs [] = xs
     loop (x:xs) (y:ys)
       = case cmp x y of
          LT -> x : loop xs (y:ys)
          EQ ->     loop xs ys
          GT ->     loop (x:xs) ys
--primesTo:: Int -> [Int]
primesTo m = 2 : eratos [3,5..m]  where
   eratos []     = []
   eratos (p:xs) = p : eratos (xs `minus` [p, p+2*p..m])