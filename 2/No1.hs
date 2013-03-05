Reverse :: [a] -> [a]
Reverse = foldl (flip (:)) []