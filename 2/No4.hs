findFirst :: (Eq a, Num b) => a -> [a] -> b
findFirst element list = findHelper element list 1
  where findHelper _ [] _ = -1
        findHelper element (x:xs) pos = if (element == x) then pos 
          else findHelper element xs (pos + 1)