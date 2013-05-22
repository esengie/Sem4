checkIf :: (Eq a) => (a -> Bool) -> [a] -> Bool
checkIf p xs = foldr (&&) True $ map p xs