isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome suspect = (==) suspect (reverse suspect)