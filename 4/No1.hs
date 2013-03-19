data Tree = Nil
	  | Branch a (Tree a) (Tree a)
searchTree :: (a-> Bool) -> (Tree a) -> Bool

searchTree f Nil = False
searchTree f (Branch v l r) = if f v then True
				     else searchTree f l || searchTree f r