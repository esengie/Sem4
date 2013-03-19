areSame :: (Eq a) => [a] -> Bool

areSame xs = areSame' [] xs
	where 
		areSame' _ [] = True
		areSame' (ys) (z:zs) = if elem z ys
					then False
					else areSame' (z:ys) zs