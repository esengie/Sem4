import GHC.IO
spaces = ' ':spaces
stars = '*':stars

does n = does' n (n - 1)
does' n k
	  | k >= 0 = ir:ini ++ [im]		       
	  | otherwise = []
		  where ir = take k spaces ++ take (n + n - 1 - 2*k) stars ++ take k spaces		      
			ini = does' n (k-1)
			im = take k spaces ++ take (n + n - 1 - 2*k) stars ++ take k spaces
diam n =  mapM print (does n)