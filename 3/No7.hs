--func f g l = filter f (map g l)
--func f g = (filter f) .(map g)

func :: (b -> Bool) -> (a -> b) -> [a] -> [b]

--func 
--func = ((.) (filter f)) . map  
func = (. map) . (.) . filter 



--func (\x -> x>34) (\y -> 2^y) [2,5,6,1,4,7,6]
