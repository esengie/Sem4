data InfoType = Book String String Double -- name author price
	      | Journal Int Int Double -- year number price
type InfoBook = [InfoType]
overallPrice :: InfoBook -> Double
overallPrice [] = 0
overallPrice ((Book a b price):xs) = price + (overallPrice xs)
overallPrice ((Journal a b price):xs) = price + (overallPrice xs)
	      