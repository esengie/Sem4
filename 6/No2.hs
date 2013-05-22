type Polynomial = [Int]

trimPolynomial :: Polynomial -> Polynomial
trimPolynomial p = if null p 
                      then [] 
                      else if (last p) /= 0 then p 
                                            else trimPolynomial (init p)
polynomialDegree :: Polynomial -> Int
polynomialDegree p = length (trimPolynomial p) - 1


term :: Polynomial -> Int -> Int
term p i = if i < (length p) then p!!i else 0
   
addition :: Polynomial -> Polynomial -> Polynomial
addition p1 p2 = trimPolynomial [(term p1 i) + (term p2 i) | i <- [0..(max (length p1) (length p2)) - 1]]
   
subtraction :: Polynomial -> Polynomial -> Polynomial
subtraction p1 p2 = trimPolynomial [(term p1 i) - (term p2 i) | i <- [0..(max (length p1) (length p2)) - 1]]


derivative' :: Polynomial -> Polynomial
derivative' p = trimPolynomial [(i * (p!!i)) | i <- [1..(length (trimPolynomial p)) - 1]]

derivative :: Polynomial -> Int -> Polynomial
derivative p 0 = p
derivative p n = derivative' (derivative p (n - 1))

showTerm :: Int -> Int -> String
showTerm 0 i = ""
showTerm a 0 = (if (a > 0) then "+" else "-") ++ show (abs a)
showTerm a 1 = (if (a == 1) then "" else if (a == -1) then "" else showTerm a 0) ++ "x"
showTerm a i = (if (a == 1) then "" else if (a == -1) then "" else showTerm a 0) ++ "x^" ++ show i

showLast :: Polynomial -> String
showLast p = if null p then "" else showTerm (last p) ((length p) - 1)

showPolynomial :: Polynomial -> String
showPolynomial p = showLast (trimPolynomial p) ++ if null p then "" else showPolynomial (init (if null (trimPolynomial p) then [0] else (trimPolynomial p)))

