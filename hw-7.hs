import Data.List

-- Class contains variable name and power value
data VarPow = VarPow {
  getVariable :: Char,
  getPower :: Integer
}

instance Show VarPow where
  show (VarPow variable power) = [variable] ++ "^" ++ (show power)

instance Eq VarPow where
  (==) vp1 vp2 = ((getVariable vp1) == (getVariable vp2)) && ((getPower vp1) == (getPower vp2))

-- Monomial class
data Monomial = Monomial {
  constant :: Integer,
  variables :: [VarPow]
}

instance Show Monomial where
  show (Monomial c v) = (show c) ++ (if (null v) then "" else " * ") ++ (intercalate " * " (map show v))

-- Simplifies Monomial
simplify :: Monomial -> Monomial
simplify monomial = Monomial (constant monomial) (simplify2 [] (variables monomial)) where
  simplify2 :: [VarPow] -> [VarPow] -> [VarPow]
  simplify2 stack [] = stack
  simplify2 stack (current:list) = simplify2 (if contains (getVariable current) stack then stack else stack ++ [combine (getVariable current) (getPower current) list]) list

-- Checks is [VarPow] contains variable
contains :: Char -> [VarPow] -> Bool
contains variable [] = False
contains variable (current:list) = if variable == (getVariable current) then True else contains variable list

-- Combines powers of the equals variable
combine :: Char -> Integer -> [VarPow] -> VarPow
combine variable power [] = VarPow variable power
combine variable power (current:list) = combine variable (power + if variable == (getVariable current) then (getPower current) else 0) list

instance Eq Monomial where
  (==) m1 m2 = simplifiedEquals (simplify m1) (simplify m2)

-- Checks that simplified monomials are equal
simplifiedEquals :: Monomial -> Monomial -> Bool
simplifiedEquals (Monomial c1 vp1) (Monomial c2 vp2) = (c1 == c2) && (varPowListEquals vp1 vp2)

-- Checks VarPow list on equals
varPowListEquals :: [VarPow] -> [VarPow] -> Bool
varPowListEquals vp1 vp2 = (all (\vp -> elem vp vp2) vp1) && (all (\vp -> elem vp vp1) vp2)

instance Num Monomial where
  fromInteger n = Monomial n []
  (*) (Monomial c1 vp1) (Monomial c2 vp2) = simplify (Monomial (c1 * c2) (vp1 ++ vp2))
  negate (Monomial c v) = Monomial (negate c) v

-- Sum of the monomials. Monomial + Monomial is often Polynomial
sumMonomial :: Monomial -> Monomial -> Polynomial
sumMonomial m1 m2 = (Polynomial [m1]) + (Polynomial [m2])

-- Polynomial class
data Polynomial = Polynomial [Monomial]

instance Show Polynomial where
  show (Polynomial p) = intercalate " + " (map (\m -> "(" ++ (show m) ++ ")") p)

-- Simplifies Polinomial
simplifyPolynomial :: Polynomial -> Polynomial
simplifyPolynomial (Polynomial p) = simplify2 [] (map simplify p)
  where simplify2 :: [Monomial] -> [Monomial] -> Polynomial
        simplify2 stack [] = Polynomial stack
        simplify2 stack (current:list) = simplify2 (if containsMonomial current stack then stack else stack ++ [combineMonomials current list]) list

-- Check that monomial contains if Monomial list(ignoring constant)
containsMonomial :: Monomial -> [Monomial] -> Bool
containsMonomial m [] = False
containsMonomial m (current:list) = if varPowListEquals (variables m) (variables current) then True else containsMonomial m list

-- Combine same monomials from the list multiplying constant
combineMonomials :: Monomial -> [Monomial] -> Monomial
combineMonomials m [] = m
combineMonomials m (current:list) = combineMonomials (Monomial (if varPowListEquals (variables m) (variables current) then (constant m) + (constant current) else (constant m)) (variables m)) list

instance Num Polynomial where
  fromInteger n = Polynomial [fromInteger n]
  (+) (Polynomial []) (Polynomial []) = Polynomial [fromInteger 0]
  (+) (Polynomial p1) (Polynomial p2) = simplifyPolynomial (Polynomial $ (++) p1 p2)
  (*) p1 (Polynomial []) = p1 * (fromInteger 0)
  (*) (Polynomial []) p2 = (fromInteger 0) * p2
  (*) (Polynomial p1) (Polynomial p2) = simplifyPolynomial (Polynomial $ doubleMap (*) p1 p2)
  negate (Polynomial p) = Polynomial (map negate p)

-- Makes list that map every pair of the two lists
doubleMap :: (a -> b -> c) -> [a] -> [b] -> [c]
doubleMap f l1 l2 = concat (map (\x -> map (f x) l2) l1)

monomial1 = (Monomial 4 [VarPow 'x' 2, VarPow 'y' 3])
monomial2 = (Monomial 8 [VarPow 'x' 2, VarPow 'y' 3])
monomial3 = (Monomial 8 [VarPow 'x' 4, VarPow 'y' 3])
monomial4 = (Monomial 8 [VarPow 'x' 4, VarPow 'z' 3])
monomial5 = (Monomial (-4) [])

monomials = [monomial1, monomial2, monomial3, monomial4, monomial5]

polynomial1 = Polynomial [monomial1, monomial2]
polynomial2 = Polynomial [monomial3]
polynomial3 = Polynomial [monomial1, monomial4, monomial5]
polynomial4 = Polynomial [monomial1, monomial2, monomial3, monomial4, monomial5]

printResult f x y = do
  print x
  print y
  print $ f x y
  print "----------------------------------"

main = do
  print "----------------------------------"
  print "Sum of the monomials"
  print "----------------------------------"
  printResult sumMonomial monomial1 monomial2
  printResult sumMonomial monomial1 monomial3
  printResult sumMonomial monomial1 monomial4
  printResult sumMonomial monomial1 monomial5
  printResult sumMonomial monomial2 monomial2
  printResult sumMonomial monomial3 monomial2
  print "Product of the monomial"
  print "----------------------------------"
  printResult (*) monomial1 monomial2
  printResult (*) monomial1 monomial3
  printResult (*) monomial1 monomial4
  printResult (*) monomial1 monomial5
  printResult (*) monomial2 monomial2
  printResult (*) monomial3 monomial2
  print "Sum of the polynomial"
  print "----------------------------------"
  printResult (+) polynomial1 polynomial2
  printResult (+) polynomial2 polynomial2
  printResult (+) polynomial2 polynomial3
  printResult (+) polynomial4 polynomial1
  printResult (+) polynomial1 polynomial4
  print "Product of the polynomial"
  print "----------------------------------"
  printResult (*) polynomial1 polynomial2
  printResult (*) polynomial2 polynomial2
  printResult (*) polynomial2 polynomial3
  printResult (*) polynomial4 polynomial1
  printResult (*) polynomial1 polynomial4
