catMultiplier :: Integer -> Integer -> Integer
catMultiplier 0 previousCatalanNumber = previousCatalanNumber
catMultiplier n previousCatalanNumber = div (2 * (2 * n - 1) * previousCatalanNumber) (n + 1)

cat :: Integer -> Integer
cat n = reversedCat 0 n 1
  where reversedCat :: Integer -> Integer -> Integer -> Integer
        reversedCat currentN n currentNumber  | currentN > n = currentNumber
                                              | currentN <= n = reversedCat (currentN + 1) n (catMultiplier currentN currentNumber)

main = do
  print (cat 50)
