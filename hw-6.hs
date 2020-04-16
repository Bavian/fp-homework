import Data.Set

data Expr =
  Integral Integer | -- целые константы
  Function String | -- идентификаторы примитивных функций
  Variable String | -- переменная
  Lambda String Expr | -- лямбда-выражение
  Application Expr Expr -- применение функции

getFunctions :: Expr -> [String]
getFunctions (Function f) = [f]
getFunctions (Lambda s expr) = getFunctions expr
getFunctions (Application expr1 expr2) = (++) (getFunctions expr1) (getFunctions expr2)
getFunctions _ = []

excludeDuplicates list = elems (fromList list)

getUniqueFunctionsList :: Expr -> [String]
getUniqueFunctionsList expr = excludeDuplicates (getFunctions expr)
