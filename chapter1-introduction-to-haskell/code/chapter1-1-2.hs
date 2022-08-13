-- `maxi` function with ternary operator
maxi (x, y) = if x >= y then x else y

-- `roots` function with "let"
roots a b c =
  let d = sqrt (b * b - 4 * a * c)
      e = 2 * a
   in ((- b - d) / e, (- b + d) / e)

-- `und` function with "case"
und x y = case x of
  True -> y
  False -> False

-- lambda expressions syntax
plus = \x y -> x + y

plusAlt x = \y -> x + y

main :: IO ()
main = do
  print (maxi (3, 22))
  print (und True True) -- True
  print (plus 4 9)
  print (plusAlt 3 4)