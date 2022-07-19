-- square is a mapping from Int to Int
square :: Int -> Int
square x = x * x

cube, quart, pent :: Int -> Int -- multiple type declarations on one line
cube x = x * x * x
quart x = x * x * x * x
pent x = x * x * x * x * x

three :: Int -> Int
three x = 3

-- non-terminating recursive function
nonTerm :: Int -> Int
nonTerm x = nonTerm (x + 1)

-- takes the maximum of two given values
maxi :: (Int, Int) -> Int
maxi (x, y)
  | x >= y = x
  | otherwise = y

-- standard addition of two arguments by supplying a tuple of integers
plus :: (Int, Int) -> Int
plus (x, y) = x + y

-- curried addition of arguments supplied in succession
plusCurried :: Int -> (Int -> Int)
plusCurried x y = x + y

-- pattern matching the "und" function with True and False
und :: Bool -> Bool -> Bool
und True y = y
und False y = False

-- pattern matching on `True` and a general expression `x`
undGeneral :: Bool -> Bool -> Bool
undGeneral True y = y
undGeneral x y = False

-- an unclear function that does not terminate
unclear :: Int -> Bool
unclear x = not (unclear x)

-- recursive definition of length function
len :: [Int] -> Int
len [] = 0
len (x : xs) = 1 + len xs

-- find the second element of a list
second :: [Int] -> Int
second [] = 0
second [x] = 0
second (x : y : xs) = y

-- Quadratic function solver (coefficient variables are curried), returning a tuple of roots
roots :: Float -> Float -> Float -> (Float, Float)
roots a b c = ((- b - d) / e, (- b + d) / e)
  where
    d = sqrt (b * b - 4 * a * c)
    e = 2 * a

-- Main function. Compile with `ghc chapter1.hs` and execute chapter1.exe
-- Alternatively, use interactive GHC with `ghci chapter1.hs`. Quit GHCi with `:quit`
main :: IO ()
main = do
  print (square 5)
  print (three (nonTerm 0)) -- this will terminate, since Haskell performs lazy evaluation

  -- these two lines provide the same result,
  print (plus (3, 5))
  print (plusCurried 3 5) -- but the second is curried

  -- this evaluation terminates because it pattern matches on `x y`
  print (und False (unclear 0))

  -- this evaluation doesn't terminate because it is unclear if (unclear 0) is True
  -- print (und (unclear 0) False) -- the second and third expressions do not terminate

  -- this evaluation doesn't terminate because it is unclear what is the return value of `y`
  -- print (und True (unclear 0))

  print (len [2, 4, 6, 8]) -- length is 4
  print (second [1, 3, 5, 7, 9]) -- 2nd element is 3

  -- quadratic solver
  print (roots 1 2 (-1 * 15)) -- returns (-5.0, 3.0)
