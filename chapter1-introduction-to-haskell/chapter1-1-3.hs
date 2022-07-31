-- append function for lists
append :: [Int] -> [Int] -> [Int]
append [] ys = ys
append (x : xs) ys = x : append xs ys

main :: IO ()
main = do
  print (append [3, 4] [5, 6])