-- square is a mapping from Int to Int
square :: Int -> Int
square x = x * x

-- Main function. Compile with `ghc chapter1.hs` and execute chapter1.exe
-- Alternatively, use interactive GHC with `ghci chapter1.hs`. Quit GHCi with `:quit`
main :: IO ()
main = do
  print (square 5)