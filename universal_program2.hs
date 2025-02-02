



main :: IO ()
main = do
    putStrLn "Enter program numbers separated by spaces:"
    input <- getLine  -- Read the input line
    let numbers = map read (words input) :: [Integer]  -- Split and convert to Int
    mapM_ putStrLn (map getInstruction numbers)  -- Print each number on a separate line
