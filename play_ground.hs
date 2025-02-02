import qualified Data.Map as Map


variablesList :: [String]
variablesList = "Y" : concat [[ "X" ++ show n, "Z" ++ show n ] | n <- [1..]]

replaceAtIndex :: Integer -> a -> [a] -> [a]
replaceAtIndex _ _ [] = []  -- Edge case: empty list
replaceAtIndex i newVal (x:xs)
    | i == 0    = newVal : xs                -- Replace element at index 0
    | i > 0     = x : replaceAtIndex (i - 1) newVal xs  -- Recursively process the rest
    | otherwise = x : xs                     -- Negative index does nothing


initialState :: [Integer] -> [Integer]
initialState [] = repeat 0
initialState (x:xs) = 0:x:initialState(xs)

main :: IO ()
main = do
    putStrLn "enter input numbers seperated by spaces:"
    input <- getLine
    let inputs = map read (words input) :: [Integer]
    --mapM_ putStrLn (map getInstruction numbers)  -- Print each number on a separate line
    print inputs
    print $ take 10 (initialState inputs)
