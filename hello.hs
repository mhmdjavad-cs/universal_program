import GHC.Natural (Natural, mkNatural)

largestPowerOf2 :: Integer -> Integer
largestPowerOf2 x
    | even x = (largestPowerOf2 $ div x 2) + 1
    | otherwise = 0

left :: Integer -> Integer
left x = largestPowerOf2 $ x+1

right :: Integer -> Integer
right x = div ((div (x+1) (2^(largestPowerOf2 $ x+1))) - 1) 2

fuctorial :: Integer -> Integer
fuctorial 0 = 1
fuctorial x = (fuctorial (x-1)) * x


mySucc :: Integer -> Integer
mySucc x = x+1

add :: Integer -> Integer -> Integer
add x 0 = x
add x y = mySucc $ add x (y-1)


zero :: Natural -> Natural
zero x = 0

successor :: Natural -> Natural
successor x = x + 1

p :: Natural -> Natural
p 0 = 0
p x = x-1

--minus :: Natural -> Natural
--minus x 0 = x
--minus x successor(y) = p (minus x y)


findLabelAtCode :: Integer -> Integer -> [Integer] -> Integer
findLabelAtCode destination current [] = current
findLabelAtCode destination current (x:xs)
    | (destination == (left$x)) = current
    | otherwise = findLabelAtCode destination (current+1) xs


main :: IO ()
main = do
--    putStrLn "Enter a number: "
  --  input <- getLine
    --let number = read input :: Natural
    --putStrLn ("the result is: " ++ (show $ p number) )

    putStrLn "Enter program numbers separated by spaces:"
    input <- getLine  -- Read the input line
    let numbers = map read (words input) :: [Integer]  -- Split and convert to Int

    print $ findLabelAtCode 14 0 numbers

    --putStrLn "enter input numbers seperated by spaces:"
    --input <- getLine
    --let inputs = map read (words input) :: [Natural]
    --mapM_ putStrLn (map getInstruction numbers)  -- Print each number on a separate line
    --print inputs
