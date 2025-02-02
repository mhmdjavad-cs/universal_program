--import Distribution.Simple.Utils (xargs)
import Debug.Trace (trace)
import GHC.Natural (Natural, mkNatural)
import Data.List (genericLength)


p :: Natural -> Natural
p 0 = 0
p x = x-1

snd2 (x,y,z) = y

variablesList :: [String]
variablesList = "Y" : concat [[ "X" ++ show n, "Z" ++ show n ] | n <- [1..]]

labelsList :: [String]
labelsList = concat [["A" ++ show n, "B" ++ show n, "C" ++ show n, "D" ++ show n, "E" ++ show n] | n <- [1..]]

godelCode :: Int -> Int -> Int
godelCode x y = (2^x)*(2*y - 1) - 1

largestPowerOf2 :: Int -> Int
largestPowerOf2 x
    | even x = (largestPowerOf2 $ div x 2) + 1
    | otherwise = 0

left :: Int -> Int
left x = largestPowerOf2 $ x+1

right :: Int -> Int
right x = div ((div (x+1) (2^(largestPowerOf2 $ x+1))) - 1) 2

getLabel :: Int -> String
getLabel x
    | left x == 0 = "[  ]"
    | otherwise = "[" ++ ( labelsList!! ( ( fromIntegral (left x) ) -1 ) ) ++ "]"

getVariable :: Int -> String
getVariable x = variablesList !! (fromIntegral $ right $ right x)

getGotoLabel :: Int -> String
getGotoLabel x = labelsList !! ((fromIntegral (left $ right x)) - 3)

getInstruction :: Int -> String
getInstruction x
    | (left $ right x) == 0 = ( (getLabel x) ++ " " ++ (getVariable x) ++ "<-" ++ (getVariable x) )
    | (left $ right x) == 1 = ( (getLabel x) ++ " " ++ (getVariable x) ++ "<-" ++ (getVariable x) ++ "+1" )
    | (left $ right x) == 2 = ( (getLabel x) ++ " " ++ (getVariable x) ++ "<-" ++ (getVariable x) ++ "-1" )
    | (left $ right x) >  2 = ( (getLabel x) ++ " " ++ "IF " ++ (getVariable x) ++ " != 0  GOTO " ++ (getGotoLabel x) )


-- from here on the codes are for running the
-- actual universal program:

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex _ _ [] = []  -- Edge case: empty list
replaceAtIndex i newVal (x:xs)
    | i == 0    = newVal : xs                -- Replace element at index 0
    | i > 0     = x : replaceAtIndex (i - 1) newVal xs  -- Recursively process the rest
    | otherwise = x : xs                     -- Negative index does nothing



findLabelAtCode :: Int -> Int -> [Int] -> Int
findLabelAtCode destination current [] = current
findLabelAtCode destination current (x:xs)
    | (destination == (left$x)) = current
    | otherwise = findLabelAtCode destination (current+1) xs


initialState :: [Natural] -> [Natural]
initialState [] = repeat 0
initialState (x:xs) = 0:x:initialState(xs)

snap :: (Int, [Natural], Bool) -> [Int] -> (Int, [Natural], Bool)
snap (instruction, state, False) program = snap (mySucc (instruction, state, False) program) program --(0, state)
snap (instruction, state, True) program  = (instruction, state, True) --mySucc (snap(instruction,state) program (i-1)) program

mySucc :: (Int, [Natural], Bool) -> [Int] -> (Int, [Natural], Bool)
mySucc (instruction, state, dummy) code
    | instruction >= (genericLength code) = trace ("halt " ++ " line number " ++ (show instruction) ++ "\t" ++ (show$take 10 state)) $
        (genericLength code, state, True)
    | (left $ right $ code!!instruction) == 0 = trace ("skip "++ " line number " ++ (show instruction) ++ "\t" ++ (show$take 10 state)) $
         (instruction + 1, state, False)
    | (left $ right $ code!!instruction) == 1 = trace ("+ to " ++ (variablesList!!(right$right$code!!instruction)) ++ " line number " ++ (show instruction) ++ "\t" ++ (show$take 10 state)) $
    (instruction + 1,
    replaceAtIndex
    (right $ right $ code!!instruction)
    ( (state !! (right$right$code!!instruction)) +1)
    state , False)
    | (left$right$code!!instruction) == 2 = trace ("- to " ++ (variablesList!!(right$right$code!!instruction)) ++ " line number " ++ (show instruction) ++ "\t" ++ (show$take 10 state)) $
    (instruction + 1,
    replaceAtIndex
    (right$right$code!!instruction)
    (p$ (state!! (right$right$code!!instruction)))
    state, False)
    | (left$right$code!!instruction) > 2 && ((state!! (right$right$code!!instruction)) /= 0) =
        trace ("jump" ++ " line number " ++ (show instruction) ++ "\t" ++ (show$take 10 state)) $
     (findLabelAtCode ((left$right$code!!instruction)-2) 0 code,state, False)
    | (left$right$code!!instruction) > 2 && ((state!! (right$right$code!!instruction)) == 0) =
        trace ("no jump" ++ " line number " ++ (show instruction) ++ "\t" ++ (show$take 10 state)) $
     (instruction+1,state, False)


main :: IO ()
main = do
    putStrLn "Enter prigram numbers separated by spaces:"
    input <- getLine  -- Read the input line
    let numbers = map read (words input) :: [Int]  -- Split and convert to Int
    putStrLn "enter input numbers seperated by spaces:"
    input <- getLine
    let inputs = map read (words input) :: [Natural]
    mapM_ putStrLn (map getInstruction numbers)  -- Print each number on a separate line
    --print inputs
    --snap (0, initialState inputs) numbers 10
    print $ (take 10  (snd2 $ snap (0, initialState inputs, False) numbers))
