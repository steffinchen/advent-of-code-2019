module Day2
( a, b
) where
    
import Debug.Trace 

input :: [Int]
input = [1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,10,1,19,1,6,19,23,1,23,13,27,2,6,27,31,1,5,31,35,2,10,35,39,1,6,39,43,1,13,43,47,2,47,6,51,1,51,5,55,1,55,6,59,2,59,10,63,1,63,6,67,2,67,10,71,1,71,9,75,2,75,10,79,1,79,5,83,2,10,83,87,1,87,6,91,2,9,91,95,1,95,5,99,1,5,99,103,1,103,10,107,1,9,107,111,1,6,111,115,1,115,5,119,1,10,119,123,2,6,123,127,2,127,6,131,1,131,2,135,1,10,135,0,99,2,0,14,0]
--input = [1,9,10,3,2,3,11,0,99,30,40,50]

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
   | n == 0 = newVal:xs
   | otherwise = x:replaceNth (n-1) newVal xs

doCalc :: (Int -> Int -> Int) -> Int -> [Int] -> [Int]
doCalc f i input = do
    let x = input!!(input!!(i+1))
    let y = input!!(input!!(i+2))
    let z = input!!(i+3)
    let result = f x y
    replaceNth z result input

getFuncForOpCode :: Int -> (Int -> Int -> Int)
getFuncForOpCode op
    | op == 1 = (+)
    | op == 2 = (*)

stepThroughProgram :: Int -> [Int] -> [Int]
stepThroughProgram i input = do
    let op = input!!i
    if op == 99 
        then input
        else do
            let newInput = doCalc (getFuncForOpCode op) i input 
            stepThroughProgram (i+4) newInput

updateInput :: Int -> Int -> [Int] -> [Int]
updateInput noun verb input = do    
    let newInput = replaceNth 2 verb (replaceNth 1 noun input)
    let result = (stepThroughProgram 0 newInput)!!0
    if result == 19690720
        then [100 * noun + verb]
    else [0]

a :: Int
a = do
    (stepThroughProgram 0 input)!!0

b :: [Int]
b = do
    noun <- [0..99]
    verb <- [0..99]
    head $ pure $ filter (\x -> x/= 0) $ updateInput noun verb input


