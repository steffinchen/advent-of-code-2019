module Day5
( a, b
) where
    
import Debug.Trace 
import Data.Char

input :: [Int]
input = [3,225,1,225,6,6,1100,1,238,225,104,0,1101,81,30,225,1102,9,63,225,1001,92,45,224,101,-83,224,224,4,224,102,8,223,223,101,2,224,224,1,224,223,223,1102,41,38,225,1002,165,73,224,101,-2920,224,224,4,224,102,8,223,223,101,4,224,224,1,223,224,223,1101,18,14,224,1001,224,-32,224,4,224,1002,223,8,223,101,3,224,224,1,224,223,223,1101,67,38,225,1102,54,62,224,1001,224,-3348,224,4,224,1002,223,8,223,1001,224,1,224,1,224,223,223,1,161,169,224,101,-62,224,224,4,224,1002,223,8,223,101,1,224,224,1,223,224,223,2,14,18,224,1001,224,-1890,224,4,224,1002,223,8,223,101,3,224,224,1,223,224,223,1101,20,25,225,1102,40,11,225,1102,42,58,225,101,76,217,224,101,-153,224,224,4,224,102,8,223,223,1001,224,5,224,1,224,223,223,102,11,43,224,1001,224,-451,224,4,224,1002,223,8,223,101,6,224,224,1,223,224,223,1102,77,23,225,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,8,226,677,224,1002,223,2,223,1006,224,329,1001,223,1,223,7,226,226,224,102,2,223,223,1006,224,344,101,1,223,223,108,677,677,224,1002,223,2,223,1006,224,359,101,1,223,223,1107,226,677,224,1002,223,2,223,1005,224,374,101,1,223,223,1008,677,226,224,1002,223,2,223,1005,224,389,101,1,223,223,1007,677,226,224,1002,223,2,223,1005,224,404,1001,223,1,223,1107,677,226,224,1002,223,2,223,1005,224,419,1001,223,1,223,108,677,226,224,102,2,223,223,1006,224,434,1001,223,1,223,7,226,677,224,102,2,223,223,1005,224,449,1001,223,1,223,107,226,226,224,102,2,223,223,1006,224,464,101,1,223,223,107,677,226,224,102,2,223,223,1006,224,479,101,1,223,223,1007,677,677,224,1002,223,2,223,1006,224,494,1001,223,1,223,1008,226,226,224,1002,223,2,223,1006,224,509,101,1,223,223,7,677,226,224,1002,223,2,223,1006,224,524,1001,223,1,223,1007,226,226,224,102,2,223,223,1006,224,539,101,1,223,223,8,677,226,224,1002,223,2,223,1006,224,554,101,1,223,223,1008,677,677,224,102,2,223,223,1006,224,569,101,1,223,223,1108,677,226,224,102,2,223,223,1005,224,584,101,1,223,223,107,677,677,224,102,2,223,223,1006,224,599,1001,223,1,223,1108,677,677,224,1002,223,2,223,1006,224,614,1001,223,1,223,1107,677,677,224,1002,223,2,223,1005,224,629,1001,223,1,223,108,226,226,224,1002,223,2,223,1005,224,644,101,1,223,223,8,226,226,224,1002,223,2,223,1005,224,659,101,1,223,223,1108,226,677,224,1002,223,2,223,1006,224,674,101,1,223,223,4,223,99,226]
--input = [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]

data State = State {program :: [Int], inp:: Int, output :: [Int], counter :: Int} deriving (Show) 
data ParameterMode = Position | Immediate deriving (Show)

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
   | n == 0 = newVal:xs
   | otherwise = x:replaceNth (n-1) newVal xs

digits :: Int -> [Int]
digits = map digitToInt . show

getWithDefault :: [a] -> a -> Int -> a
getWithDefault list def index = do
    if index >= (length list) 
        then def
    else list!!index

getParameterValue :: [Int] -> Int -> [Int] -> Int -> Int
getParameterValue program i modes position = do
    let mode = getWithDefault modes 0 position
    if mode == 0
        then program!!(program!!(i+position+1))
    else program!!(i+position+1)


doCalc :: (Int -> Int -> Int) -> State -> [Int] -> State
doCalc f state parameterModes = do
    let p = (program state)
    let i = (counter state)
    let x = getParameterValue p i parameterModes 0 
    let y = getParameterValue p i parameterModes 1 
    let z = p!!(i+3)
    let result = f x y
    let newProgram = replaceNth z result p
    state {program = newProgram, counter = i+4}

getFuncForOpCode :: Int -> (Int -> Int -> Int)
getFuncForOpCode op
    | op == 1 = (+)
    | op == 2 = (*)

parseOp :: State -> (Int, [Int])
parseOp state = do
    let instruction = ((program state)!!(counter state)) 
    let op = instruction `mod` 100
    let modes = reverse $ digits (instruction `div` 100)
    ( op, modes)

saveOutput :: State -> [Int] -> State    
saveOutput state modes = do
    let param = getParameterValue (program state) (counter state) modes 0
    state {counter = (counter state) + 2, output = (output state) ++ [param]}

saveInput :: State -> [Int] -> State    
saveInput state modes = do
    let targetPosition = (program state)!!((counter state)+1)
    let newProgram = replaceNth targetPosition (inp state) (program state)
    state {program = newProgram, counter = (counter state) + 2}

jumpIf :: (Int -> Bool) -> State -> [Int] -> State   
jumpIf cond state modes = do
    let firstParam =  getParameterValue (program state) (counter state) modes 0
    if (cond firstParam)  then do
        let sndParam = getParameterValue (program state) (counter state) modes 1
        state {counter = sndParam}
    else state {counter = (counter state) + 3}

compare2 :: (Int -> Int -> Bool) -> State -> [Int] -> State   
compare2 comp state modes = do
    let firstParam =  getParameterValue (program state) (counter state) modes 0
    let sndParam = getParameterValue (program state) (counter state) modes 1
    let thirdParam = (program state)!!((counter state)+ 3)
    if (comp firstParam sndParam) then
        state {program = (replaceNth thirdParam 1 (program state)), counter = (counter state) + 4}
    else state {program = (replaceNth thirdParam 0 (program state)), counter = (counter state) + 4}
    
stepThroughProgram :: State -> State
stepThroughProgram state = do
    let (op, modes) = parseOp (traceShow state state)
    case op of 
        99 -> state
        3 -> do 
            let newState = saveInput state modes
            stepThroughProgram (newState)
        4 -> do
            let newState = saveOutput state modes
            stepThroughProgram (newState)
        5 -> do 
            let newState = jumpIf (/= 0) state modes
            stepThroughProgram (newState)
        6 -> do
            let newState = jumpIf (== 0) state modes
            stepThroughProgram (newState)
        7 -> do 
            let newState = compare2 (<) state modes
            stepThroughProgram (newState)
        8 -> do
            let newState = compare2 (==) state modes
            stepThroughProgram (newState)
        _ -> do
            let newState = doCalc (getFuncForOpCode op) state modes 
            stepThroughProgram (newState)


a :: State
a = do  
    stepThroughProgram (State input 1 [] 0)

b :: State
b = do  
    stepThroughProgram (State input 5 [] 0)

