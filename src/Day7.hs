module Day7
( a, b
) where
    
import Debug.Trace 
import Data.Char
import Data.List

input :: [Int]
input = [3,8,1001,8,10,8,105,1,0,0,21,46,67,76,97,118,199,280,361,442,99999,3,9,1002,9,3,9,101,4,9,9,102,3,9,9,1001,9,3,9,1002,9,2,9,4,9,99,3,9,102,2,9,9,101,5,9,9,1002,9,2,9,101,2,9,9,4,9,99,3,9,101,4,9,9,4,9,99,3,9,1001,9,4,9,102,2,9,9,1001,9,4,9,1002,9,5,9,4,9,99,3,9,102,3,9,9,1001,9,2,9,1002,9,3,9,1001,9,3,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,99]
--input = [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]

data State = State {
    program :: [Int], 
    inp:: [Int], 
    output :: [Int], 
    counter :: Int,
    isFeedbackLoopMode :: Bool
    } deriving (Show) 
data ParameterMode = Position | Immediate deriving (Show)

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
   | n == 0 = newVal:xs
   | otherwise = x:replaceNth (n-1) newVal xs

digits :: Int -> [Int]
digits = map digitToInt . show

getWithDefault :: [a] -> a -> Int -> a
getWithDefault list def index = if index >= length list then def else list!!index

getParameterValue :: [Int] -> Int -> [Int] -> Int -> Int
getParameterValue program i modes position = do
    let mode = getWithDefault modes 0 position
    if mode == 0
        then program!!(program!!(i+position+1))
    else program!!(i+position+1)


doCalc :: (Int -> Int -> Int) -> State -> [Int] -> State
doCalc f state parameterModes = do
    let p = program state
    let i = counter state
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
    let instruction = program state !! counter state
    let op = instruction `mod` 100
    let modes = reverse $ digits (instruction `div` 100)
    ( op, modes)

saveOutput :: State -> [Int] -> State    
saveOutput state modes = do
    let param = getParameterValue (program state) (counter state) modes 0
    state {counter = counter state + 2, output = output state ++ [param]}

saveInput :: State -> [Int] -> State    
saveInput state modes = do
    let targetPosition = program state!!(counter state+1)
    let (nextInput:otherInputs) = inp state
    let newProgram = replaceNth targetPosition nextInput (program state)
    state {program = newProgram, counter = counter state + 2, inp = otherInputs}

jumpIf :: (Int -> Bool) -> State -> [Int] -> State   
jumpIf cond state modes = do
    let firstParam =  getParameterValue (program state) (counter state) modes 0
    if cond firstParam  then do
        let sndParam = getParameterValue (program state) (counter state) modes 1
        state {counter = sndParam}
    else state {counter = counter state + 3}

compare2 :: (Int -> Int -> Bool) -> State -> [Int] -> State   
compare2 comp state modes = do
    let firstParam =  getParameterValue (program state) (counter state) modes 0
    let sndParam = getParameterValue (program state) (counter state) modes 1
    let thirdParam = program state !! counter state + 3
    if comp firstParam sndParam then
        state {program = replaceNth thirdParam 1 (program state), counter = counter state + 4}
    else state {program = replaceNth thirdParam 0 (program state), counter = counter state + 4}
    
stepThroughProgram :: State -> State
stepThroughProgram state = do
    let (op, modes) = parseOp state
    case op of 
        99 -> state
        3 -> do 
            let newState = saveInput state modes
            stepThroughProgram newState
        4 -> do
            let newState = saveOutput state modes
            if isFeedbackLoopMode state then newState else stepThroughProgram newState
        5 -> do 
            let newState = jumpIf (/= 0) state modes
            stepThroughProgram newState
        6 -> do
            let newState = jumpIf (== 0) state modes
            stepThroughProgram newState
        7 -> do 
            let newState = compare2 (<) state modes
            stepThroughProgram newState
        8 -> do
            let newState = compare2 (==) state modes
            stepThroughProgram newState
        _ -> do
            let newState = doCalc (getFuncForOpCode op) state modes 
            stepThroughProgram newState

runAmplifierSingle :: Int -> Int -> Int
runAmplifierSingle previousOutput phaseSetting  = 
    head $ output $ stepThroughProgram (State input [phaseSetting, previousOutput] [] 0 False)


a :: Int
a = do  
    let allPhaseSettingSequence = permutations [0..4]
    maximum $ map (foldl runAmplifierSingle 0) allPhaseSettingSequence

b :: Int
b = 0

