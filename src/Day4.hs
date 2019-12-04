module Day4
( a,b
) where

import Data.Char
import Control.Monad
import Control.Monad.Reader
import Data.List
import qualified Data.Map as Map
    
start = 271973
end = 785961

digits :: Int -> [Int]
digits = map digitToInt . show

hasAdjacentSameDigits :: Int -> Bool
hasAdjacentSameDigits x = snd (foldl compareDigits (-1, False) (digits x) )

compareDigits :: (Int, Bool) -> Int -> (Int, Bool)
compareDigits (x, True) _ = (x, True)
compareDigits (x, False) d = if d == x then (d, True) else (d, False)


digitsSameOrIncreaseFromLeft :: Int -> Bool
digitsSameOrIncreaseFromLeft x = do
    (digits x) == sort (digits x)

hasTwoSameDigits :: Int -> Bool
hasTwoSameDigits x = do
    let count = foldl countDigits Map.empty (digits x)
    let filtered = (Map.filter (== 2) count)
    (Map.size filtered) >= 1

countDigits :: Map.Map Int Int -> Int -> Map.Map Int Int
countDigits acc x = if (Map.member x acc) then (Map.adjust (+1) x acc) else (Map.insert x 1 acc)

b = do
    length $ filter (liftM2 (&&) hasTwoSameDigits digitsSameOrIncreaseFromLeft) [start..end]
    
a = do
    length $ filter (liftM2 (&&) hasAdjacentSameDigits digitsSameOrIncreaseFromLeft) [start..end]


