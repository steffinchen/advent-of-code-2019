module Day10
( a,b
) where

import Data.List
import Data.List.Index
import Debug.Trace
import Data.Ord
import qualified Data.Map.Strict as Map

data Coord = Coord { x:: Int, y :: Int, visibleAsteroids:: [(Int, Int)]} deriving (Show, Eq)

input :: [String]
input = ["#.#................#..............#......#......", ".......##..#..#....#.#.....##...#.........#.#...", ".#...............#....#.##......................", "......#..####.........#....#.......#..#.....#...", ".....#............#......#................#.#...", "....##...#.#.#.#.............#..#.#.......#.....", "..#.#.........#....#..#.#.........####..........", "....#...#.#...####..#..#..#.....#...............", ".............#......#..........#...........#....", "......#.#.........#...............#.............", "..#......#..#.....##...##.....#....#.#......#...", "...#.......##.........#.#..#......#........#.#..", "#.............#..........#....#.#.....#.........", "#......#.#................#.......#..#.#........", "#..#.#.....#.....###..#.................#..#....", "...............................#..........#.....", "###.#.....#.....#.............#.......#....#....", ".#.....#.........#.....#....#...................", "........#....................#..#...............", ".....#...#.##......#............#......#.....#..", "..#..#..............#..#..#.##........#.........", "..#.#...#.......#....##...#........#...#.#....#.", ".....#.#..####...........#.##....#....#......#..", ".....#..#..##...............................#...", ".#....#..#......#.#............#........##...#..", ".......#.....................#..#....#.....#....", "#......#..###...........#.#....#......#.........", "..............#..#.#...#.......#..#.#...#......#", ".......#...........#.....#...#.............#.#..", "..##..##.............#........#........#........", "......#.............##..#.........#...#.#.#.....", "#........#.........#...#.....#................#.", "...#.#...........#.....#.........#......##......", "..#..#...........#..........#...................", ".........#..#.......................#.#.........", "......#.#.#.....#...........#...............#...", "......#.##...........#....#............#........", "#...........##.#.#........##...........##.......", "......#....#..#.......#.....#.#.......#.##......", ".#....#......#..............#.......#...........", "......##.#..........#..................#........", "......##.##...#..#........#............#........", "..#.....#.................###...#.....###.#..#..", "....##...............#....#..................#..", ".....#................#.#.#.......#..........#..", "#........................#.##..........#....##..", ".#.........#.#.#...#...#....#........#..#.......", "...#..#.#......................#...............#", ""]

parseInput :: [String] -> [Coord]
parseInput input = concat $ imap mapLine input

mapLine :: Int -> String -> [Coord]
mapLine y = ifoldl (charToCoord y) []

charToCoord :: Int -> [Coord] -> Int -> Char -> [Coord]
charToCoord y acc x el = if el == '#' then acc ++ [Coord x y []] else acc

countDetectableAsteroids :: [Coord] -> Coord ->  Coord
countDetectableAsteroids allAsteroids coord = 
    foldl (checkAsteroid allAsteroids) coord (delete coord allAsteroids)

checkAsteroid :: [Coord] -> Coord -> Coord -> Coord
checkAsteroid allAsteroids current asteroidToCheck = do 
    let seenAsteroids = visibleAsteroids current
    let visible = canSee current asteroidToCheck allAsteroids
    if visible then current {visibleAsteroids = (x asteroidToCheck, y asteroidToCheck) : seenAsteroids}
    else current

canSee :: Coord -> Coord -> [Coord] -> Bool
canSee a b allAsteroids = do
    let (dx, dy) = calcOffset a b
    let intermediatePointCount = gcd dx dy - 1
    (intermediatePointCount == 0) ||
        (do
            let intermediatePoints = map (calcIntermediatePoint a (dx, dy)) [1..intermediatePointCount]
            let asteroidsInTheWay = allAsteroids `intersect` intermediatePoints
            length asteroidsInTheWay == 0)

calcIntermediatePoint :: Coord -> (Int, Int) -> Int -> Coord
calcIntermediatePoint Coord {x=x, y=y} (dx,dy) count = 
    Coord (x + (dx `div` gcd dx dy * count)) (y + (dy `div` gcd dx dy * count)) []


calcOffset :: Coord -> Coord -> (Int, Int)
calcOffset a b = (x b - x a, y b - y a)

angleBetween :: Coord -> (Int, Int) -> Float
angleBetween Coord {x=x1,y=y1} (x2,y2) = do
    let (dx,dy) = (x2-x1, y1-y2)
    let angle = atan2 (fromIntegral dx) (fromIntegral dy)
    if angle < 0 then (2*pi) + angle else angle

keepVaporizing :: [(Int, Int)] -> Coord -> Int -> (Int, Int)
keepVaporizing [a] _ _ = a
keepVaporizing asteroids station count = do
    let currentlyVisible = visibleAsteroids $ countDetectableAsteroids (map (\(x,y) -> Coord x y []) asteroids) station
    let vaporized = foldl (\acc point -> Map.insert (angleBetween station point) point acc) Map.empty currentlyVisible
    let newAsteroids = asteroids \\ Map.elems vaporized
    let totalVaporizedCount = count + (Map.size vaporized)
    if totalVaporizedCount >= 200 
        then snd $ Map.elemAt ( 200 - count -1) vaporized
        else keepVaporizing newAsteroids station totalVaporizedCount

a :: Int
a = do
    let asteroids = parseInput input
    maximum $ map (length . visibleAsteroids . countDetectableAsteroids asteroids) asteroids

b :: (Int, Int)
b = do
    let asteroids = parseInput input
    -- from part a
    let station = Coord 37 25 []
    keepVaporizing (map (\Coord{x=x, y=y} -> (x,y)) asteroids) station 0
