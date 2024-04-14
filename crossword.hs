{-
    FROM: https://www.hackerrank.com/challenges/crosswords-101/problem?isFullScreen=true
    Given inputs of a crossword grid and a list of words, the task is to place the words in the grid.
    (Sample Inputs & Outputs at the bottom of this file.)
-}

module Main where

import Control.Monad.Writer
import Data.List (group, intersect, permutations)
import Data.List.Split (splitOn)

import Data.Map qualified as Map
import Data.Maybe (mapMaybe)

type Log = [String]

data Direction = ACROSS | DOWN deriving (Show, Eq, Ord)

data Slot = Slot
    { start :: (Int, Int) -- (row, column)
    , direction :: Direction
    , lengthSlot :: Int
    }
    deriving (Show, Eq)

-- will be part of a Map later, so need Ord too
instance Ord Slot where
    compare s1 s2 = compare (start s1, direction s1, lengthSlot s1) (start s2, direction s2, lengthSlot s2)

data Constraint = Constraint
    { slot1 :: Slot
    , pos1 :: Int
    , slot2 :: Slot
    , pos2 :: Int
    }
    deriving (Show, Eq)

data Fill = Fill
    { word :: String
    , fillLength :: Int
    }
    deriving (Show, Eq)

findSlots :: String -> [(Int, Int)]
findSlots rc = findSlots' 0 [] (group rc)
  where
    findSlots' :: Int -> [(Int, Int)] -> [String] -> [(Int, Int)]
    findSlots' cursor acc [] = acc
    findSlots' cursor acc (x : xs)
        | head x == '-' =
            if Prelude.length x == 1
                then findSlots' (cursor + 1) acc xs
                else findSlots' (cursor + Prelude.length x) ((cursor, Prelude.length x) : acc) xs
        | otherwise = findSlots' (cursor + Prelude.length x) acc xs

-- Direction, Index, Found Slots (row, col) -> [Slot]
createSlotRecs :: Direction -> Int -> [(Int, Int)] -> [Slot]
createSlotRecs _ _ [] = []
createSlotRecs ACROSS index fs = [Slot (index, x) ACROSS len | (x, len) <- fs]
createSlotRecs DOWN index fs = [Slot (y, index) DOWN len | (y, len) <- fs]

xSlots :: [Slot] -> [(Slot, Slot)]
xSlots slots =
    [ (x1, x2)
    | x1 <- slots
    , x2 <- slots
    , direction x1 == ACROSS
    , direction x2 == DOWN
    ]

-- detectConstraint :: Slot -> Slot -> Writer Log (Maybe Constraint)
detectConstraint :: Slot -> Slot -> Maybe Constraint
detectConstraint s1 s2 = do
    -- tell ["Detecting constraint between slots: " ++ show s1 ++ " and " ++ show s2]
    if direction s1 /= ACROSS || direction s2 /= DOWN
        then do
            -- tell ["Invalid directions"]
            -- return Nothing
            Nothing
        else do
            let rowSet = [(fst (start s1), ry) | ry <- [snd (start s1) .. snd (start s1) + lengthSlot s1 - 1]]
                colSet = [(cx, snd (start s2)) | cx <- [fst (start s2) .. fst (start s2) + lengthSlot s2 - 1]]
                common = rowSet `intersect` colSet
             in do
                    -- tell ["Row set: " ++ show rowSet ++ ", Col set: " ++ show colSet ++ ", Common: " ++ show common]
                    if null common
                        then do
                            -- tell ["No common points"]
                            -- return Nothing
                            Nothing
                        else do
                            -- tell ["Common points: " ++ show common]
                            -- return $ Just $ Constraint s1 (snd (head common) - snd (start s1)) s2 (fst (head common) - fst (start s2))
                            Just $ Constraint s1 (snd (head common) - snd (start s1)) s2 (fst (head common) - fst (start s2))

fitCheck :: (Slot, Fill) -> Bool
fitCheck (slot, fill) = lengthSlot slot == fillLength fill

-- handle check constraints against one option set
satisfiesConstraints :: [Constraint] -> [(Slot, Fill)] -> Bool
satisfiesConstraints constraints sfs =
    let s2f = Map.fromList sfs
        results =
            and
                [ f1c == f2c
                | con <- constraints
                , let s1 = slot1 con
                , let p1 = pos1 con
                , let f1 = s2f Map.! s1
                , let f1c = word f1 !! p1
                , let s2 = slot2 con
                , let p2 = pos2 con
                , let f2 = s2f Map.! s2
                , let f2c = word f2 !! p2
                ]
     in results

solve :: [Slot] -> [Fill] -> [Constraint] -> [[(Slot, Fill)]]
solve slots fills constraints =
    let
        optionset = permutations fills
        z = map (zip slots) optionset
        z' = filter (all fitCheck) z
     in
        filter (satisfiesConstraints constraints) z'

pp :: [[Char]] -> IO ()
pp = mapM_ putStrLn

insert2D :: [[Char]] -> (Int, Int, Char) -> [[Char]]
insert2D x (r, c, e) = take r x ++ [take c (x !! r) ++ [e] ++ drop (c + 1) (x !! r)] ++ drop (r + 1) x

coordLetters :: Int -> Int -> Direction -> String -> [(Int, Int, Char)]
coordLetters r c d w = coordLetters' acc r c d w
  where
    acc = []
    coordLetters' acc r c d [] = acc
    coordLetters' acc r c d (l : ls) =
        case d of
            ACROSS -> coordLetters' ((r, c, l) : acc) r (c + 1) d ls
            DOWN -> coordLetters' ((r, c, l) : acc) (r + 1) c d ls

insertWordAt :: [[Char]] -> (Int, Int) -> Direction -> String -> [[Char]]
insertWordAt cw (r, c) d w = foldl insert2D cw (coordLetters r c d w)

displaySolution :: [(Slot, Fill)] -> IO ()
displaySolution sfs = do
    let m = replicate 10 (replicate 10 '+')
    let m' = foldl (\acc (s, f) -> insertWordAt acc (start s) (direction s) (word f)) m sfs
    pp m'

main :: IO ()
main = do
    -- raw input processing
    input <- getContents
    let game = lines input

    -- Determine the slots for words from the map
    let rows = take 10 game
    let cols = [[row !! i | row <- rows] | i <- [0 .. 9]]

    let rowSlots = [createSlotRecs ACROSS i $ findSlots $ rows !! i | i <- [0 .. 9]]
    let colSlots = [createSlotRecs DOWN i $ findSlots $ cols !! i | i <- [0 .. 9]]
    let allSlots = concat $ filter (not . null) $ rowSlots ++ colSlots

    -- Determine the constraints imposed by slots
    let constraints = mapMaybe (uncurry detectConstraint) $ xSlots allSlots

    -- grab word list (11th line)
    let fills = map (\x -> Fill x (Prelude.length x)) $ splitOn ";" $ head $ words $ game !! 10

    -- solve
    let solutions = solve allSlots fills constraints

    -- display as crossword
    displaySolution $ head solutions

    return ()

{-
INPUT 1
+-++++++++
+-++++++++
+-++++++++
+-----++++
+-+++-++++
+-+++-++++
+++++-++++
++------++
+++++-++++
+++++-++++
LONDON;DELHI;ICELAND;ANKARA

OUTPUT 1
+L++++++++
+O++++++++
+N++++++++
+DELHI++++
+O+++C++++
+N+++E++++
+++++L++++
++ANKARA++
+++++N++++
+++++D++++

INPUT 2
+-++++++++
+-++++++++
+-------++
+-++++++++
+-++++++++
+------+++
+-+++-++++
+++++-++++
+++++-++++
++++++++++
AGRA;NORWAY;ENGLAND;GWALIOR

OUTPUT 2
+E++++++++
+N++++++++
+GWALIOR++
+L++++++++
+A++++++++
+NORWAY+++
+D+++G++++
+++++R++++
+++++A++++
++++++++++

-}