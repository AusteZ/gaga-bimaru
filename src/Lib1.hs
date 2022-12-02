{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Lib1(
    State(..), emptyState, gameStart, render, mkCheck, toggle, hint
) where

import Types

-- This is a state of your game.
-- It must contain all values you might need during a game:
-- number of occupied rows/cols, hints, occupied cells,..
-- You can change the right hand side as you wish but please
-- keep the type name as is
data State = State {
    guess :: [Coord],
    ocRows :: [Int],
    ocCols :: [Int],
    hints :: Int,
    hintCoords :: [Coord]
    }
    deriving (Show, Eq)

-- IMPLEMENT
-- This is very initial state of your program
emptyState :: State
emptyState = (State [] [] [] 0 [])

-- IMPLEMENT
-- This adds game data to initial state 
gameStart :: State -> Document -> State
gameStart _ _ = emptyState
-- IMPLEMENT
-- renders your game board
render :: State -> String
--render = show
render (State g oR oC h hC) = init (glueStrings (inputGuesses 'x' g (inputGuesses 'o' hC (createBlankBoard oC oR)))) 

createBlankBoard :: [Int] -> [Int] -> [String]
createBlankBoard oC oR = ("   " ++ createHeaderOcc oC) : ("    A B C D E F G H I J") : createBlankLines oR 0
createHeaderOcc :: [Int] -> String
createHeaderOcc (oc:ocs) = " " ++ (show oc) ++ createHeaderOcc ocs
createHeaderOcc [] = []
createBlankLines :: [Int] -> Int -> [String]
createBlankLines (or:ors) nr = (show or ++ " " ++ show nr ++ " - - - - - - - - - -") : createBlankLines ors (nr+1)
createBlankLines [] _ = []

inputGuesses :: Char -> [Coord] -> [String] -> [String]
inputGuesses ch ((Coord c r):cs) strs = inputGuesses ch cs (findRow ch (-2) c r strs)
inputGuesses _ [] strs = strs
    
    
    --(strs!!r)!!c == '-' then if r == 0 then (take 4 (head strs) ++ (putInCol 0 c (drop 4 (head strs)))) : tail strs
--else strs--((take (r-1) strs) ++ (take 4 strs!!r ++ (putInCol 0 c (drop 4 (strs!!r)))) ++ (drop r strs) ++ [])
--else strs)
--inputGuesses _ strs = strs

findRow :: Char -> Int -> Int -> Int -> [String] -> [String]
findRow ch nr c r (str:strs) = if nr == r then (findCol ch (-2) c str) : strs else str:(findRow ch (nr+1) c r strs)
findRow _ _ _ _ [] = []
findCol :: Char -> Int -> Int -> String -> String
findCol ch nr c (ch1:ch2:str) = if nr == c then ch : ch2 : str else ch1:ch2:(findCol ch (nr+1) c str)
findCol _ _ _ [] = []

glueStrings :: [String] -> String
glueStrings (str:strs) = str ++ "\n" ++ glueStrings strs
glueStrings [] = ""

-- IMPLEMENT
-- Make check from current state
mkCheck :: State -> Check
mkCheck _ = Check []

-- IMPLEMENT
-- Toggle state's value
-- Receive raw user input tokens
toggle :: State -> [String] -> State
toggle (State g oC oR h hC) guesses = (State (removeDublicates (g ++ (addCoords guesses))) oC oR h hC)

addCoords :: [String] -> [Coord]
addCoords ((a:b:[]):strxs)  = (if elem a ['A'..'J'] && elem b ['0'..'9']
    then [(Coord (addBoth 'A' 0 a) (addBoth '0' 0 b))] else []) ++ addCoords strxs
addCoords (_:strxs) = addCoords strxs
addCoords [] = []
addCoords _ = []
addBoth:: Char -> Int -> Char -> Int
addBoth l i ch = if l == ch then i else addBoth (succ l) (i+1) ch 

removeDublicates :: [Coord] -> [Coord]
removeDublicates (c:all) = if elem c all then removeDublicates (coordEquals c all)
else c:(removeDublicates all)
removeDublicates c = c

coordEquals :: Coord -> [Coord] -> [Coord]
coordEquals c (e:all) = if c == e then all else e:(coordEquals c (all))
coordEquals _ [] = []
-- IMPLEMENT
-- Adds hint data to the game state
hint :: State -> Document -> State
hint _ _ = emptyState