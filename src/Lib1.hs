{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Lib1(
    State, emptyState, gameStart, render, mkCheck, toggle, hint
) where

import Types

-- This is a state of your game.
-- It must contain all values you might need during a game:
-- number of occupied rows/cols, hints, occupied cells,..
-- You can change the right hand side as you wish but please
-- keep the type name as is

-- ship head    ship tail     oc cols    oc rows
data State = State {
    guess :: [Coord],
    ocRows :: [Int],
    ocCols :: [Int],
    hints :: Int
    }
    deriving Show

-- IMPLEMENT
-- This is very initial state of your program
emptyState :: State
emptyState = State {
    guess = [],
    ocRows = [],
    ocCols = [],
    hints = -1
}

-- IMPLEMENT
-- This adds game data to initial state 
gameStart :: State -> Document -> State
gameStart _ (DMap (x1:x2:x3:_)) = State {
    hints = parseDI (snd x1),
    ocCols = traverseList x2,
    ocRows = traverseList x3,
    guess = []
} where
    parseDI (DInteger a) = a

    traverseList :: (String,Document) -> [Int]
    traverseList (_, DMap (x1:x2:_)) = traverseList x1 ++ traverseList x2
    traverseList (_ , DInteger i) = [i]
    traverseList (_, _) = []
gameStart _ _ = emptyState

-- IMPLEMENT
-- renders your game board
render :: State -> String
render = show


--render (State _ oR oC _) = "  " ++ colAdder oC ++ rowAdder oR
    --where
        --colAdder :: [int] -> String
        --colAdder [] = ""
        --colAdder (oC:oCs) = (show oC) : " " : (colAdder oCs)
        
        

-- IMPLEMENT
-- Make check from current state
mkCheck :: State -> Check
mkCheck _ = Check []

-- IMPLEMENT
-- Toggle state's value
-- Receive raw user input tokens
toggle :: State -> [String] -> State
toggle l t = l

-- IMPLEMENT
-- Adds hint data to the game state
hint :: State -> Document -> State
hint l h = l