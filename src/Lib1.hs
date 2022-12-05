{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Lib1(
    State(..), emptyState, gameStart, render, mkCheck, toggle, hint, mergesort
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
emptyState = State {
    guess = [],
    ocRows = [],
    ocCols = [],
    hints = 0,
    hintCoords = []
}

-- IMPLEMENT
-- This adds game data to initial state 
gameStart :: State -> Document -> State
gameStart _ (DMap (x1:x2:x3:_)) = State {
    hints = parseDI (snd x1),
    hintCoords = [],
    ocCols = traverseList x2,
    ocRows = traverseList x3,
    guess = []
} where
    parseDI (DInteger a) = a
    parseDI _ = -1

    traverseList :: (String,Document) -> [Int]
    traverseList (_, DMap (y1:y2:_)) = traverseList y1 ++ traverseList y2
    traverseList (_ , DInteger i) = [i]
    traverseList (_, _) = []
gameStart _ _ = emptyState

-- IMPLEMENT
-- renders yougame board
render :: State -> String
--render = show
render (State g oR oC _ hC) = "     " ++ colAdder oC ++ rowAdder g oR 0
    where
        colAdder :: [Int] -> String
        colAdder [] = "\n     " ++ showChs 'A'
        colAdder (oCx:oCxs) = show oCx  ++ " " ++ colAdder oCxs

        showChs :: Char -> String
        showChs 'K' = []
        showChs ch = ch : " " ++ showChs (succ ch)

        rowAdder :: [Coord] -> [Int] -> Int -> String 
        rowAdder gx (oRx:oRxs) n 
            | n == 10 = ""
            | otherwise = "\n" ++ show oRx ++ "  " ++ show n ++ fillRow n 0 gx hC ++ rowAdder gx oRxs (n+1)
        rowAdder _ _ _ = ""
        fillRow :: Int -> Int -> [Coord] -> [Coord] -> String
        fillRow n l (gx@(Coord gc gr):gs) (h@(Coord hc hr):hs)
            | gr == hr && gc == hc = fillRow n l (gx:gs) hs
            | gr == n && (hr > gr || (hr == gr && hc > gc)) = guessAdder l gx "x" ++ fillRow n (gc+1) gs (h:hs)
            | hr == n && (hr < gr || (hr == gr && hc < gc)) = guessAdder l h "o" ++ fillRow n (hc+1) (gx:gs) hs
            | gr < n = fillRow n l gs (h:hs)
            | hr < n = fillRow n l (gx:gs) hs
            | otherwise = cycleEmpty (10-l)
        fillRow n l (gx@(Coord gc gr):gs) [] 
            | n == gr = guessAdder l gx "x" ++ fillRow n (gc+1) gs []
            | n > gr = fillRow n l gs [] 
            | otherwise = cycleEmpty (10-l)
        fillRow n l [] (h@(Coord hc hr):hs)
            | n == hr = guessAdder l h "o" ++ fillRow n (hc+1) [] hs
            | n > hr = fillRow n l [] hs
            | otherwise = cycleEmpty (10-l)
        fillRow _ l [] [] = cycleEmpty (10-l)
        guessAdder :: Int -> Coord -> String -> String
        guessAdder l (Coord c _) str = cycleEmpty (c-l) ++ str
        cycleEmpty:: Int -> String
        cycleEmpty i 
            | i <= 0 = " "
            | otherwise = " -" ++ cycleEmpty (i-1)
-- IMPLEMENT
-- Make check from current state
mkCheck :: State -> Check
mkCheck (State g _ _ _ _) = Check{coords = g}

-- IMPLEMENT
-- Toggle state's value
-- Receive raw user input tokens
toggle :: State -> [String] -> State
toggle (State g oR oC h hC) strs = State{
    guess = mergesort (g ++ addCoord strs),
    ocRows = oR,
    ocCols = oC,
    hints = h,
    hintCoords = hC
} where
    addCoord :: [String] -> [Coord]
    addCoord ((a:b:_):strxs)  = Coord (addBoth 'A' 0 a) (addBoth '0' 0 b) : addCoord strxs
    addCoord _ = []
    addBoth:: Char -> Int -> Char -> Int
    addBoth l i ch = if l == ch then i else addBoth (succ l) (i+1) ch

-- IMPLEMENT
-- Adds hint data to the game state
hint :: State -> Document -> State
hint (State g oR oC h _) (DMap ((_,DList x):_)) = State{
    guess = g,
    ocRows = oR,
    ocCols = oC,
    hints = h,
    hintCoords = mergesort (traverseList x)
} where
    traverseList :: [Document] -> [Coord]
    traverseList ((DMap (x1:x2:_)):xs) = Coord (getInt x1) (getInt x2) : traverseList xs
    traverseList _ = []
    getInt :: (String, Document) -> Int
    getInt (_,DInteger xx) = xx
    getInt _ = -1
hint s _ = s

mergesort'splitinhalf :: [Coord] -> ([Coord], [Coord])
mergesort'splitinhalf cs = (take n cs, drop n cs)
    where n = div (length cs) 2 
mergesort'merge :: [Coord] -> [Coord] -> [Coord]
mergesort'merge [] cs = cs
mergesort'merge cs [] = cs
mergesort'merge (x@(Coord cx rx):xs) (y@(Coord cy ry):ys)
    | rx < ry || (rx == ry && cx < cy) = x : mergesort'merge xs (y:ys)
    | rx == ry && cx == cy = mergesort'merge xs ys
    | rx > 9 || cx > 9  = mergesort'merge xs (y:ys)
    | otherwise = y : mergesort'merge (x:xs) ys
mergesort :: [Coord] -> [Coord]
mergesort cs
    | length cs > 1 = mergesort'merge (mergesort ls) (mergesort rs)
    | otherwise = cs
    where (ls, rs) = mergesort'splitinhalf cs