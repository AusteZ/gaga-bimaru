{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Lib2(renderDocument, hint, gameStart) where

import Types
import Lib1 (State(..))

-- IMPLEMENT
-- First, make Check an instance of ToDocument class
instance ToDocument Check where
    toDocument (Check x) = DMap [("coords", DList (parseList x))]
        where
        parseList :: [Coord] -> [Document]
        parseList ((Coord z y):cs) = (DMap [("col", DInteger z),("row", DInteger y)]) : parseList cs
        parseList [] = []

-- IMPLEMENT
-- Renders document to yaml

renderDocument :: Document -> String
renderDocument d = formatYaml False 0 d ++ "\n"

formatYaml :: Bool -> Int -> Document -> String
formatYaml _ _ (DMap []) = "{}"
formatYaml l nr (DMap xs) = init (formatDMap l nr xs)
formatYaml _ _ (DList []) = "[]"
formatYaml l nr (DList xs) = init (formatDList l (if nr == 0 then 1 else nr) xs)
formatYaml _ _ (DInteger x) = show x
formatYaml _ _ (DString x) = "\'" ++ x ++ "\'"
formatYaml _ _ DNull = "null"

formatDMap :: Bool -> Int -> [(String, Document)] -> String
formatDMap l nr ((key, x):xs) = (if l then "" else prefix nr l) ++ (if key == "" then "''" else key) ++ ":" ++ (if checkDocType x 
    then " " else "\n") ++ formatYaml False (nr+1) x ++ "\n" ++ formatDMap False nr xs
formatDMap _ _ [] = ""

formatDList :: Bool -> Int -> [Document] -> String
formatDList l nr (x:xs) = (if l then "- " else prefix nr True) ++ formatYaml True (checkDList x nr) x ++ "\n" ++ formatDList False nr xs
formatDList _ _ [] = ""

prefix :: Int -> Bool -> String
prefix nr l = if nr == 0 then "" else if l then (generateWS (nr-1)) ++ "- " else generateWS nr

generateWS :: Int -> String
generateWS nr = if nr > 0 then take (nr*2) (cycle " ") else ""

checkDList :: Document -> Int -> Int
checkDList (DList _) nr = nr+1
checkDList _ nr = nr

checkDocType :: Document -> Bool
checkDocType (DMap []) = True
checkDocType (DList []) = True
checkDocType (DMap _) = False
checkDocType (DList _) = False
checkDocType _ = True 

-- IMPLEMENT
-- This adds game data to initial state
-- Errors are reported via Either but not error 
gameStart :: State -> Document -> Either String State
gameStart (State g _ _ _ hC) (DMap (z1:z2:z3:_)) = do
    oR <- validate "occupied_rows" z3
    oC <- validate "occupied_cols" z2
    h <- hintparse z1
    return (State g oR oC h hC)
    where
        validate :: String -> (String,Document) -> Either String [Int]
        validate check (seccheck, DMap(x1:x2:_)) = if check == seccheck
            then traverseList 10 x1 x2
            else Left ("The input from server is not labeled as expected (" ++ check ++ ")")
        validate _ _ = Left "Unexpected input in DMap (a DInteger and second another DMap or DNull)"
        hintparse :: (String, Document) -> Either String Int
        hintparse (check, DInteger num) = if check == "number_of_hints"
            then Right num else Left "The input from server is not labeled as expected (number_of_hints)"
        hintparse _ = Left "Error occured while parsing input from the server in gameStart"

        traverseList :: Int -> (String,Document) -> (String,Document) -> Either String [Int]
        traverseList nr (_, DInteger num) (_, DMap(x1:x2:_)) = do
            let list = traverseList (nr-1) x1 x2
            case list of
                Right a -> return (num : a)
                Left b -> Left b
        traverseList nr (_, DInteger num) (_, DNull) = if (nr-1) == 0 then Right [num]
        else Left ("The number of coordinates passed was too " ++ if nr < 0 then "high" else "low")
        traverseList _ _ _ = Left "Unexpected input in DMap (a DInteger and second another DMap or DNull)"
gameStart _ _ = Left "Error occured while parsing input from the server in gameStart"
-- IMPLEMENT
-- Adds hint data to the game state
-- Errors are reported via Either but not error 
hint :: State -> Document -> Either String State
hint (State g oR oC h _) (DMap ((_, DList ds):_)) = do
    hC <- traverseList ds
    return (State g oR oC h hC)
    where
        traverseList :: [Document] -> Either String [Coord]
        traverseList ((DMap ((_, DInteger num1):(_, DInteger num2):_)):xs) = do
            let list = traverseList xs
            case list of
                Right a -> return (mergesort((Coord num1 num2) : a))
                Left b -> Left b
        traverseList [] = Right []
        traverseList _ = Left "Error occured while parsing input from the server in hint"
hint _ _ = Left "Error occured while parsing input from the server in hint"



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