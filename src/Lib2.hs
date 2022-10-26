{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Lib2(renderDocument, hint, gameStart) where

import Lib1 (State(..), mergesort)
import Types


-- IMPLEMENT
-- First, make Check an instance of ToDocument class

-- IMPLEMENT
-- Renders document to yaml
renderDocument :: Document -> String
renderDocument (DList ds) = "{coords: [" ++ init (init (coordToYaml ds)) ++ "]}" where
    coordToYaml :: [Document] -> String
    coordToYaml ((DInteger x):(DInteger y):ds) = "{col: " ++ show x ++ ", row: " ++ show y ++ "}, " ++ coordToYaml ds
    coordToYaml [] = ""
-- IMPLEMENT
-- This adds game data to initial state
-- Errors are reported via Either but not error 
gameStart :: State -> Document -> Either String State
--  gameStart s _ = Right s
gameStart (State g _ _ _ hC) (DMap (x1:x2:x3:_)) = do
    oR <- validate "occupied_rows" x3
    oC <- validate "occupied_cols" x2
    h <- hintparse x1
    return (State g oR oC h hC)
    where
        validate :: String -> (String,Document) -> Either String [Int]
        validate check (seccheck, DMap(x1:x2:_)) = if check == seccheck
            then traverseList x1 x2
            else Left ("The input from server is not labeled as expected (" ++ check ++ ")")
        validate _ _ = Left "Error occured while parsing input from the server"
        hintparse :: (String, Document) -> Either String Int
        hintparse (check, DInteger num) = if check == "number_of_hints"
            then Right num else Left "The input from server is not labeled as expected (number_of_hints)"
        hintparse _ = Left "Error occured while parsing input from the server"

        traverseList :: (String,Document) -> (String,Document) -> Either String [Int]
        traverseList (_, DInteger num) (_, DMap(x1:x2:_)) = do
            let list = traverseList x1 x2
            case list of
                Right a -> return (num : a)
                Left b -> Left b
        traverseList (_, DInteger num) (_, DNull) = Right [num]
        traverseList _ _ = Left "Error occured while parsing input from the server"
        
-- IMPLEMENT
-- Adds hint data to the game state
-- Errors are reported via Either but not error 
hint :: State -> Document -> Either String State
--hint (State l) h = Right $ State $ ("Hint " ++ show h) : l
-- hint s d = Right s
hint (State g oR oC h _) (DMap ((check, DList xs):_)) = do
    hC <- traverseList xs
    return (State g oR oC h hC)
    where
        traverseList :: [Document] -> Either String [Coord]
        traverseList ((DMap ((_, DInteger num1):(_, DInteger num2):_)):xs) = do
            let list = traverseList xs
            case list of
                Right a -> return (mergesort((Coord num1 num2) : a))
                Left b -> Left b
        traverseList [] = Right []
        traverseList _ = Left "Error occured while parsing input from the server"
hint _ _ = Left "Error occured while parsing input from the server"


-- hint (State g oR oC h _) (DMap ((_,DList x):_)) = 
--     State{
--     guess = g,
--     ocRows = oR,
--     ocCols = oC,
--     hints = h,
--     hintCoords = mergesort (traverseList x)
-- } where
--     traverseList :: [Document] -> [Coord]
--     traverseList ((DMap (x1:x2:_)):xs) = Coord (getInt x1) (getInt x2) : traverseList xs
--     traverseList _ = []
--     getInt :: (String, Document) -> Int
--     getInt (_,DInteger x) = x
--     getInt _ = -1
-- hint s _ = s









-- data Either = Left a I Right a





