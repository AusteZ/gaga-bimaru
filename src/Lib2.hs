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
renderDocument (DList ds) = validate ds where
    validate :: [Document] -> String
    validate ds = do
        let list = parseLeftRight (traverseList ds)
        if list == "An even amount of DIntegers in DList were expected."
        then list else "{coords: [" ++ init (init list) ++ "]}"
    traverseList :: [Document] -> Either String String
    traverseList ((DInteger x):(DInteger y):xs) = do
            let list = traverseList xs
            case list of
                Right a -> return ("{col: " ++ show x ++ ", row: " ++ show y ++ "}, " ++ a)
                Left b -> Left b
    traverseList [] = Right ""
    traverseList _ = Left "no"
    parseLeftRight :: Either String String -> String
    parseLeftRight (Left a) = "An even amount of DIntegers in DList were expected."
    parseLeftRight (Right a) = a
renderDocument _ = "A non DList was passed to renderDocument."
-- IMPLEMENT
-- This adds game data to initial state
-- Errors are reported via Either but not error 
gameStart :: State -> Document -> Either String State
gameStart (State g _ _ _ hC) (DMap (x1:x2:x3:_)) = do
    oR <- validate "occupied_rows" x3
    oC <- validate "occupied_cols" x2
    h <- hintparse x1
    return (State g oR oC h hC)
    where
        validate :: String -> (String,Document) -> Either String [Int]
        validate check (seccheck, DMap(x1:x2:_)) = if check == seccheck
            then traverseList 10 x1 x2
            else Left ("The input from server is not labeled as expected (" ++ check ++ ")")
        validate _ _ = Left "Error occured while parsing input from the server"
        hintparse :: (String, Document) -> Either String Int
        hintparse (check, DInteger num) = if check == "number_of_hints"
            then Right num else Left "The input from server is not labeled as expected (number_of_hints)"
        hintparse _ = Left "Error occured while parsing input from the server"

        traverseList :: Int -> (String,Document) -> (String,Document) -> Either String [Int]
        traverseList nr (_, DInteger num) (_, DMap(x1:x2:_)) = do
            let list = traverseList (nr-1) x1 x2
            case list of
                Right a -> return (num : a)
                Left b -> Left b
        traverseList nr (_, DInteger num) (_, DNull) = if nr == 0 then Right [num]
        else Left ("The number of coordinates passed was too" ++ if nr > 0 then "hight" else "low")
        traverseList _ _ _ = Left "Unexpected input in DMap (a DInteger and second another DMap or DNull)"
gameStart _ _ = Left "Error occured while parsing input from the server"
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





