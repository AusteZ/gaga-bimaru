{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Lib2(renderDocument, hint, gameStart) where

import Lib1 (State(..), mergesort)
import Types


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
renderDocument d = "---" ++ (formatYaml "\n" d)

formatYaml :: String -> Document -> String
formatYaml che (DList ds) = formatList (checkcheck che) ds
    where
        formatList :: String -> [Document] -> String
        formatList check (x:xs) = (if elem ':' check then ":" else "") ++ formatYaml (nextCheck check) x ++ formatList (nextCheck check) xs
        formatList _ [] = ""
        nextCheck :: String -> String
        nextCheck check
            | elem ':' check = "\n" ++ (take ((length (check))-5) (cycle " ")) ++ "- "
            | otherwise = check
        checkcheck :: String -> String
        checkcheck check
            | elem ':' check = "\n" ++ init (init check) ++ "- "
            | otherwise = check ++ "- "
formatYaml che (DMap ds) = formatMap che ds
    where
        formatMap :: String -> [(String, Document)] -> String
        formatMap _ [] = ""
        formatMap check ((str,x):xs) = check ++ str ++ formatYaml ((if elem ':' check then "" else ":")++(nextCheck check) ++ "  ") x ++ formatMap (nextCheck check) xs
        nextCheck :: String -> String
        nextCheck check
            | elem '-' check = "\n" ++ (take ((length (check))-1) (cycle " "))
            | otherwise = check
formatYaml che (DString str) = (if (elem ':' che) then ": " else che) ++ "\"" ++ str ++ "\""
formatYaml che (DInteger num) = (if (elem ':' che) then ": " else che) ++ (show num)
formatYaml che DNull = (if (elem ':' che) then ": " else che) ++ "~"

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
--hint (State l) h = Right $ State $ ("Hint " ++ show h) : l
-- hint s d = Right s
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





