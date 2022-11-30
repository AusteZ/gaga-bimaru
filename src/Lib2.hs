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
renderDocument d = formatYaml (False,False) 0 d ++ "\n"

formatYaml :: (Bool,Bool) -> Int -> Document -> String
formatYaml (_,l) nr (DMap []) = (if l then prefix nr l else "") ++ "{}"
formatYaml (_,l) nr (DMap xs) = init(formatDMap True nr xs)
formatYaml (_,l) nr (DList []) = (if l then prefix nr l else "") ++ "[]"
formatYaml (s,l) nr (DList xs) = init(formatDList (s,l) nr xs)
formatYaml (_,l) nr (DInteger x) = (if l then prefix nr l else "") ++ show x
formatYaml (_,l) nr (DString x) = (if l then prefix nr l else "") ++ "\"" ++ x ++ "\""
formatYaml (_,l) nr DNull = (if l then prefix nr l else "") ++ "null"


formatDList :: (Bool,Bool) -> Int -> [Document] -> String
formatDList (s,l) check (x:xs) = formatYaml (True,True) (check+(if s then 0 else 1)) x ++ "\n" ++ formatDList (s,True) check xs
formatDList _ _ [] = ""

formatDMap :: Bool -> Int -> [(String,Document)] -> String
formatDMap _ _ [] = ""
formatDMap l nr ((key,x):xs)
    | key == "" = (formatYaml (True,False) (nr+1) x) ++ "\n" ++ formatDMap False nr xs
    | otherwise = (prefix nr l) ++ key ++ ":" ++ (if checkDocType x then checkEmptyString x
        else "\n") ++ formatYaml (True,False) (nr+1) x ++ "\n" ++ formatDMap False nr xs

prefix :: Int -> Bool -> String
prefix nr l = if nr == 0 then "" else if l then (generateWS (nr-1)) ++ "- " else generateWS nr

generateWS :: Int -> String
generateWS nr = if nr > 0 then take (nr*2) (cycle " ") else ""

checkDocType :: Document -> Bool
checkDocType (DMap []) = True
checkDocType (DList []) = True
checkDocType (DMap _) = False
checkDocType (DList _) = False
checkDocType _ = True

checkEmptyString :: Document -> String
checkEmptyString (DString "") = ""
checkEmptyString _ = " "


{-renderDocument d = (formatYaml "\n" d) ++ "\n"

checkcheck :: String -> String
checkcheck check
    | elem ':' check = "\n" ++ init (init check) ++ "- "
    | otherwise = check ++ "- "

formatYaml :: String -> Document -> String
formatYaml che (DList []) = if elem ':' che then ": []" else che
formatYaml che (DList ds) = formatList (checkcheck che) ds

    where
        formatList :: String -> [Document] -> String
        formatList check (x:xs) = (if elem ':' check then ":" else "") ++ formatYaml (nextCheck check) x ++ formatList (nextCheck check) xs
        formatList _ [] = ""
        nextCheck :: String -> String
        nextCheck check
            | elem ':' check = "\n" ++ (take ((length (check))-5) (cycle " ")) ++ "- "
            | otherwise = init (init check) ++ "- "
formatYaml che (DMap []) = che ++ "[]"
formatYaml che (DMap ds) = formatMap che ds
    where
        formatMap :: String -> [(String, Document)] -> String
        formatMap _ [] = ""
        formatMap check ((str,x):xs) = checkMapCheck check ++ str ++ formatYaml ((if elem ':' check then "" else ":")++(nextCheck check) ++ "  ") x ++ formatMap (nextCheck check) xs
        checkMapCheck :: String -> String
        checkMapCheck check = if elem ':' check then init (init check) ++ "- " else check
        nextCheck :: String -> String
        nextCheck check
            | elem '-' check = "\n" ++ (take ((length (check))-1) (cycle " "))
            | otherwise = check
formatYaml che (DString "") = (if (elem ':' che) then ":" else che) ++ ""
formatYaml che (DString str) = (if (elem ':' che) then ": " else che) ++ "\"" ++ str ++ "\""
formatYaml che (DInteger num) = (if (elem ':' che) then ": " else che) ++ (show num)
formatYaml che DNull = (if (elem ':' che) then ": " else che) ++ "~"-}

-- IMPLEMENT
-- This adds game data to initial state
-- Errors are reported via Either but not error 
gameStart :: State -> Document -> Either String State
--gameStart (State l) d = Right $ State $ ("Game started: " ++ show d) : l
gameStart l _ = Right l

-- IMPLEMENT
-- Adds hint data to the game state
-- Errors are reported via Either but not error 
hint :: State -> Document -> Either String State
--hint (State l) h = Right $ State $ ("Hint " ++ show h) : l
hint l _ = Right l