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
--gameStart (State l) d = Right $ State $ ("Game started: " ++ show d) : l
gameStart l _ = Right l

-- IMPLEMENT
-- Adds hint data to the game state
-- Errors are reported via Either but not error 
hint :: State -> Document -> Either String State
--hint (State l) h = Right $ State $ ("Hint " ++ show h) : l
hint l _ = Right l