{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Lib2(renderDocument, hint, gameStart) where

import Lib1 (State(..))
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
 gameStart s _ = Right s
-- gameStart (State sg soR soC sh shC) (DMap (x1:x2:x3:_)) = do
--     oR <- traverseList "occupied_rows" x3
--     oC <- traverseList "occupied_cols" x2
--     h <- parseDI x1
--     return (State sg oR oC h shC)
--     where
--         parseDI :: (String, Document) -> Either String Int
--         parseDI (check, DInteger a) = if check == "number_of_hints"
--             then Right a else Left "Wrong input"
--         parseDI _ = Left "Wrong input"

--         traverseList :: String -> (String,Document) -> Either String [Int]
--         traverseList firstcheck (check, DMap (x1:x2:_)) = do

            
            
--             if firstcheck == check
--             then traverseList "head" x1 ++ traverseList "tail" x2
--             else Left ("Expected " ++ firstcheck)
--         traverseList firstcheck (check , DInteger i) = if firstcheck == check 
--             then Right [i] else Left ("Expected " ++ firstcheck)
--         traverseList _ (check, DNull) = if check == "tail" then Right [] else Left ("Expected tail")
--         traverseList _ _ = Left ("Wrong input.")
-- gameStart _ _ = emptyState


-- Right [a,b,c] -> [a,b,c]

-- fun :: Document -> Int
-- fun (DInteger a) = a



-- gameStart s d = Right s
-- IMPLEMENT
-- Adds hint data to the game state
-- Errors are reported via Either but not error 
hint :: State -> Document -> Either String State
--hint (State l) h = Right $ State $ ("Hint " ++ show h) : l
hint s d = Right s










-- data Either = Left a I Right a





