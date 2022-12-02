{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib3(hint, gameStart, parseDocument, GameStart, Hint) where
    
import Types
-- import Types ( Document, FromDocument, fromDocument )
import Lib1 (State(..))
-- import Data.Text as T
import Data.List
import Text.Read

errorDocType :: Document -> String
errorDocType (DInteger _) = "DInteger"
errorDocType (DString _) = "DString"
errorDocType (DList _) = "DList"
errorDocType (DMap _) = "DMap"
errorDocType DNull = "DNull"
errorDocType doc = show doc

-- IMPLEMENT
-- Parses a document from yaml
parseDocument :: String -> Either String Document
parseDocument str = Right (yamlToDocument 0 (lines str))

yamlToDocument :: Int -> [String] -> Document
yamlToDocument _ [] = DNull
yamlToDocument nr a@(str:strs)
    | isInfixOf "- " str = DList(listDList (fst (stripWSStart (0,str))) a)
    | isInfixOf ":" str = DMap(listDMap (fst (stripWSStart (0,str))) a)
    | checkingNextLine strs = yamlToDocument nr strs
    | str == "[]" || (wsAmount str == nr && drop (wsAmount str) str == "[]") = (DList [])
    | str == "{}" || (wsAmount str == nr && drop (wsAmount str) str == "{}") = (DMap [])
    | otherwise = do
        let strippedValue = if wsAmount str == nr then drop (wsAmount str) str else str
        let value = readMaybe strippedValue :: Maybe Int
        case value of
            Just a -> (DInteger a)
            Nothing -> (if strippedValue == "null" || strippedValue == "~" then DNull else DString (stringparser strippedValue))
    where
        listDList :: Int -> [String] -> [Document]
        listDList nr (str:strs) = if nr == wsAmount str && head (drop nr str) == '-' 
            then (yamlToDocument (nr+2) ((replaceDash str):strs)) : listDList nr strs else if nr >= wsAmount str
            then [] else listDList nr strs
        listDList _ [] = []
        listDMap :: Int -> [String] -> [(String, Document)]
        listDMap nr a@(str:strs) = if nr == wsAmount str && isInfixOf ":" str  && isInfixOf "- " str == False
            then (emptyKey (drop nr (fst (splitAtC str))), dmapInner nr a):(listDMap nr strs)
            else if nr > wsAmount str then [] else listDMap nr strs
        listDMap _ [] = []

        replaceDash :: String -> String
        replaceDash (ch1:ch2:str) = if (ch1:ch2:[]) == "- " then "  " ++ str else if ch1 == ' ' then ch1 : replaceDash (ch2:str) else ch1:ch2:str
        wsAmount :: String -> Int
        wsAmount (ch:str) = if ch == ' ' then 1 + wsAmount str else 0
        wsAmount [] = 0
        emptyKey :: String -> String
        emptyKey str = if length str >= 2 && head str == '\'' && last str == '\'' then init (tail str) else str
        dmapInner :: Int -> [String] -> Document
        dmapInner nr (str:strs) = if snd (splitAtC str) /= "" then yamlToDocument (wsAmount str) ((snd (splitAtC str)):[]) else yamlToDocument (checkForList nr strs) strs
        checkForList :: Int -> [String] -> Int
        checkForList nr (next:_) = if wsAmount next == nr && isInfixOf "- " next then nr else nr+1
        checkForList nr _ = nr+1
        checkingNextLine :: [String] -> Bool
        checkingNextLine (str:next:_) = str == "" && (wsAmount next > nr || wsAmount next == nr && isInfixOf "- " next)
        checkingNextLine _ = False
        stringparser :: String -> String
        stringparser str = if length str >= 2 && (head str == '\'' && last str == '\'' || head str == '"' && last str == '"') then init(tail str) else str
        stripWSStart :: (Int,String) -> (Int, String)
        stripWSStart (nr, (char:str)) = if char == ' ' then stripWSStart ((nr+1), str++"") else (nr,char:str++"")
        stripWSStart (nr, []) = (nr,"")
        stripWSBoth str = stripWSEnd (snd (stripWSStart (0,str)))
        stripWSEnd str = if str /= "" && (last str) == ' ' then stripWSEnd (init str) else str
        splitAtC :: String -> (String, String)
        splitAtC str = if elem ':' str then splitAtC' ("",str) else (str,"")
        splitAtC' :: (String,String) -> (String,String)
        splitAtC' (str1, (ch1:str2)) = if ch1 == ':' then (str1, if str2 == "" then str2 else tail str2) else splitAtC' (str1++[ch1], str2)
        splitAtC' _ = ("","")
        getString :: Document -> String
        getString (DString x) = x
        getString _ = ""
-- IMPLEMENT
-- Change right hand side as you wish
-- You will have to create an instance of FromDocument
data GameStart = GameStart{
    ocRows :: [Int],
    ocCols :: [Int],
    hints :: Int
} deriving Show

instance FromDocument GameStart where
    --fromDocument d = Right (GameStart (show d))
    -- fromDocument :: Document -> Either String a
    fromDocument d = do
        oR <- validate "occupied_rows" d
        oC <- validate "occupied_cols" d
        h <- validate "number_of_hints" d
        return (GameStart oR oC (head h))
        where
            validate :: String -> Document -> Either String [Int]
            validate check (DMap ((xkey,xdoc):all)) = if check == xkey then if check == "number_of_hints" then parseHint xdoc
                else traverselist xkey 2 xdoc else validate check (DMap all)
            validate check (DMap []) = Left ("The list in DMap was empty or the key: \"" ++ check ++ "\" was not found on 1st level")
            validate _ _ = Left "The outermost (0th level) Document must be DMap"

            parseHint :: Document -> Either String [Int]
            parseHint (DInteger nr) = Right [nr]
            parseHint _ = Left "DInteger expected on level 2 in number_of_hints"

            traverselist :: String -> Int -> Document -> Either String [Int]
            traverselist xkey lv (DMap ((key1,(DInteger nr)):(key2,xdoc2):[])) = if key1 == "head" && key2 == "tail" then do
                let list = traverselist xkey (lv+1) xdoc2
                case list of
                    Right a ->  return (nr : a) 
                    Left b -> Left b
                else Left ("Mismatched keys on level" ++ show lv  ++ " in " ++ xkey ++ ". Given keys: " ++ key1 ++ " and " ++ key2)
            traverselist xkey lv DNull = Right []
            traverselist xkey lv (DMap lst) = if (length lst) /= 2 then Left ("Expected exactly 2 (String, Document) members in DList on level " ++ show lv ++ " in " ++ xkey)
                else Left ("DMap must contain (key, DInteger) and either (key, DMap) or (key, DNull) on level " ++ show lv ++ " in " ++ xkey)
            traverselist xkey lv xdoc = Left ("DMap expected on level " ++ show lv ++ " in " ++ xkey ++ ". Received: " ++ errorDocType xdoc)

-- This adds game data to initial state
-- Errors are not reported since GameStart is already totally valid adt
-- containing all fields needed
gameStart :: State -> GameStart -> State
gameStart (State g _ _ _ hC) (GameStart oR oC h) = (State g oR oC h hC)
    
    --State $ ("Game started: " ++ show d) : l

-- IMPLEMENT
-- Change right hand side as you wish
-- You will have to create an instance of FromDocument
data Hint = Hint{
    hintCoords :: [(Int,Int)]
} deriving Show
instance FromDocument Hint where
   -- fromDocument d = Right (Hint (show d))
    -- fromDocument :: Document -> Either String a
    fromDocument d = do
        hC <- validate d
        return (Hint hC)
        where
            validate :: Document -> Either String [(Int, Int)]
            validate (DMap ((key, (DList (all))):[])) = if key == "coords" then getHints all
            else Left "Expected key named coords on 1st level in hint"
            validate (DMap lst) = if length lst /= 1 then Left "Expected only one (String,Document) in the outermost (level 0) DMap"
            else Left ("Expected (String, DList) on level 1, received (String," ++ errorDocType (snd (head lst)) ++ ")")
            validate x = Left ("Expected DMap as outermost Document (level 0), received " ++ errorDocType x)
            getHints :: [Document] -> Either String [(Int, Int)]
            getHints [] = Right []
            getHints ((DMap ((key1, DInteger nr1):(key2, DInteger nr2):[])):all) =
                if key1 == "col" && key2 == "row" then do
                let list = getHints all
                case list of
                    Right a ->  return ((nr1, nr2) : a) 
                    Left b -> Left b
                else Left ("Mismatched labels in hint on level 3: " ++ key1 ++ " and " ++ key2)
            getHints ((DMap lst):_) = if length lst /= 2 then Left "Expected two (String, Document) in DMap on level 3"
                else Left ("Expected (String, DInteger) on level 3, received: (String," ++ errorDocType(snd (head lst)) ++ ") and (String," ++ errorDocType(snd (last lst)) ++ ")")
            getHints x = Left ("Expected DMap on level 3, received: " ++ errorDocType (head x)) 
-- Adds hint data to the game state
-- Errors are not reported since GameStart is already totally valid adt
-- containing all fields needed
hint :: State -> Hint -> State
hint (State g oR oC h _) (Hint hC) = (State g oR oC h (tupleToCoord hC))
    where 
        tupleToCoord :: [(Int,Int)] -> [Coord]
        tupleToCoord ((xc,xr):all) = (Coord xc xr):(tupleToCoord all)
        tupleToCoord [] = []
-- hint (State l) h = State $ ("Hint " ++ show h) : l