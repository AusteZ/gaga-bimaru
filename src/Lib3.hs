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
import Data.Either

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
parseDocument str = do
    let val = validateYaml (lines (str))
    case val of
        Right a -> Right (yamlToDocument 0 a)
        Left b -> Left b

yamlToDocument :: Int -> [String] -> Document
yamlToDocument _ [] = DNull
yamlToDocument nr a@(str:strs)
    | isInfixOf "- " str && head (drop (wsAmount str) str) == '-' = DList(listDList (wsAmount str) a)
    | isInfixOf ":" str = DMap(listDMap (wsAmount str) a)
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


replaceDash :: String -> String
replaceDash (ch1:ch2:str) = if (ch1:ch2:[]) == "- " then "  " ++ str else if ch1 == ' ' then ch1 : replaceDash (ch2:str) else ch1:ch2:str
replaceDash _ = ""
wsAmount :: String -> Int
wsAmount (ch:str) = if ch == ' ' then 1 + wsAmount str else 0
wsAmount [] = 0

stringparser :: String -> String
stringparser str = if length str >= 2 && (head str == '\'' && last str == '\'' || head str == '"' && last str == '"') then init(tail str) else str
splitAtC :: String -> (String, String)
splitAtC str = if elem ':' str then splitAtC' ("",str) else (str,"")
splitAtC' :: (String,String) -> (String,String)
splitAtC' (str1, (ch1:str2)) = if ch1 == ':' then (str1, if str2 == "" then str2 else tail str2) else splitAtC' (str1++[ch1], str2)
splitAtC' _ = ("","")

validateYaml :: [String] -> Either String [String]
validateYaml strs = if fst (checkForEmptyLines (False, strs)) then Left "Empty lines were found in the middle of the document"
    else checkLines (snd (checkForEmptyLines (False, strs)))
    where 
        checkForEmptyLines :: (Bool, [String]) -> (Bool, [String])
        checkForEmptyLines (_,("":[])) = (False,[""])
        checkForEmptyLines (_,[]) = (False, [])
        checkForEmptyLines (l, (str:next:strs)) = if l || str == "" then if next /= "" then (True, strs) else (False, next:strs) else (False, (str:next:strs))
        checkForEmptyLines (_,l) = (False, l)

        checkLines :: [String] -> Either String [String]
        checkLines strs = if checkFirstLine strs then Right strs else if isLeft (checkSymbols 1 strs) then checkSymbols 1 strs
            else if fst (checkIndent (True, False, False, False) 1 0 strs) then Left (snd (checkIndent (True, False, False, False) 1 0 strs))
            else Right strs
                
                
                
            --     if fst(checkIndent (True, False, False, False) 1 0 strs) then Left (snd (checkIndent (True, False, False, False) 1 0 strs))
            -- else Right strs

        checkSymbols :: Int -> [String] -> Either String [String]
        checkSymbols _ ("":[]) = Right [""]
        checkSymbols _ [] = Right [] 
        checkSymbols nr (str:strs)
            | isInfixOf "- " str && head (drop (wsAmount str) str) == '-' = if getThroughDashes (drop (wsAmount str) str) == "" then Left ("List has no element, and is not marked as empty")
                else if fst (errorOneLine nr ((getThroughDashes (drop (wsAmount str) str)))) then Left (snd (errorOneLine nr ((getThroughDashes (drop (wsAmount str) str))))) else (do
                    let cS = checkSymbols (nr+1) (strs)
                    case cS of
                        Right a -> return (str:a)
                        Left b -> Left b)
            | isInfixOf ":" str = if getThroughDashes (drop (wsAmount str) (fst (splitAtC str))) == "" then Left ("Key cannot be totally empty and not marked ''")
                else if snd (splitAtC str) == "" && last str == ' ' then Left ("Please either mark empty string as '' or delete the space after ':'") 
                else do
                    let cS = checkSymbols (nr+1) (strs)
                    case cS of
                        Right a -> return (str : a)
                        Left b -> Left b
            | otherwise = Left ("It is not possible to parse this into Document ADT")

        
        checkIndent :: (Bool,Bool,Bool,Bool) -> Int -> Int -> [String] -> (Bool, String)
        checkIndent _ _ _ [] = (False, "")
        checkIndent _ _ _ [""] = (False, "")
        checkIndent (f, l, m, s) nr id (str:strs)
            | (f || l) && isInfixOf "- " str && (2 + wsAmount str == id || (f && id == 0)) = if fst (checkIndent (True, False, False, False) nr (id+(if id == 0 then 4 else 2)) ((replaceDash str):strs))
                then (checkIndent (True, False, False, False) nr (id+(if id == 0 then 4 else 2)) ((replaceDash str):strs))
                else (checkIndent (False, True, False, False) nr (id+(if id == 0 then 2 else 0)) (strs))
            | (f || m) && isInfixOf ":" str && wsAmount str == id = if last str /= ':' && fst (checkIndent (False, False, True, True) (nr+1) id strs) 
                then (checkIndent (False, False, True, True) (nr+1) id strs)
                else if fst (checkIndent (True, False, False, False) (nr+1) (id+2) strs) then (True, (snd (checkIndent (True, False, False, False) (nr+1) (id+2) strs)) ++ "False" ++ str)
                else (checkIndent (False, True, False, False) (nr+1) id strs)
            | f && wsAmount str > id = (True, "A non-list and non-dictionary value must be in the same line as the key")
            | m && s && wsAmount str > id = (True, "The key already has value")
            | wsAmount str == id = checkIndent (False, False, False, True) (nr+1) id strs
            | wsAmount str > id = checkIndent (f, l, m, s) (nr+1) id strs
            | wsAmount str < id = (False, "")
            | otherwise = (True, "Idk") 

        -- checkIndent :: (Bool,Bool,Bool,Bool) -> Int -> Int -> [String] -> Either String [String]    
        -- checkIndent _ _ _ [] = Right []    
        -- checkIndent _ _ _ ("":[]) = Right [""]
        -- checkIndent (f, l, m, i) nr id (str:strs)
        --     | (f || l) && isInfixOf "- " str && (2 + wsAmount str == id || (l && id == 0)) = if isLeft  
        --         then (checkIndent (True, False, False, False) nr (id+(if id == 0 then 4 else 2)) ((replaceDash str):strs))
        --         else do
        --             let lll = (checkIndent (False, True, False, False) (nr+1) (id+(if id == 0 then 2 else 0)) strs)
        --             case lll of
        --                 Right a ->  return (str : a)
        --                 Left b -> Left b
        --     | (f || m) && isInfixOf ":" str && wsAmount str == id = if last str /= ':' && isLeft (checkIndent (True, False, False, True) (nr+1) (id+2) strs) then (checkIndent (True, False, False, True) (nr+1) (id+2) strs)
        --         else do
        --             let lll = (checkIndent (False, False, True, True) (nr+1) id strs)
        --             case lll of
        --                 Right a -> return (if (f && i) || m then str:a else a)
        --                 Left b -> Left b
        --     | (id > wsAmount str) || (wsAmount str == id && isInfixOf "- " str) = if f && str == "" then Left ("Error on line " ++ show nr ++ ". Empty structures have to be marked ('', {}, [])") else Right []
        --     | wsAmount str == id = if checkN id strs then Right [] else Left ("Line "++ show (nr+1) ++ " is too many indents from the one above it.")
        --     | otherwise = Right []
        --     where
        --         checkN :: Int -> [String] -> Bool
        --         checkN id (str:strs) = if wsAmount str > id then False else True
        --         checkN _ _ = True

        checkFirstLine :: [String] -> Bool
        checkFirstLine (str:[]) = True
        checkFirstLine (str:"":[]) = True
        checkFirstLine a = False
        getThroughDashes :: String -> String
        getThroughDashes (ch1:ch2:str) = if ch1:ch2:[] == "- " then getThroughDashes str else ch1:ch2:str
        getThroughDashes str = str
        errorOneLine :: Int -> String -> (Bool,String)
        errorOneLine nr str = if isInfixOf ":" str == False then (False,str) else do
            let ess = (checkSymbols nr (str:"":[]))
            case ess of
                Right a -> (False, "")
                Left b -> (True, b)
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