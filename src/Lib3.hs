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
            Just c -> (DInteger c)
            Nothing -> (if strippedValue == "null" || strippedValue == "~" then DNull else DString (stringparser strippedValue))
    where
        listDList :: Int -> [String] -> [Document]
        listDList nrr (str1:strss) = if nr == wsAmount str1 && head (drop nrr str1) == '-' 
            then (yamlToDocument (nrr+2) ((replaceDash str1):strss)) : listDList nrr strss else if nrr >= wsAmount str1
            then [] else listDList nrr strss
        listDList _ [] = []
        listDMap :: Int -> [String] -> [(String, Document)]
        listDMap nrr al@(str1:strss) = if nrr == wsAmount str1 && isInfixOf ":" str1  && isInfixOf "- " str1 == False
            then (emptyKey (drop nrr (fst (splitAtC str1))), dmapInner nrr al):(listDMap nrr strss)
            else if nrr > wsAmount str1 then [] else listDMap nrr strss
        listDMap _ [] = []

        emptyKey :: String -> String 
        emptyKey str1 = if length str1 >= 2 && head str1 == '\'' && last str1 == '\'' then init (tail str1) else str1
        dmapInner :: Int -> [String] -> Document
        dmapInner nrr (str1:strss) = if snd (splitAtC str1) /= "" then yamlToDocument (wsAmount str1) ((snd (splitAtC str1)):[]) else yamlToDocument (checkForList nrr strss) strss
        dmapInner _ _ = DNull
        checkForList :: Int -> [String] -> Int
        checkForList nr1 (next:_) = if wsAmount next == nr1 && isInfixOf "- " next then nr1 else nr1+1
        checkForList nr1 _ = nr1+1
        checkingNextLine :: [String] -> Bool
        checkingNextLine (str1:next:_) = str1 == "" && (wsAmount next > nr || wsAmount next == nr && isInfixOf "- " next)
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
        checkForEmptyLines (l, (str:next:strss)) = if l || str == "" then if next /= "" then (True, strss) else (False, next:strss) else (False, (str:next:strss))
        checkForEmptyLines (_,l) = (False, l)

        checkLines :: [String] -> Either String [String]
        checkLines strss = if checkFirstLine strss then Right strss else if isLeft (checkSymbols 1 strss) then checkSymbols 1 strss
            else if fst (checkIndent (True, False, False, False) 1 0 strss) then Left (snd (checkIndent (True, False, False, False) 1 0 strss))
            else Right strss

        checkSymbols :: Int -> [String] -> Either String [String]
        checkSymbols _ ("":[]) = Right [""]
        checkSymbols _ [] = Right [] 
        checkSymbols nr (str:strss)
            | isInfixOf "- " str && head (drop (wsAmount str) str) == '-' = if getThroughDashes (drop (wsAmount str) str) == "" then Left ("List has no element, and is not marked as empty")
                else if fst (errorOneLine nr ((getThroughDashes (drop (wsAmount str) str)))) then Left (snd (errorOneLine nr ((getThroughDashes (drop (wsAmount str) str))))) else (do
                    let cS = checkSymbols (nr+1) (strss)
                    case cS of
                        Right a -> return (str:a)
                        Left b -> Left b)
            | isInfixOf ":" str = if getThroughDashes (drop (wsAmount str) (fst (splitAtC str))) == "" then Left ("Key cannot be totally empty and not marked ''")
                else if snd (splitAtC str) == "" && last str == ' ' then Left ("Please either mark empty string as '' or delete the space after ':'") 
                else do
                    let cS = checkSymbols (nr+1) (strss)
                    case cS of
                        Right a -> return (str : a)
                        Left b -> Left b
            | otherwise = Left ("It is not possible to parse this into Document ADT")

        
        checkIndent :: (Bool,Bool,Bool,Bool) -> Int -> Int -> [String] -> (Bool, String)
        checkIndent _ _ _ [] = (False, "")
        checkIndent _ _ _ [""] = (False, "")
        checkIndent (f, l, m, s) nr idd (str:strss)
            | (f || l) && isInfixOf "- " str && (2 + wsAmount str == idd || (f && idd == 0)) = if fst (checkIndent (True, False, False, False) nr (idd+(if idd == 0 then 4 else 2)) ((replaceDash str):strss))
                then (checkIndent (True, False, False, False) nr (idd+(if idd == 0 then 4 else 2)) ((replaceDash str):strss))
                else (checkIndent (False, True, False, False) nr (idd+(if idd == 0 then 2 else 0)) (strss))
            | (f || m) && isInfixOf ":" str && wsAmount str == idd = if last str /= ':' && fst (checkIndent (False, False, True, True) (nr+1) idd strss) 
                then (checkIndent (False, False, True, True) (nr+1) idd strss)
                else if fst (checkIndent (True, False, False, False) (nr+1) (idd+2) strss) then (True, (snd (checkIndent (True, False, False, False) (nr+1) (idd+2) strss)) ++ "False" ++ str)
                else (checkIndent (False, True, False, False) (nr+1) idd strss)
            | f && wsAmount str > idd = (True, "A non-list and non-dictionary value must be in the same line as the key")
            | m && s && wsAmount str > idd = (True, "The key already has value")
            | wsAmount str == idd = checkIndent (False, False, False, True) (nr+1) idd strss
            | wsAmount str > idd = checkIndent (f, l, m, s) (nr+1) idd strss
            | wsAmount str < idd = (False, "")
            | otherwise = (True, "Some other error occured") 

        checkFirstLine :: [String] -> Bool
        checkFirstLine (_:[]) = True
        checkFirstLine (_:"":[]) = True
        checkFirstLine _ = False
        getThroughDashes :: String -> String
        getThroughDashes (ch1:ch2:str) = if ch1:ch2:[] == "- " then getThroughDashes str else ch1:ch2:str
        getThroughDashes str = str
        errorOneLine :: Int -> String -> (Bool,String)
        errorOneLine nr str = if isInfixOf ":" str == False then (False,str) else do
            let ess = (checkSymbols nr (str:"":[]))
            case ess of
                Right _ -> (False, "")
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
            validate check (DMap ((xkey,xdoc):al)) = if check == xkey then if check == "number_of_hints" then parseHint xdoc
                else traverselist xkey 2 xdoc else validate check (DMap al)
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
            traverselist _ _ DNull = Right []
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
    fromDocument d = do
        hC <- validate d
        return (Hint hC)
        where
            validate :: Document -> Either String [(Int, Int)]
            validate (DMap ((key, (DList (al))):[])) = if key == "coords" then getHints al
            else Left "Expected key named coords on 1st level in hint"
            validate (DMap lst) = if length lst /= 1 then Left "Expected only one (String,Document) in the outermost (level 0) DMap"
            else Left ("Expected (String, DList) on level 1, received (String," ++ errorDocType (snd (head lst)) ++ ")")
            validate x = Left ("Expected DMap as outermost Document (level 0), received " ++ errorDocType x)
            getHints :: [Document] -> Either String [(Int, Int)]
            getHints [] = Right []
            getHints ((DMap ((key1, DInteger nr1):(key2, DInteger nr2):[])):al) =
                if key1 == "col" && key2 == "row" then do
                let list = getHints al
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
        tupleToCoord ((xc,xr):al) = (Coord xc xr):(tupleToCoord al)
        tupleToCoord [] = []