import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.String.Conversions
import Data.Yaml as Y ( encodeWith, defaultEncodeOptions, defaultFormatOptions, setWidth, setFormat )
import qualified Data.List as L

import Lib1 (State(..),emptyState)
import Lib2 (renderDocument, gameStart, hint)
import Lib3 (parseDocument)
import Types (Document(..), Coord(..))

main :: IO ()
main = defaultMain (testGroup "Tests" [
  toYamlTests,
  fromYamlTests,
  gameStartTests,
  hintTests,
  properties
  ])

properties :: TestTree
properties = testGroup "Properties" [golden, dogfood]

friendlyEncode :: Document -> String
friendlyEncode doc = cs (Y.encodeWith (setFormat (setWidth Nothing defaultFormatOptions) defaultEncodeOptions) doc)

golden :: TestTree
golden = testGroup "Handles foreign rendering"
  [
    testProperty "parseDocument (Data.Yaml.encode doc) == doc" $
      \doc -> parseDocument (friendlyEncode doc) == Right doc
  ]

dogfood :: TestTree
dogfood = testGroup "Eating your own dogfood" 
  [  
    testProperty "parseDocument (renderDocument doc) == doc" $
      \doc -> parseDocument (renderDocument doc) == Right doc
  ]

fromYamlTests :: TestTree
fromYamlTests = testGroup "Document from yaml"
  [   testCase "null" $
        parseDocument "null" @?= Right DNull
    , testCase "String" $
        parseDocument "abc" @?= Right (DString "abc") 
    , testCase "StringWithSpace" $
        parseDocument "ab c" @?= Right (DString "ab c")
    , testCase "StringWithDoubleQuotation" $
        parseDocument "\"ab c\"" @?= Right (DString "ab c")
    , testCase "StringWithSingleQuotation" $
        parseDocument "\'  \'" @?= Right (DString "  ")
    , testCase "EmptyString" $
        parseDocument "\'\'" @?= Right (DString "")
    , testCase "Integer" $
        parseDocument "5" @?= Right (DInteger 5) 
    , testCase "NegativeInteger" $
        parseDocument "-69" @?= Right (DInteger (-69))
    , testCase "SingleList" $
        parseDocument "- 5" @?= Right (DList [DInteger 5]) 
    , testCase "List" $
        parseDocument "- 5\n- null\n- ula" @?= Right (DList [DInteger 5, DNull, DString "ula"]) 
    , testCase "NestedList" $
        parseDocument "- 5\n- - null\n  - 45\n- ula" @?= Right (DList [DInteger 5, DList[DNull, DInteger 45], DString "ula"])
    , testCase "EmptyList" $
        parseDocument "[]" @?= Right (DList [])
    , testCase "EmptyMap" $
        parseDocument "{}" @?= Right (DMap [])
    , testCase "SingleMap" $
        parseDocument "key: 5" @?= Right (DMap [("key", DInteger 5)])
    , testCase "Map" $
        parseDocument "key: 5\nlala: 76\nkuku: null" @?= Right (DMap [("key", DInteger 5),("lala", DInteger 76),("kuku", DNull)])
    , testCase "NestedMap" $
        parseDocument "key:\n  lala: 76\n  kuku: null\nqaw: kzn" @?= Right (DMap [("key", DMap[("lala", DInteger 76),("kuku", DNull)]), ("qaw", DString "kzn")])
    , testCase "ListInDMap" $
        parseDocument "key:\n- 5\n- null\n- ula\nups: nepavyko" @?= Right (DMap [("key", DList [DInteger 5, DNull, DString "ula"]),("ups", DString "nepavyko")])
    , testCase "MapInList" $
        parseDocument "- key: 5\n  lala: 76\n  kuku: null\n- 56\n- null" @?= Right (DList[DMap [("key", DInteger 5),("lala", DInteger 76),("kuku", DNull)],DInteger 56, DNull])
    , testCase "IncorrectYaml" $
        parseDocument "5\n           \n" @?= Left "It is not possible to parse this into Document ADT"
    , testCase "EmptyLinesInYaml" $
        parseDocument "\n \n" @?= Left "Empty lines were found in the middle of the document"
    , testCase "ListWithNoElement" $
        parseDocument "- \n- " @?= Left "List has no element, and is not marked as empty"
    , testCase "EmptyKey" $
        parseDocument ":\n  :" @?= Left "Key cannot be totally empty and not marked ''"
    , testCase "EmptyStringIsNotMarkedInMap" $
        parseDocument "k:\n  k: " @?= Left "Please either mark empty string as '' or delete the space after ':'"
    -- IMPLEMENT more test cases:
    -- * other primitive types/values
    -- * nested types 
  ]


toYamlTests :: TestTree
toYamlTests = testGroup "Document to yaml"
  [   testCase "null" $
        renderDocument DNull @?= "null\n"
    , testCase "int" $
        renderDocument (DInteger 5) @?= "5\n"
    , testCase "DMap" $
        renderDocument (DMap [("aaaaaa", DInteger 5),("bbbbbb", DInteger 6),("cccccc", DInteger 20)]) @?= "aaaaaa: 5\nbbbbbb: 6\ncccccc: 20\n"
    , testCase "DString" $
        renderDocument (DString "hello this is a test") @?= "'hello this is a test'\n"
    , testCase "list of strings" $
        renderDocument (DList [DString "5", DString "6"]) @?= "- '5'\n- '6'\n"
    , testCase "list of ints" $
        renderDocument (DList [DInteger 5, DInteger 6, DInteger 4]) @?= "- 5\n- 6\n- 4\n"
    , testCase "list of nulls" $
        renderDocument (DList [DNull, DNull]) @?= "- null\n- null\n"
    , testCase "list of int in map" $
        renderDocument (DMap[("name", (DList [(DInteger 5), (DInteger 6)]))]) @?= "name:\n- 5\n- 6\n"
    , testCase "map in map in map" $
        renderDocument (DMap[("hi", DMap[("hello", DMap[("end", DNull)])])]) @?= "hi:\n  hello:\n    end: null\n" 
    , testCase "dmap in dlist in dmap" $
        renderDocument (DMap [("title", (DList [(DMap [("end", DNull)])]))]) @?= "title:\n- end: null\n"
    , testCase "coord list" $
        renderDocument (DMap [("coords", (DList [DMap [("col",DInteger 0),("row",DInteger 8)],DMap[("col",DInteger 5),("row",DInteger 8)]]))]) @?= "coords:\n- col: 0\n  row: 8\n- col: 5\n  row: 8\n"
    , testCase "tricky" $
        renderDocument trickyCaseDocument @?= trickyCaseString
    -- IMPLEMENT more test cases:
    -- * other primitive types/values     
    -- * nested types   
    , testCase "dsdds" $
        parseDocument (renderDocument ddd) @?= Right ddd
  ]

ddd :: Document
ddd = DMap [("KoLp",DList [DList [DMap [("AkgS",DInteger 0),("aC",DString "u ")],DMap [("dUs",DList [DInteger 1,DMap [("Ztk",DList [DList [DString "e",DString "x8g"]]),("n",DList [])],DList []])],DMap [("QKVy",DList [DList [DList [DMap [("z",DMap [("DNbU",DString "18b"),("cNUj",DMap [("T",DList []),("fULs",DList [])]),("hki",DString ""),("u",DString " i")])],DInteger 4,DMap [("KAE",DString " 9v"),("TK",DList []),("z",DList [])],DList [DInteger (-2),DList [DString "  N",DMap [("hd",DMap [("B",DInteger 2),("N",DInteger 3),("eNZo",DList []),("reyn",DString "")]),("jJ",DString " P0h")]],DMap [],DMap [("Sd",DMap [("O",DInteger (-2)),("OHZ",DInteger (-1)),("Z",DList [])])]]],DMap [("y",DMap [("Gl",DInteger 2),("Yi",DString ""),("r",DInteger (-1))])],DInteger (-2),DMap [("aQgF",DMap [])]],DString "v"]),("SYDU",DString "FS"),("X",DList [DString "t "])],DInteger 0],DInteger 0,DString ""]),("ZrR",DString " "),("cTN",DMap [("GK",DString ""),("Kdj",DMap [("CAqf",DMap [("Aqx",DMap [("PpNU",DMap [("Bvdl",DList [DInteger 3,DMap [],DString " "])])]),("SZf",DList [DString "5",DMap [("wt",DMap [("f",DInteger 2),("t",DInteger (-3)),("xZ",DMap [])]),("zeN",DList [DInteger (-4)])],DInteger (-4)]),("Y",DString "2 "),("bZg",DString "")])]),("Yij",DInteger (-3))]),("t",DMap [("AJ",DInteger (-2)),("K",DList [DInteger 4]),("c",DMap [("C",DString "213 "),("oqG",DList [DString "",DInteger (-1)]),("vzxz",DString "h")])])]

trickyCaseDocument :: Document
trickyCaseDocument =
 DMap [
    ("key1", DMap [
        ("key2", DList [
            DInteger 1,
            DMap [
                ("key3", DList [
                    DInteger 1,
                    DInteger 3,
                    DNull,
                    DMap [("", DList[
                      DInteger 3,
                      DNull
                    ])],
                    DMap []
                ]),
                ("key4", DString "")],
            DNull
        ])
    ]),
    ("key5", DList [])
 ]
trickyCaseString :: String
trickyCaseString = unlines [
    "key1:",
    "  key2:",
    "  - 1",
    "  - key3:",
    "    - 1",
    "    - 3",
    "    - null",
    "    - '':",
    "      - 3",
    "      - null",     
    "    - {}",
    "    key4: ''",
    "  - null",
    "key5: []"
 ]

gameStartTests :: TestTree
gameStartTests = testGroup "Test start document"
  [   testCase "null" $
        gameStart emptyState DNull @?= Left "Error occured while parsing input from the server in gameStart"
    , testCase "int" $
        gameStart emptyState (DInteger 5) @?= Left "Error occured while parsing input from the server in gameStart"
    , testCase "DString" $
        gameStart emptyState (DString "DMap[(\"Something\", DInteger 5), (\"Something\", DInteger 5)]") @?= Left "Error occured while parsing input from the server in gameStart"
    , testCase "list of strings" $
        gameStart emptyState (DList [DString "5", DString "6"]) @?= Left "Error occured while parsing input from the server in gameStart"
    , testCase "list of ints" $
        gameStart emptyState (DList [DInteger 5, DNull]) @?= Left "Error occured while parsing input from the server in gameStart"
    , testCase "list of nulls" $
        gameStart emptyState (DList [DNull, DNull]) @?= Left "Error occured while parsing input from the server in gameStart"
    , testCase "list of list of ints" $
        gameStart emptyState ((DList ([DList([DInteger 5, DInteger 6])]))) @?= Left "Error occured while parsing input from the server in gameStart"
    , testCase "correct list input" $
        gameStart emptyState (dmapgenerate "occupied_" 10) @?= Right (correctinput emptyState)
    , testCase "incorrect label" $
        gameStart emptyState (dmapgenerate "no_" 10) @?= Left "The input from server is not labeled as expected (occupied_rows)"
    , testCase "too many DMaps" $
        gameStart emptyState (dmapgenerate "occupied_" 12) @?= Left "The number of coordinates passed was too high"
    , testCase "too few DMaps" $
        gameStart emptyState (dmapgenerate "occupied_" 5) @?= Left "The number of coordinates passed was too low"
    , testCase "DMap with DString" $
        gameStart emptyState (unexpectedindmap (DString "nothing")) @?= Left "Unexpected input in DMap (a DInteger and second another DMap or DNull)"
    , testCase "DMap with DInteger" $
        gameStart emptyState (unexpectedindmap (DInteger 5)) @?= Left "Unexpected input in DMap (a DInteger and second another DMap or DNull)"
    , testCase "DMap with DNull" $
        gameStart emptyState (unexpectedindmap DNull) @?= Left "Unexpected input in DMap (a DInteger and second another DMap or DNull)"
    , testCase "DString instrad of DNull as ending in DMap" $
        gameStart emptyState (unexpecteddeeper (dmapgenerate "occupied_" 10))  @?= Left "Unexpected input in DMap (a DInteger and second another DMap or DNull)"
  ]

dmapgenerate :: String -> Int -> Document
dmapgenerate prefix nr = DMap[("number_of_hints", DInteger 10),((prefix ++ "cols"), generateD nr),((prefix ++ "rows"), generateD nr)]
  where
    generateD :: Int -> Document
    generateD 0 = DNull
    generateD nr1 = DMap[("head", DInteger 5), ("tail", generateD (nr1-1))]

correctinput :: State -> State
correctinput (State g _ _ _ hC) = (State g (take 10 (cycle [5])) (take 10 (cycle [5])) 10 hC)

unexpectedindmap :: Document -> Document
unexpectedindmap d = DMap [("something", d),("something", d),("something", d)]

unexpecteddeeper :: Document -> Document
unexpecteddeeper (DMap (x1:(ch,x2):x3:_)) = DMap [x1, (ch, replace x2), x3]
  where
    replace :: Document -> Document
    replace (DMap (x:(chc,DNull):_)) = DMap[x, (chc, (DString "nothing"))]
    replace (DMap (d:(chc, x):_)) = DMap[d, (chc, replace x)]
    replace _ = DNull
unexpecteddeeper _ = DNull


hintTests :: TestTree
hintTests = testGroup "Test hint document"
    [
      testCase "null" $
        hint emptyState DNull @?= Left "Error occured while parsing input from the server in hint"
    , testCase "int" $
        hint emptyState (DInteger 5) @?= Left "Error occured while parsing input from the server in hint"
    , testCase "DMap" $
        hint emptyState (DMap [("aaaaaa", DInteger 5), ("aaaaaa", DInteger 6)]) @?= Left "Error occured while parsing input from the server in hint"
    , testCase "DString" $
        hint emptyState (DString "{coords: [{col: 5, row: 6}]}") @?= Left "Error occured while parsing input from the server in hint"
    , testCase "list of strings" $
        hint emptyState (DList [DString "5", DString "6"]) @?= Left "Error occured while parsing input from the server in hint"
    , testCase "list of ints" $
        hint emptyState (DList [DInteger 5, DInteger 6]) @?= Left "Error occured while parsing input from the server in hint"
    , testCase "list of nulls" $
        hint emptyState (DList [DNull, DNull]) @?= Left "Error occured while parsing input from the server in hint"
    , testCase "list of list of ints" $
        hint emptyState ((DList ([DList([DInteger 5, DInteger 6])]))) @?= Left "Error occured while parsing input from the server in hint"
    , testCase "correct input" $
        hint emptyState (hintinput (DInteger 5)) @?= Right (correcthintoutput emptyState)
    , testCase "nulls instead of ints" $
        hint emptyState (hintinput DNull) @?= Left "Error occured while parsing input from the server in hint"
    , testCase "strings instead of ints" $
        hint emptyState (hintinput (DString "Nothing")) @?= Left "Error occured while parsing input from the server in hint"
  ]

hintinput :: Document -> Document
hintinput d = DMap [("coords", DList ([DMap([("col", d), ("row", d)])]))]

correcthintoutput :: State -> State
correcthintoutput (State g oR oC h _) = (State g oR oC h [(Coord 5 5)])








