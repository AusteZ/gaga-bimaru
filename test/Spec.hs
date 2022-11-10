import Test.Tasty
import Test.Tasty.HUnit

import Lib1 (State(..),emptyState)
import Lib2 (renderDocument, gameStart, hint)
import Types (Document(..), Coord(..))

main :: IO ()
main = defaultMain (testGroup "Tests" [
  toYamlTests,
  gameStartTests,
  hintTests])

toYamlTests :: TestTree
toYamlTests = testGroup "Document to yaml"
  [
      testCase "null" $
        renderDocument DNull @?= "---\n~\n"
    , testCase "int" $
        renderDocument (DInteger 5) @?= "---\n5\n"
    , testCase "DMap" $
        renderDocument (DMap [("aaaaaa", DInteger 5),("bbbbbb", DInteger 6),("cccccc", DInteger 20)]) @?= "---\naaaaaa: 5\nbbbbbb: 6\ncccccc: 20\n"
    , testCase "DString" $
        renderDocument (DString "hello this is a test") @?= "---\n\"hello this is a test\"\n"
    , testCase "list of strings" $
        renderDocument (DList [DString "5", DString "6"]) @?= "---\n- \"5\"\n- \"6\"\n"
    , testCase "list of ints" $
        renderDocument (DList [DInteger 5, DInteger 6, DInteger 4]) @?= "---\n- 5\n- 6\n- 4\n"
    , testCase "list of nulls" $
        renderDocument (DList [DNull, DNull]) @?= "---\n- ~\n- ~\n"
    , testCase "list of int in map" $
        renderDocument (DMap[("name", (DList [(DInteger 5), (DInteger 6)]))]) @?= "---\nname:\n- 5\n- 6\n"
    , testCase "map in map in map" $
        renderDocument (DMap[("hi", DMap[("hello", DMap[("end", DNull)])])]) @?= "---\nhi:\n- hello:\n  - end: ~\n"
    , testCase "dmap in dlist in dmap" $
        renderDocument (DMap [("title", (DList [(DMap [("end", DNull)])]))]) @?= "---\ntitle:\n- end: ~\n"
    , testCase "coord list" $
        renderDocument (DMap [("coords", (DList [DMap [("col",DInteger 0),("row",DInteger 8)],DMap[("col",DInteger 5),("row",DInteger 8)]]))]) @?= "---\ncoords:\n- col: 0\n  row: 8\n- col: 5\n  row: 8\n"
    , testCase "tricky" $
        renderDocument trickyCaseDocument @?= trickyCaseString
  ]

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
    "---",
    "key1:",
    "- key2:",
    "  - 1",
    "  - key3:",
    "    - 1",
    "    - 3",
    "    - ~",
    "    - :",
    "      - 3",
    "      - ~",     
    "    - []",
    "    key4:",
    "  - ~",
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
dmapgenerate prefix nr = DMap[("number_of_hints", DInteger 10),((prefix ++ "cols"), generate nr),((prefix ++ "rows"), generate nr)]
  where
    generate :: Int -> Document
    generate 0 = DNull
    generate nr1 = DMap[("head", DInteger 5), ("tail", generate (nr1-1))]

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