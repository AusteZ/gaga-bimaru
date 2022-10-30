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
  [   testCase "null" $
        renderDocument DNull @?= "A non DList was passed to renderDocument."
    , testCase "int" $
        renderDocument (DInteger 5) @?= "A non DList was passed to renderDocument."
    , testCase "DMap" $
        renderDocument (DMap [("aaaaaa", DInteger 5), ("aaaaaa", DInteger 6)]) @?= "A non DList was passed to renderDocument."
    , testCase "DString" $
        renderDocument (DString "{coords: [{col: 5, row: 6}]}") @?= "A non DList was passed to renderDocument."
    , testCase "list of strings" $
        renderDocument (DList [DString "5", DString "6"]) @?= "An even amount of DIntegers in DList were expected."
    , testCase "list of odd ints" $
        renderDocument (DList [DInteger 5, DInteger 6, DInteger 4]) @?= "An even amount of DIntegers in DList were expected."
    , testCase "list of ints" $
        renderDocument (DList [DInteger 5, DInteger 6]) @?= listOfInts
    , testCase "list of nulls" $
        renderDocument (DList [DNull, DNull]) @?= "An even amount of DIntegers in DList were expected."
    , testCase "list of list of ints" $
        renderDocument ((DList ([DList([DInteger 5, DInteger 6])]))) @?= "An even amount of DIntegers in DList were expected."
    -- IMPLEMENT more test cases:
    -- * other primitive types/values
    -- * nested types
  ]

listOfInts :: String
listOfInts = "{coords: [{col: 5, row: 6}]}"

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