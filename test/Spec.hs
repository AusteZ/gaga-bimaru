import Test.Tasty
import Test.Tasty.HUnit

import Lib1 (State(..),emptyState)
import Lib2 (renderDocument, gameStart, hint)
import Types (Document(..))

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
        gameStart emptyState DNull @?= Left "Error occured while parsing input from the server"
    , testCase "int" $
        gameStart emptyState (DInteger 5) @?= Left "Error occured while parsing input from the server"
    , testCase "DString" $
        gameStart emptyState (DString "DMap[(\"Something\", DInteger 5), (\"Something\", DInteger 5)]") @?= Left "Error occured while parsing input from the server"
    , testCase "list of strings" $
        gameStart emptyState (DList [DString "5", DString "6"]) @?= Left "Error occured while parsing input from the server"
    , testCase "list of ints" $
        gameStart emptyState (DList [DInteger 5, DNull]) @?= Left "Error occured while parsing input from the server"
    , testCase "list of nulls" $
        gameStart emptyState (DList [DNull, DNull]) @?= Left "Error occured while parsing input from the server"
    , testCase "list of list of ints" $
        gameStartg emptyState ((DList ([DList([DInteger 5, DInteger 6])]))) @?= Left "Error occured while parsing input from the server"
  ]
    
{-, testCase "DMap incorrect label"
  gameStart emptyState ()-}
  

dmapgenerate :: String -> Int -> Document
dmapgenerate prefix nr = DMap("number_of_hints")

hintTests :: TestTree
hintTests = testGroup "Test hint document" []