import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.String.Conversions
import Data.Yaml as Y ( encodeWith, defaultEncodeOptions, defaultFormatOptions, setWidth, setFormat )
import qualified Data.List as L

import Lib2 (renderDocument, gameStart, hint)
import Lib3 (parseDocument)
import Types (Document(..))

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
    , testCase "(with renderDocument) - golden test that didn't work" $
        parseDocument (renderDocument ddd) @?= Right ddd
    , testCase "(with friendlyEncode) - golden test that didn't work " $
        parseDocument (friendlyEncode ddd) @?= Right ddd
    -- , testCase "dsds" $
    --     parseDocument (friendlyEncode ddd) @?= parseDocument (renderDocument ddd)
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
    , testCase "list of ints" $
        renderDocument (DList [DInteger 5, DInteger 6]) @?= listOfInts
    -- IMPLEMENT more test cases:
    -- * other primitive types/values     
    -- * nested types   
  ]

listOfInts :: String
listOfInts = unlines [
      "- 5"
    , "- 6" 
  ]

gameStartTests :: TestTree
gameStartTests = testGroup "Test start document" []

hintTests :: TestTree
hintTests = testGroup "Test hint document" []




ddd :: Document
ddd = DMap[("d", DNull), ("d", DString "aaa")]








