{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )
import Test.Tasty.QuickCheck as QC

import Data.List
import Data.Ord

import Lib1 qualified
import Lib2 qualified
import Lib3 qualified

import Debug.Trace (trace)


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyTests]

unitTests :: TestTree

unitTests = testGroup "Lib2 tests"
  [ -- Test for parsing empty input
    testCase "Empty Query" $
    Lib2.parseQuery "" @?= Left "Input cannot be empty.",

    -- Test for parsing "Define board as 6x6"
    testCase "Define Board Query" $
    Lib2.parseQuery "Define board as 6x6" @?= Right (Lib2.DefineBoard 6 6, ""),

    -- Test for parsing "Validate for knight"
    testCase "Define Piece Query" $
    Lib2.parseQuery "Validate for knight" @?= Right (Lib2.DefinePiece "knight", ""),

    -- Test for parsing "Move tree (A1(B3)(C2))"
    testCase "Define Move Tree Query" $
    Lib2.parseQuery "Move tree (A1(B3)(C2))" @?= Right (Lib2.DefineMoveTree (Lib2.MoveTree (Lib2.Position 'A' 1) [Lib2.MoveTree (Lib2.Position 'B' 3) [], Lib2.MoveTree (Lib2.Position 'C' 2) []]), ""),

    -- Test for parsing "Show state"
    testCase "Show State Query" $
    Lib2.parseQuery "Show state" @?= Right (Lib2.ShowState, ""),

    -- Test for parsing "Run"
    testCase "Run Query" $
    Lib2.parseQuery "Run" @?= Right (Lib2.Run, "")
  ]

propertyTests :: TestTree
propertyTests = testGroup "Property Tests"
  [ QC.testProperty "render/parse round-trip for Statements" $
  QC.withMaxSuccess 10 prop_renderParseRoundTrip

  ]

genStatements :: Gen Lib3.Statements
genStatements = Lib3.Batch <$> listOf genQuery


genQuery :: Gen Lib2.Query
genQuery = oneof
  [ do
      size <- genSize
      return $ Lib2.DefineBoard size size
  , Lib2.DefinePiece <$> genPiece
  , Lib2.DefineMoveTree <$> genMoveTree
  , pure Lib2.ShowState
  , pure Lib2.Run
  ]


genSize :: Gen Int
genSize = elements [4, 5, 6, 7, 8]



genPiece :: Gen String
genPiece = elements ["knight", "bishop", "rook", "queen"]


genPosition :: Gen Lib2.Position
genPosition = Lib2.Position <$> elements ['A'..'H'] <*> choose (1, 8)

genMoveTree :: Gen Lib2.MoveTree
genMoveTree = do
  pos <- genPosition
  subtrees <- resize 2 (listOf genMoveTree) 
  return $ Lib2.MoveTree pos subtrees


prop_renderParseRoundTrip :: Lib3.Statements -> QC.Property
prop_renderParseRoundTrip stmts =
  let rendered = Lib3.renderStatements stmts
      parsed = Lib3.parseStatements rendered
  in case parsed of
       Right (parsedStatements, _) -> 
         QC.counterexample 
           ("Rendered output:\n" ++ rendered ++ 
            "\nParsed output:\n" ++ show parsedStatements) $
           parsedStatements == stmts
       Left err -> QC.counterexample 
           ("Parsing failed with error: " ++ err ++ 
            "\nRendered output:\n" ++ rendered) False


instance Arbitrary Lib3.Statements where
  arbitrary = genStatements
