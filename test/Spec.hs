{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )
import Test.Tasty.QuickCheck as QC

import Data.List
import Data.Ord

import Lib1 qualified
import Lib2 qualified

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
    Lib2.parseQuery "Define board as 6x6" @?= Right (Lib2.DefineBoard 6 6),

    -- Test for parsing "Validate for knight"
    testCase "Define Piece Query" $
    Lib2.parseQuery "Validate for knight" @?= Right (Lib2.DefinePiece "knight"),

    -- Test for parsing "Move tree (A1(B3)(C2))"
    testCase "Define Move Tree Query" $
    Lib2.parseQuery "Move tree (A1(B3)(C2))" @?= Right (Lib2.DefineMoveTree (Lib2.MoveTree (Lib2.Position 'A' 1) [Lib2.MoveTree (Lib2.Position 'B' 3) [], Lib2.MoveTree (Lib2.Position 'C' 2) []])),

    -- Test for parsing "Show state"
    testCase "Show State Query" $
    Lib2.parseQuery "Show state" @?= Right Lib2.ShowState,

    -- Test for parsing "Run"
    testCase "Run Query" $
    Lib2.parseQuery "Run" @?= Right Lib2.Run
  ]

propertyTests :: TestTree
propertyTests = testGroup "some meaningful name"
  [
    QC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)

  ]