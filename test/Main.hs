module Main (main) where

import Dahdit (Binary, ByteSized)
import Midiot.Arb (Arb (..))
import Midiot.Msg (Channel)
import Test.Falsify.Generator (Gen)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

testSimple :: TestTree
testSimple = testCase "simple" $ do
  let actual = (1 + 1) :: Int
      expected = 2 :: Int
  actual @?= expected

data RTCase where
  RTCase :: (Eq a, Show a, ByteSized a, Binary a, Arb a) => String -> Gen a -> RTCase

runRTCase :: RTCase -> TestTree
runRTCase (RTCase name _) = testCase name $ do
  pure () -- TODO

rtCases :: [RTCase]
rtCases =
  [ RTCase "Channel" (arb @Channel)
  ]

testRTCases :: TestTree
testRTCases = testGroup "RT" (fmap runRTCase rtCases)

main :: IO ()
main =
  defaultMain $
    testGroup
      "Midiot"
      [ testSimple
      , testRTCases
      ]
