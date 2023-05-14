module Main (main) where

import Dahdit (Binary, ByteCount (..), ByteSized (..), StaticByteSized (..), decode, encode)
import qualified Data.ByteString.Short as BSS
import Data.Foldable (for_)
import Data.Proxy (Proxy (..))
import Midiot.Arb (Arb (..))
import Midiot.Msg (Channel, Note)
import Test.Falsify.Generator (Gen)
import qualified Test.Falsify.Predicate as FR
import qualified Test.Falsify.Property as FP
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Falsify (Property, testProperty)

data RTCase where
  RTCase :: (Eq a, Show a, ByteSized a, Binary a) => String -> Gen a -> Maybe ByteCount -> RTCase

dynRTCase :: (Eq a, Show a, ByteSized a, Binary a) => String -> Gen a -> RTCase
dynRTCase name gen = RTCase name gen Nothing

proxyFor :: f a -> Proxy a
proxyFor _ = Proxy

staRTCase :: (Eq a, Show a, StaticByteSized a, Binary a) => String -> Gen a -> RTCase
staRTCase name gen = RTCase name gen (Just (staticByteSize (proxyFor gen)))

assertEq :: (Eq a, Show a) => a -> a -> Property ()
assertEq x y = FP.assert (FR.eq FR..$ ("x", x) FR..$ ("y", y))

runRTCase :: RTCase -> TestTree
runRTCase (RTCase name gen mayStaBc) = testProperty name $ do
  startVal <- FP.gen gen
  let startDynBc = byteSize startVal
  for_ mayStaBc (assertEq startDynBc)
  let encVal = encode startVal
      encBc = ByteCount (BSS.length encVal)
  assertEq encBc startDynBc
  let (endRes, endConBc) = decode encVal
  case endRes of
    Left err -> fail ("Decode of " ++ name ++ " failed: " ++ show err)
    Right endVal -> do
      assertEq endVal startVal
      let endDynBc = byteSize endVal
      assertEq endDynBc startDynBc
      assertEq endConBc startDynBc

rtCases :: [RTCase]
rtCases =
  [ staRTCase "Channel" (arb @Channel)
  , staRTCase "Note" (arb @Note)
  ]

testRTCases :: TestTree
testRTCases = testGroup "RT" (fmap runRTCase rtCases)

main :: IO ()
main =
  defaultMain $
    testGroup
      "Midiot"
      [ testRTCases
      ]
