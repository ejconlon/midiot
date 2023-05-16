module Main (main) where

import Dahdit (Binary, ByteCount (..), ByteSized (..), StaticByteSized (..), decode, encode)
import qualified Data.ByteString.Short as BSS
import Data.Foldable (for_)
import Data.Proxy (Proxy (..))
import Midiot.Arb (Arb (..))
-- (Channel, Note)
import Midiot.Binary
import Midiot.Msg
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
  let (endRes, endConBc) = decode encVal
  case endRes of
    Left err -> fail ("Decode of " ++ name ++ " failed: " ++ show err)
    Right endVal -> do
      assertEq endVal startVal
      let endDynBc = byteSize endVal
      assertEq endDynBc startDynBc
      assertEq endConBc startDynBc
      assertEq encBc startDynBc

rtCases :: [RTCase]
rtCases =
  [ staRTCase "MidiWord7" (arb @MidiWord7)
  , staRTCase "MidiInt7" (arb @MidiInt7)
  , staRTCase "MidiWord14" (arb @MidiWord14)
  , staRTCase "MidiInt14" (arb @MidiInt14)
  , dynRTCase "VarWord" (arb @VarWord)
  , staRTCase "Channel" (arb @Channel)
  , staRTCase "Note" (arb @Note)
  , staRTCase "Velocity" (arb @Velocity)
  , staRTCase "ControlNum" (arb @ControlNum)
  , staRTCase "ControlVal" (arb @ControlVal)
  , staRTCase "Pressure" (arb @Pressure)
  , staRTCase "ProgramNum" (arb @ProgramNum)
  , staRTCase "PitchBend" (arb @PitchBend)
  , staRTCase "Song" (arb @Song)
  , staRTCase "Position" (arb @Position)
  , staRTCase "Manf" (arb @Manf)
  , staRTCase "QuarterTime" (arb @QuarterTime)
  , dynRTCase "SysExString" (arb @SysExString)
  , staRTCase "Status" (arb @Status)
  , dynRTCase "Msg" (arb @Msg)
  , dynRTCase "Track" (arb @Track)
  , dynRTCase "File" (arb @File)
  ]

testRTCases :: TestTree
testRTCases = testGroup "RT" (fmap runRTCase rtCases)

-- Increase number of examples with TASTY_FALSIFY_TESTS=1000 etc
main :: IO ()
main =
  defaultMain $
    testGroup
      "Midiot"
      [ testRTCases
      ]
