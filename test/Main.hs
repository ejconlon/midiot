module Main (main) where

import Control.Monad (unless, when)
import Dahdit (Binary (..), ByteCount (..), GetError, StaticByteSized (..), decodeEnd, decodeFileEnd, encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Short as BSS
import Data.Foldable (for_)
import Data.Proxy (Proxy (..))
import Midiot.Arb (Arb (..), arbSBS)
import Midiot.Binary
import Midiot.Midi
import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>))
import Test.Falsify.Generator (Gen)
import qualified Test.Falsify.Predicate as FR
import qualified Test.Falsify.Property as FP
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Falsify (Property, testProperty)
import Test.Tasty.HUnit (testCase, (@?=))

reDecodeEnd :: Binary a => a -> (Either GetError a, ByteCount)
reDecodeEnd = decodeEnd . encode @_ @BSS.ShortByteString

data RTCase where
  RTCase :: (Eq a, Show a, Binary a) => String -> Gen a -> Maybe ByteCount -> RTCase

dynRTCase :: (Eq a, Show a, Binary a) => String -> Gen a -> RTCase
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
  let (endRes, endConBc) = decodeEnd encVal
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
  , staRTCase "ShortManf" (arb @ShortManf)
  , staRTCase "LongManf" (arb @LongManf)
  , dynRTCase "Manf" (arb @Manf)
  , staRTCase "QuarterTime" (arb @QuarterTime)
  , dynRTCase "UnivSysEx" (arb @UnivSysEx)
  , dynRTCase "ManfSysEx" (arb @ManfSysEx)
  , dynRTCase "SysExData" (arb @SysExData)
  , staRTCase "LiveStatus" (arb @LiveStatus)
  , staRTCase "RecStatus" (arb @RecStatus)
  , staRTCase "ShortStatus" (arb @ShortStatus)
  , dynRTCase "MetaString" (fmap MetaString (arbSBS 0 255))
  , dynRTCase "MetaData" (arb @MetaData)
  , dynRTCase "LiveMsg" (arb @LiveMsg)
  , dynRTCase "RecMsg" (arb @RecMsg)
  , dynRTCase "ShortMsg" (arb @ShortMsg)
  , dynRTCase "Track" (arb @Track)
  , dynRTCase "MidFile" (arb @MidFile)
  , dynRTCase "SysExDump" (arb @SysExDump)
  ]

testRTCases :: TestTree
testRTCases = testGroup "RT" (fmap runRTCase rtCases)

findFiles :: IO [FilePath]
findFiles = do
  let tdDir = "testdata"
      midiDir = tdDir </> "midi"
  let xtraFiles = fmap (tdDir </>) ["twinkle.mid", "parse_me.mid"]
  midiFiles <- fmap (fmap (midiDir </>)) (listDirectory midiDir)
  pure (xtraFiles ++ midiFiles)

decodeFileAs :: Binary a => Proxy a -> FilePath -> IO (Either GetError a, ByteCount)
decodeFileAs _ = decodeFileEnd

shouldFail :: FilePath -> Bool
shouldFail fn =
  let xs = fmap (\p -> BS.pack ("/test-" ++ p ++ "-")) ["illegal", "non-midi", "corrupt"]
  in  any (`BS.isInfixOf` BS.pack fn) xs

runFileCase :: (Binary a, Eq a, Show a) => Proxy a -> FilePath -> IO ()
runFileCase prox fn = do
  (fileRes, fileBc) <- decodeFileAs prox fn
  case fileRes of
    Left err ->
      unless
        (shouldFail fn)
        (fail ("Decode " ++ fn ++ " failed at " ++ show (unByteCount fileBc) ++ ": " ++ show err))
    Right fileVal -> do
      when (shouldFail fn) (fail "Expected failure")
      -- Rendered size should not be larger than input size
      let dynBc = byteSize fileVal
      unless (dynBc <= fileBc) (fail "Bad byte size")
      let (endRes, endConBc) = reDecodeEnd fileVal
      case endRes of
        Left err -> fail ("Re-decode " ++ fn ++ " failed: " ++ show err)
        Right endVal -> do
          endVal @?= fileVal
          endConBc @?= dynBc

testFileCase :: FilePath -> TestTree
testFileCase fn = testCase fn $ do
  let ext = takeExtension fn
  case ext of
    ".mid" -> runFileCase (Proxy @MidFile) fn
    ".syx" -> runFileCase (Proxy @SysExDump) fn
    _ -> fail ("Unhandled file format: " ++ ext)

testFileCases :: [FilePath] -> TestTree
testFileCases = testGroup "Files" . fmap testFileCase

-- Increase number of examples with TASTY_FALSIFY_TESTS=1000 etc
main :: IO ()
main = do
  files <- findFiles
  defaultMain $
    testGroup
      "Midiot"
      [ testRTCases
      , testFileCases files
      ]
