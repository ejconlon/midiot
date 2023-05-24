module Main (main) where

import Dahdit (Binary (..), ByteCount (..), GetError, StaticByteSized (..), decodeFileEnd)
import qualified Data.ByteString.Char8 as BS
import Data.Proxy (Proxy (..))
import Midiot.Arb (arbI, genSBS)
import Midiot.Binary
  ( MidiInt14
  , MidiInt7
  , MidiWord14
  , MidiWord7
  , VarWord
  )
import qualified Midiot.Midi as MM
-- import qualified Midiot.Osc as MO
import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>))
import Test.Dahdit.Tasty (FileExpect (..), RT, fileRT, genRT, staticRT, testRT)
import Test.Falsify.Generator (Gen)
import Test.Tasty (TestTree, defaultMain, testGroup)

proxyFor :: f a -> Proxy a
proxyFor _ = Proxy

staRT :: (Eq a, Show a, StaticByteSized a, Binary a) => String -> Gen a -> RT
staRT name gen = let p = proxyFor gen in staticRT p (genRT name gen)

genCases :: [RT]
genCases =
  [ staRT "MidiWord7" (arbI @MidiWord7)
  , staRT "MidiInt7" (arbI @MidiInt7)
  , staRT "MidiWord14" (arbI @MidiWord14)
  , staRT "MidiInt14" (arbI @MidiInt14)
  , genRT "VarWord" (arbI @VarWord)
  , staRT "Channel" (arbI @MM.Channel)
  , staRT "Note" (arbI @MM.Note)
  , staRT "Velocity" (arbI @MM.Velocity)
  , staRT "ControlNum" (arbI @MM.ControlNum)
  , staRT "ControlVal" (arbI @MM.ControlVal)
  , staRT "Pressure" (arbI @MM.Pressure)
  , staRT "ProgramNum" (arbI @MM.ProgramNum)
  , staRT "PitchBend" (arbI @MM.PitchBend)
  , staRT "Song" (arbI @MM.Song)
  , staRT "Position" (arbI @MM.Position)
  , staRT "ShortManf" (arbI @MM.ShortManf)
  , staRT "LongManf" (arbI @MM.LongManf)
  , genRT "Manf" (arbI @MM.Manf)
  , staRT "QuarterTime" (arbI @MM.QuarterTime)
  , genRT "UnivSysEx" (arbI @MM.UnivSysEx)
  , genRT "ManfSysEx" (arbI @MM.ManfSysEx)
  , genRT "SysExData" (arbI @MM.SysExData)
  , staRT "LiveStatus" (arbI @MM.LiveStatus)
  , staRT "RecStatus" (arbI @MM.RecStatus)
  , staRT "ShortStatus" (arbI @MM.ShortStatus)
  , genRT "MetaString" (fmap MM.MetaString (genSBS 0 255))
  , genRT "MetaData" (arbI @MM.MetaData)
  , genRT "LiveMsg" (arbI @MM.LiveMsg)
  , genRT "RecMsg" (arbI @MM.RecMsg)
  , genRT "ShortMsg" (arbI @MM.ShortMsg)
  , genRT "Track" (arbI @MM.Track)
  , genRT "MidFile" (arbI @MM.MidFile)
  , genRT "SysExDump" (arbI @MM.SysExDump)
  ]

testGenCases :: TestTree
testGenCases = testGroup "Gen" (fmap testRT genCases)

findFiles :: IO [FilePath]
findFiles = do
  let tdDir = "testdata"
      midiDir = tdDir </> "midi"
  let xtraFiles = fmap (tdDir </>) ["twinkle.mid", "parse_me.mid"]
  midiFiles <- fmap (fmap (midiDir </>)) (listDirectory midiDir)
  pure (xtraFiles ++ midiFiles)

decodeFileAs :: Binary a => Proxy a -> FilePath -> IO (Either GetError a, ByteCount)
decodeFileAs _ = decodeFileEnd

shouldFail :: FilePath -> FileExpect a
shouldFail fn =
  let xs = fmap (\p -> BS.pack ("/test-" ++ p ++ "-")) ["illegal", "non-midi", "corrupt"]
      b = any (`BS.isInfixOf` BS.pack fn) xs
  in  if b then FileExpectFail else FileExpectOk

mkFileRT :: FilePath -> IO RT
mkFileRT fn = do
  let ext = takeExtension fn
  case ext of
    ".mid" -> pure (fileRT @MM.MidFile fn fn (shouldFail fn))
    ".syx" -> pure (fileRT @MM.SysExDump fn fn (shouldFail fn))
    _ -> fail ("Unhandled file format: " ++ ext)

-- Increase number of examples with TASTY_FALSIFY_TESTS=1000 etc
main :: IO ()
main = do
  files <- findFiles
  fileCases <- traverse mkFileRT files
  let testFileCases = testGroup "File" (fmap testRT fileCases)
  defaultMain $
    testGroup
      "Midiot"
      [ testGenCases
      , testFileCases
      ]
