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
import Midiot.Midi
  ( Channel
  , ControlNum
  , ControlVal
  , LiveMsg
  , LiveStatus
  , LongManf
  , Manf
  , ManfSysEx
  , MetaData
  , MetaString (MetaString)
  , MidFile
  , Note
  , PitchBend
  , Position
  , Pressure
  , ProgramNum
  , QuarterTime
  , RecMsg
  , RecStatus
  , ShortManf
  , ShortMsg
  , ShortStatus
  , Song
  , SysExData
  , SysExDump
  , Track
  , UnivSysEx
  , Velocity
  )
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
  , staRT "Channel" (arbI @Channel)
  , staRT "Note" (arbI @Note)
  , staRT "Velocity" (arbI @Velocity)
  , staRT "ControlNum" (arbI @ControlNum)
  , staRT "ControlVal" (arbI @ControlVal)
  , staRT "Pressure" (arbI @Pressure)
  , staRT "ProgramNum" (arbI @ProgramNum)
  , staRT "PitchBend" (arbI @PitchBend)
  , staRT "Song" (arbI @Song)
  , staRT "Position" (arbI @Position)
  , staRT "ShortManf" (arbI @ShortManf)
  , staRT "LongManf" (arbI @LongManf)
  , genRT "Manf" (arbI @Manf)
  , staRT "QuarterTime" (arbI @QuarterTime)
  , genRT "UnivSysEx" (arbI @UnivSysEx)
  , genRT "ManfSysEx" (arbI @ManfSysEx)
  , genRT "SysExData" (arbI @SysExData)
  , staRT "LiveStatus" (arbI @LiveStatus)
  , staRT "RecStatus" (arbI @RecStatus)
  , staRT "ShortStatus" (arbI @ShortStatus)
  , genRT "MetaString" (fmap MetaString (genSBS 0 255))
  , genRT "MetaData" (arbI @MetaData)
  , genRT "LiveMsg" (arbI @LiveMsg)
  , genRT "RecMsg" (arbI @RecMsg)
  , genRT "ShortMsg" (arbI @ShortMsg)
  , genRT "Track" (arbI @Track)
  , genRT "MidFile" (arbI @MidFile)
  , genRT "SysExDump" (arbI @SysExDump)
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
    ".mid" -> pure (fileRT @MidFile fn fn (shouldFail fn))
    ".syx" -> pure (fileRT @SysExDump fn fn (shouldFail fn))
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
