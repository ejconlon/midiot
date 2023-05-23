module Main (main) where

import Dahdit (Binary (..), ByteCount (..), GetError, StaticByteSized (..), decodeFileEnd)
import qualified Data.ByteString.Char8 as BS
import Data.Proxy (Proxy (..))
import Midiot.Arb (Arb (..), arbSBS)
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
  [ staRT "MidiWord7" (arb @MidiWord7)
  , staRT "MidiInt7" (arb @MidiInt7)
  , staRT "MidiWord14" (arb @MidiWord14)
  , staRT "MidiInt14" (arb @MidiInt14)
  , genRT "VarWord" (arb @VarWord)
  , staRT "Channel" (arb @Channel)
  , staRT "Note" (arb @Note)
  , staRT "Velocity" (arb @Velocity)
  , staRT "ControlNum" (arb @ControlNum)
  , staRT "ControlVal" (arb @ControlVal)
  , staRT "Pressure" (arb @Pressure)
  , staRT "ProgramNum" (arb @ProgramNum)
  , staRT "PitchBend" (arb @PitchBend)
  , staRT "Song" (arb @Song)
  , staRT "Position" (arb @Position)
  , staRT "ShortManf" (arb @ShortManf)
  , staRT "LongManf" (arb @LongManf)
  , genRT "Manf" (arb @Manf)
  , staRT "QuarterTime" (arb @QuarterTime)
  , genRT "UnivSysEx" (arb @UnivSysEx)
  , genRT "ManfSysEx" (arb @ManfSysEx)
  , genRT "SysExData" (arb @SysExData)
  , staRT "LiveStatus" (arb @LiveStatus)
  , staRT "RecStatus" (arb @RecStatus)
  , staRT "ShortStatus" (arb @ShortStatus)
  , genRT "MetaString" (fmap MetaString (arbSBS 0 255))
  , genRT "MetaData" (arb @MetaData)
  , genRT "LiveMsg" (arb @LiveMsg)
  , genRT "RecMsg" (arb @RecMsg)
  , genRT "ShortMsg" (arb @ShortMsg)
  , genRT "Track" (arb @Track)
  , genRT "MidFile" (arb @MidFile)
  , genRT "SysExDump" (arb @SysExDump)
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
