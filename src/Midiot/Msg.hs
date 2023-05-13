{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

module Midiot.Msg
  ( Channel (..)
  , Note (..)
  , Velocity (..)
  , ControlNum (..)
  , ControlVal (..)
  , Pressure (..)
  , ProgramNum (..)
  , PitchBend (..)
  , noteOn
  , noteOff
  , ChanVoiceMsg (..)
  , ChanVoiceMsgData (..)
  , MidiMsg (..)
  -- , MidiEvent (..)
  , MidiParsed (..)
  )
where

import Control.DeepSeq (NFData)
-- import Midiot.Time (TimeDelta)
import Dahdit (Binary (..), BinaryRep (..), ByteSized (..), ExactBytes, StaticByteSized (..), ViaBinaryRep (..), ViaBoundedEnum (..), ViaGeneric (..), ViaStaticGeneric (..), Word16LE, byteSizeFoldable)
import Data.Bits (Bits (..))
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as BSS
import Data.Hashable (Hashable)
import Data.Sequence (Seq)
import Data.String (IsString)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Midiot.Binary (BoundsCheck (..), MidiInt14 (..), MidiWord14 (..), MidiWord7 (..), VarInt (..))

newtype Channel = Channel {unChannel :: MidiWord7}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num, Real, Integral, NFData, Hashable, ByteSized, StaticByteSized)
  deriving (Binary) via (BoundsCheck "Channel" Channel)

instance Bounded Channel where
  minBound = 0
  maxBound = 15

newtype Note = Note {unNote :: MidiWord7}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num, Real, Integral, NFData, Hashable, ByteSized, StaticByteSized, Binary)

newtype Velocity = Velocity {unVelocity :: MidiWord7}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num, Real, Integral, NFData, Hashable, ByteSized, StaticByteSized, Binary)

newtype ControlNum = ControlNum {unControlNum :: MidiWord7}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num, Real, Integral, NFData, Hashable, ByteSized, StaticByteSized, Binary)

newtype ControlVal = ControlVal {unControlVal :: MidiWord7}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num, Real, Integral, NFData, Hashable, ByteSized, StaticByteSized, Binary)

newtype Pressure = Pressure {unPressure :: MidiWord7}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num, Real, Integral, NFData, Hashable, ByteSized, StaticByteSized, Binary)

newtype ProgramNum = ProgramNum {unProgramNum :: MidiWord7}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num, Real, Integral, NFData, Hashable, ByteSized, StaticByteSized, Binary)

newtype PitchBend = PitchBend {unPitchBend :: MidiInt14}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num, Real, Integral, NFData, Hashable, ByteSized, StaticByteSized, Binary)

newtype Song = Song {unSong :: MidiWord7}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num, Real, Integral, NFData, Hashable, ByteSized, StaticByteSized, Binary)

newtype Position = Position {unPosition :: MidiWord14}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num, Real, Integral, NFData, Hashable, ByteSized, StaticByteSized, Binary)

newtype Manf = Manf {unManf :: MidiWord7}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num, Real, Integral, NFData, Hashable, ByteSized, StaticByteSized, Binary)

eduManf :: Manf
eduManf = Manf 0x7D

data QuarterTimeKey
  = QTKFramesLow
  | QTKFramesHigh
  | QTKSecondsLow
  | QTKSecondsHigh
  | QTKMinutesLow
  | QTKMinutesHigh
  | QTKHoursLow
  | QTKHoursHigh
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving (BinaryRep MidiWord7) via (ViaBoundedEnum MidiWord7 QuarterTimeKey)
  deriving (Binary) via (ViaBinaryRep QuarterTimeKey)
  deriving anyclass (NFData)

instance ByteSized QuarterTimeKey where
  byteSize _ = 1

instance StaticByteSized QuarterTimeKey where
  staticByteSize _ = 1

data QuarterTime = QuarterTime
  { qtKey :: !QuarterTimeKey
  , qtVal :: !MidiWord7
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (ByteSized, StaticByteSized, Binary) via (ViaStaticGeneric QuarterTime)
  deriving anyclass (NFData)

noteOn :: Channel -> Note -> Velocity -> MidiMsg
noteOn c k v = ParsedMidiMsg (MidiChanVoice (ChanVoiceMsg c (ChanVoiceNoteOnOff k v)))

noteOff :: Channel -> Note -> MidiMsg
noteOff c k = noteOn c k 0

data ChanVoiceMsgData
  = ChanVoiceNoteOnOff !Note !Velocity
  | ChanVoicePolyAftertouch !Note !Pressure
  | ChanVoiceCC !ControlNum !ControlVal
  | ChanVoiceProgramChange !ProgramNum
  | ChanVoiceAftertouch !Pressure
  | ChanVoicePitchWheel !PitchBend
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data ChanVoiceMsg
  = ChanVoiceMsg !Channel !ChanVoiceMsgData
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

newtype SysexString = SysexString {unSysexString :: ShortByteString}
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString, NFData)

instance ByteSized SysexString where
  byteSize = fromIntegral . BSS.length . unSysexString

instance Binary SysexString where
  get = undefined
  put = undefined

-- TODO(ejconlon) Implement ChannelMode message
-- https://www.midi.org/specifications/item/table-1-summary-of-midi-message
data MidiParsed
  = MidiChanVoice !ChanVoiceMsg
  | MidiSysex !Manf !SysexString
  | MidiQuarterFrame !QuarterTime
  | MidiSongPosition !Position
  | MidiSongSelect !Song
  | MidiTuneRequest
  | MidiSRTClock
  | MidiSRTStart
  | MidiSRTContinue
  | MidiSRTStop
  | MidiActiveSensing
  | MidiReset
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ByteSized MidiParsed where
  byteSize mp =
    1 + case mp of
      MidiChanVoice (ChanVoiceMsg _ dat) ->
        case dat of
          ChanVoiceNoteOnOff _ _ -> 2
          ChanVoicePolyAftertouch _ _ -> 2
          ChanVoiceCC _ _ -> 2
          ChanVoiceProgramChange _ -> 1
          ChanVoiceAftertouch _ -> 1
          ChanVoicePitchWheel _ -> 2
      MidiSysex _ sbs -> 1 + byteSize sbs
      MidiQuarterFrame _ -> 1
      MidiSongPosition _ -> 2
      MidiSongSelect _ -> 1
      MidiTuneRequest -> 0
      MidiSRTClock -> 0
      MidiSRTStart -> 0
      MidiSRTContinue -> 0
      MidiSRTStop -> 0
      MidiActiveSensing -> 0
      MidiReset -> 0

instance Binary MidiParsed where
  get = do
    x <- get @Word8
    undefined

  put = \case
    MidiChanVoice (ChanVoiceMsg c dat) ->
      let d = fromIntegral (unChannel c) :: Word8
      in  case dat of
            ChanVoiceNoteOnOff n v -> do
              put (d .|. 0x90)
              put n
              put v
            ChanVoicePolyAftertouch n p -> do
              put (d .|. 0xA0)
              put n
              put p
            ChanVoiceCC cn cv -> do
              put (d .|. 0xB0)
              put cn
              put cv
            ChanVoiceProgramChange pn -> do
              put (d .|. 0xC0)
              put pn
            ChanVoiceAftertouch p -> do
              put (d .|. 0xD0)
              put p
            ChanVoicePitchWheel pb -> do
              put (d .|. 0xE0)
              put pb
    MidiSysex m ss -> do
      put @Word8 0xF0
      put m
      put ss
    MidiQuarterFrame qt -> do
      put @Word8 0xF1
      put qt
    MidiSongPosition p -> do
      put @Word8 0xF2
      put p
    MidiSongSelect s -> do
      put @Word8 0xF3
      put s
    MidiTuneRequest -> put @Word8 0xF6
    MidiSRTClock -> put @Word8 0xF8
    MidiSRTStart -> put @Word8 0xFA
    MidiSRTContinue -> put @Word8 0xFB
    MidiSRTStop -> put @Word8 0xFC
    MidiActiveSensing -> put @Word8 0xFE
    MidiReset -> put @Word8 0xFF

data MidiMsg
  = UnparsedMidiMsg !ShortByteString
  | ParsedMidiMsg !MidiParsed
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ByteSized MidiMsg where
  byteSize = \case
    UnparsedMidiMsg sbs -> byteSize sbs
    ParsedMidiMsg mp -> byteSize mp

instance Binary MidiMsg where
  get = error "TODO"
  put = error "TODO"

data MidiEvent = MidiEvent
  { meTimeDelta :: !VarInt
  , meMessage :: !MidiMsg
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)
  deriving (ByteSized, Binary) via (ViaGeneric MidiEvent)

-- -- TODO finish this
-- data MidiTrackHeader = MidiTrackHeader
--   , mthLength :: !Word32LE
--   } deriving stock (Eq, Show, Generic)
--     deriving (ByteSized, StaticByteSized, Binary) via (ViaStaticGeneric MidiTrackHeader)

newtype MidiRun = MidiRun {unMidiRun :: Seq MidiEvent}
  deriving stock (Show)
  deriving newtype (Eq, NFData)

instance ByteSized MidiRun where
  byteSize _ = 4 + undefined

instance Binary MidiRun where
  get = undefined
  put = undefined

data MidiTrack = MidiTrack
  { mtMagic :: !(ExactBytes "MTrk")
  , mtRun :: !MidiRun
  }
  deriving stock (Eq, Show, Generic)

instance ByteSized MidiTrack where
  byteSize (MidiTrack _ run) = 4 + byteSize run

instance Binary MidiTrack where
  get = undefined -- TODO
  put = undefined -- TODO

data MidiFileType
  = MidiFileTypeSingle
  | MidiFileTypeMultiSync
  | MidiFileTypeMultiAsync
  deriving stock (Eq, Ord, Enum, Bounded, Show)
  deriving (ByteSized, StaticByteSized, Binary) via (ViaBinaryRep MidiFileType)

instance BinaryRep Word16LE MidiFileType where
  fromBinaryRep = \case
    0 -> Right MidiFileTypeSingle
    1 -> Right MidiFileTypeMultiSync
    2 -> Right MidiFileTypeMultiAsync
    other -> Left ("invalid midi file stype: " ++ show other)
  toBinaryRep = \case
    MidiFileTypeSingle -> 0
    MidiFileTypeMultiSync -> 1
    MidiFileTypeMultiAsync -> 2

data MidiFile = MidiFile
  { mfMagic :: !(ExactBytes "MThd\NUL\NUL\NUL\ACK")
  , mfType :: !MidiFileType
  , mfTicks :: !Word16LE
  , mfTracks :: !(Seq MidiTrack)
  }
  deriving stock (Eq, Show, Generic)

instance ByteSized MidiFile where
  byteSize (MidiFile _ _ _ tracks) = 14 + byteSizeFoldable tracks

instance Binary MidiFile where
  get = undefined -- TODO
  put = undefined -- TODO
