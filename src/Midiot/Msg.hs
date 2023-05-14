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
  , Song (..)
  , Position (..)
  , Manf (..)
  , QuarterTimeKey (..)
  , QuarterTime (..)
  , noteOn
  , noteOff
  , ChanVoiceMsg (..)
  , ChanVoiceMsgData (..)
  , SysexString (..)
  , Msg (..)
  , Event (..)
  )
where

import Control.DeepSeq (NFData)
import Control.Newtype (Newtype)
import Dahdit (Binary (..), BinaryRep (..), ByteSized (..), ExactBytes, StaticByteSized (..), ViaBinaryRep (..), ViaBoundedEnum (..), ViaGeneric (..), ViaStaticGeneric (..), Word16LE, byteSizeFoldable)
import Data.Bits (Bits (..))
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as BSS
import Data.Hashable (Hashable)
import Data.Sequence (Seq)
import Data.String (IsString)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Midiot.Arb (Arb (..), ArbEnum (..), ArbGeneric (..))
import Midiot.Binary (BoundedBinary (..), MidiInt14 (..), MidiWord14 (..), MidiWord7 (..), VarWord (..))

newtype Channel = Channel {unChannel :: MidiWord7}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num, Real, Integral, NFData, Hashable, ByteSized, StaticByteSized)
  deriving (Binary) via (BoundedBinary "Channel" Channel MidiWord7)
  deriving (Arb) via (ArbEnum Channel)

instance Newtype Channel MidiWord7

instance Bounded Channel where
  minBound = 0
  maxBound = 15

newtype Note = Note {unNote :: MidiWord7}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num, Real, Integral, NFData, Hashable, ByteSized, StaticByteSized, Binary, Arb)

newtype Velocity = Velocity {unVelocity :: MidiWord7}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num, Real, Integral, NFData, Hashable, ByteSized, StaticByteSized, Binary, Arb)

newtype ControlNum = ControlNum {unControlNum :: MidiWord7}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num, Real, Integral, NFData, Hashable, ByteSized, StaticByteSized, Binary, Arb)

newtype ControlVal = ControlVal {unControlVal :: MidiWord7}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num, Real, Integral, NFData, Hashable, ByteSized, StaticByteSized, Binary, Arb)

newtype Pressure = Pressure {unPressure :: MidiWord7}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num, Real, Integral, NFData, Hashable, ByteSized, StaticByteSized, Binary, Arb)

newtype ProgramNum = ProgramNum {unProgramNum :: MidiWord7}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num, Real, Integral, NFData, Hashable, ByteSized, StaticByteSized, Binary, Arb)

newtype PitchBend = PitchBend {unPitchBend :: MidiInt14}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num, Real, Integral, NFData, Hashable, ByteSized, StaticByteSized, Binary, Arb)

newtype Song = Song {unSong :: MidiWord7}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num, Real, Integral, NFData, Hashable, ByteSized, StaticByteSized, Binary, Arb)

newtype Position = Position {unPosition :: MidiWord14}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num, Real, Integral, NFData, Hashable, ByteSized, StaticByteSized, Binary, Arb)

newtype Manf = Manf {unManf :: MidiWord7}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num, Real, Integral, NFData, Hashable, ByteSized, StaticByteSized, Binary, Arb)

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
  deriving (Arb) via (ArbGeneric QuarterTimeKey)
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
  deriving (Arb) via (ArbGeneric QuarterTime)
  deriving anyclass (NFData)

noteOn :: Channel -> Note -> Velocity -> Msg
noteOn c k v = MsgChanVoice (ChanVoiceMsg c (ChanVoiceNoteOnOff k v))

noteOff :: Channel -> Note -> Msg
noteOff c k = noteOn c k 0

data ChanVoiceMsgData
  = ChanVoiceNoteOnOff !Note !Velocity
  | ChanVoicePolyAftertouch !Note !Pressure
  | ChanVoiceCC !ControlNum !ControlVal
  | ChanVoiceProgramChange !ProgramNum
  | ChanVoiceAftertouch !Pressure
  | ChanVoicePitchWheel !PitchBend
  deriving stock (Eq, Show, Generic)
  deriving (Arb) via (ArbGeneric ChanVoiceMsgData)
  deriving anyclass (NFData)

data ChanVoiceMsg
  = ChanVoiceMsg !Channel !ChanVoiceMsgData
  deriving stock (Eq, Show, Generic)
  deriving (Arb) via (ArbGeneric ChanVoiceMsg)
  deriving anyclass (NFData)

newtype SysexString = SysexString {unSysexString :: ShortByteString}
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString, NFData)

instance Arb SysexString where
  arb = undefined

instance ByteSized SysexString where
  byteSize = fromIntegral . BSS.length . unSysexString

instance Binary SysexString where
  get = undefined
  put = undefined

-- TODO(ejconlon) Implement ChannelMode message
-- https://www.midi.org/specifications/item/table-1-summary-of-midi-message
data Msg
  = MsgChanVoice !ChanVoiceMsg
  | MsgSysex !Manf !SysexString
  | MsgQuarterFrame !QuarterTime
  | MsgSongPosition !Position
  | MsgSongSelect !Song
  | MsgTuneRequest
  | MsgSRTClock
  | MsgSRTStart
  | MsgSRTContinue
  | MsgSRTStop
  | MsgActiveSensing
  | MsgReset
  deriving stock (Eq, Show, Generic)
  deriving (Arb) via (ArbGeneric Msg)
  deriving anyclass (NFData)

instance ByteSized Msg where
  byteSize mp =
    1 + case mp of
      MsgChanVoice (ChanVoiceMsg _ dat) ->
        case dat of
          ChanVoiceNoteOnOff _ _ -> 2
          ChanVoicePolyAftertouch _ _ -> 2
          ChanVoiceCC _ _ -> 2
          ChanVoiceProgramChange _ -> 1
          ChanVoiceAftertouch _ -> 1
          ChanVoicePitchWheel _ -> 2
      MsgSysex _ sbs -> 1 + byteSize sbs
      MsgQuarterFrame _ -> 1
      MsgSongPosition _ -> 2
      MsgSongSelect _ -> 1
      MsgTuneRequest -> 0
      MsgSRTClock -> 0
      MsgSRTStart -> 0
      MsgSRTContinue -> 0
      MsgSRTStop -> 0
      MsgActiveSensing -> 0
      MsgReset -> 0

instance Binary Msg where
  get = do
    x <- get @Word8
    undefined

  put = \case
    MsgChanVoice (ChanVoiceMsg c dat) ->
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
    MsgSysex m ss -> do
      put @Word8 0xF0
      put m
      put ss
    MsgQuarterFrame qt -> do
      put @Word8 0xF1
      put qt
    MsgSongPosition p -> do
      put @Word8 0xF2
      put p
    MsgSongSelect s -> do
      put @Word8 0xF3
      put s
    MsgTuneRequest -> put @Word8 0xF6
    MsgSRTClock -> put @Word8 0xF8
    MsgSRTStart -> put @Word8 0xFA
    MsgSRTContinue -> put @Word8 0xFB
    MsgSRTStop -> put @Word8 0xFC
    MsgActiveSensing -> put @Word8 0xFE
    MsgReset -> put @Word8 0xFF

data Event = Event
  { evTimeDelta :: !VarWord
  , evMessage :: !Msg
  }
  deriving stock (Eq, Show, Generic)
  deriving (ByteSized, Binary) via (ViaGeneric Event)
  deriving (Arb) via (ArbGeneric Event)
  deriving anyclass (NFData)

-- -- TODO finish this
-- data MidiTrackHeader = MidiTrackHeader
--   , mthLength :: !Word32LE
--   } deriving stock (Eq, Show, Generic)
--     deriving (ByteSized, StaticByteSized, Binary) via (ViaStaticGeneric MidiTrackHeader)

newtype Run = Run {unRun :: Seq Event}
  deriving stock (Show)
  deriving newtype (Eq, NFData)

instance Arb Run where
  arb = undefined

instance ByteSized Run where
  byteSize _ = 4 + undefined

instance Binary Run where
  get = undefined
  put = undefined

data Track = Track
  { trackMagic :: !(ExactBytes "MTrk")
  , trackRun :: !Run
  }
  deriving stock (Eq, Show, Generic)

-- deriving (Arb) via (ArbGeneric MidiTrack)
-- deriving anyclass (NFData)

instance ByteSized Track where
  byteSize (Track _ run) = 4 + byteSize run

instance Binary Track where
  get = undefined -- TODO
  put = undefined -- TODO

data FileType
  = FileTypeSingle
  | FileTypeMultiSync
  | FileTypeMultiAsync
  deriving stock (Eq, Ord, Enum, Bounded, Show, Generic)
  deriving (ByteSized, StaticByteSized, Binary) via (ViaBinaryRep FileType)
  deriving (Arb) via (ArbEnum FileType)
  deriving anyclass (NFData)

instance BinaryRep Word16LE FileType where
  fromBinaryRep = \case
    0 -> Right FileTypeSingle
    1 -> Right FileTypeMultiSync
    2 -> Right FileTypeMultiAsync
    other -> Left ("invalid midi file type: " ++ show other)
  toBinaryRep = \case
    FileTypeSingle -> 0
    FileTypeMultiSync -> 1
    FileTypeMultiAsync -> 2

data File = File
  { fileMagic :: !(ExactBytes "MThd\NUL\NUL\NUL\ACK")
  , fileType :: !FileType
  , fileTicks :: !Word16LE
  , fileTracks :: !(Seq Track)
  }
  deriving stock (Eq, Show, Generic)

-- deriving (Arb) via (ArbGeneric MidiTrack)
-- deriving anyclass (NFData)

instance ByteSized File where
  byteSize (File _ _ _ tracks) = 14 + byteSizeFoldable tracks

instance Binary File where
  get = undefined -- TODO
  put = undefined -- TODO
