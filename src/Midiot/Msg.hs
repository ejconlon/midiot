{-# LANGUAGE DeriveAnyClass #-}

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
  ) where

import Control.DeepSeq (NFData)
import Data.ByteString.Short (ShortByteString)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
-- import Midiot.Time (TimeDelta)
-- import Midiot.Time (TimeDelta)
import Data.Word (Word8, Word32, Word16)
import Dahdit (Binary (..), StaticByteSized (..), ByteSized (..), ExactBytes, ViaBinaryRep (..), Word16LE, BinaryRep (..), putWord8, byteSizeFoldable, ViaGeneric (..))
import Data.Sequence (Seq)
import Data.Bits (Bits(..))
import Control.Monad (unless)
import Data.Int (Int16)
import qualified Data.ByteString.Short as BSS
import Data.String (IsString)

newtype Channel = Channel { unChannel :: Word8 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, NFData, Hashable)

newtype Note = Note { unNote :: Word8 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, NFData, Hashable)

newtype Velocity = Velocity { unVelocity :: Word8 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, NFData, Hashable)

newtype ControlNum = ControlNum { unControlNum :: Word8 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, NFData, Hashable)

newtype ControlVal = ControlVal { unControlVal :: Word8 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, NFData, Hashable)

newtype Pressure = Pressure { unPressure :: Word8 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, NFData, Hashable)

newtype ProgramNum = ProgramNum { unProgramNum :: Word8 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, NFData, Hashable)

newtype PitchBend = PitchBend { unPitchBend :: Int16 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, NFData, Hashable)

newtype Song = Song { unSong :: Word8 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, NFData, Hashable)

newtype Position = Position { unPosition :: Word16 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, NFData, Hashable)

newtype Manf = Manf { unManf :: Word8 }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, NFData, Hashable)

eduManf :: Manf
eduManf = Manf 0x7D

data QuarterTime =
    QTFramesLow !Word8
  | QTFramesHigh !Word8
  | QTSecondsLow !Word8
  | QTSecondsHigh !Word8
  | QTMinutesLow !Word8
  | QTMinutesHigh !Word8
  | QTHoursLow !Word8
  | QTHoursHigh !Word8
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

quarterTimeKey :: QuarterTime -> Word8
quarterTimeKey = \case
  QTFramesLow _ -> 0x0
  QTFramesHigh _ -> 0x1
  QTSecondsLow _ -> 0x2
  QTSecondsHigh _ -> 0x3
  QTMinutesLow _ -> 0x4
  QTMinutesHigh _ -> 0x5
  QTHoursLow _ -> 0x6
  QTHoursHigh _ -> 0x7

quarterTimeValue :: QuarterTime -> Word8
quarterTimeValue = \case
  QTFramesLow w -> w
  QTFramesHigh w -> w
  QTSecondsLow w -> w
  QTSecondsHigh w -> w
  QTMinutesLow w -> w
  QTMinutesHigh w -> w
  QTHoursLow w -> w
  QTHoursHigh w -> w

noteOn :: Channel -> Note -> Velocity -> MidiMsg
noteOn c k v = ParsedMidiMsg (MidiChanVoice (ChanVoiceMsg c (ChanVoiceNoteOnOff k v)))

noteOff :: Channel -> Note -> MidiMsg
noteOff c k = noteOn c k 0

data ChanVoiceMsgData =
    ChanVoiceNoteOnOff !Note !Velocity
  | ChanVoicePolyAftertouch !Note !Pressure
  | ChanVoiceCC !ControlNum !ControlVal
  | ChanVoiceProgramChange !ProgramNum
  | ChanVoiceAftertouch !Pressure
  | ChanVoicePitchWheel !PitchBend
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data ChanVoiceMsg =
  ChanVoiceMsg !Channel !ChanVoiceMsgData
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

newtype SysexString = SysexString { unSysexString :: ShortByteString }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString, NFData)

instance ByteSized SysexString where
  byteSize = fromIntegral . BSS.length . unSysexString

instance Binary SysexString where
  get = undefined
  put = undefined

-- TODO(ejconlon) Implement ChannelMode message
-- https://www.midi.org/specifications/item/table-1-summary-of-midi-message
data MidiParsed =
    MidiChanVoice !ChanVoiceMsg
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
    MidiChanVoice (ChanVoiceMsg (Channel c) dat) ->
      let !d = min 15 c
      in case dat of
        ChanVoiceNoteOnOff (Note n) (Velocity v) -> do
          putWord8 (d .|. 0x90)
          putWord8 (min 127 n)
          putWord8 (min 127 v)
        ChanVoicePolyAftertouch (Note n) (Pressure p) -> do
          putWord8 (d .|. 0xA0)
          putWord8 (min 127 n)
          putWord8 (min 127 p)
        ChanVoiceCC (ControlNum cn) (ControlVal cv) -> do
          putWord8 (d .|. 0xB0)
          putWord8 (min 127 cn)
          putWord8 (min 127 cv)
        ChanVoiceProgramChange (ProgramNum pn) -> do
          putWord8 (d .|. 0xC0)
          putWord8 (min 127 pn)
        ChanVoiceAftertouch (Pressure p) -> do
          putWord8 (d .|. 0xD0)
          putWord8 (min 127 p)
        ChanVoicePitchWheel (PitchBend pb) -> do
          putWord8 (d .|. 0xE0)
          let !w = min 16383 (max 0 (pb + 8192))
          putWord8 (fromIntegral w .|. 0x7)
          putWord8 (min 127 (fromIntegral (shiftR w 7)))
    MidiSysex (Manf m) ss -> do
      putWord8 0xF0
      putWord8 (min 127 m)
      put ss
    MidiQuarterFrame qt -> do
      putWord8 0xF1
      putWord8 (quarterTimeKey qt)
      putWord8 (min 127 (quarterTimeValue qt))
    MidiSongPosition (Position p) -> do
      putWord8 0xF2
      putWord8 (fromIntegral p .|. 0x7)
      putWord8 (min 127 (fromIntegral (shiftR p 7)))
    MidiSongSelect (Song s) -> do
      putWord8 0xF3
      putWord8 (min 127 s)
    MidiTuneRequest -> putWord8 0xF6
    MidiSRTClock -> putWord8 0xF8
    MidiSRTStart -> putWord8 0xFA
    MidiSRTContinue -> putWord8 0xFB
    MidiSRTStop -> putWord8 0xFC
    MidiActiveSensing -> putWord8 0xFE
    MidiReset -> putWord8 0xFF

data MidiMsg =
    UnparsedMidiMsg !ShortByteString
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

newtype VarInt = VarInt { unVarInt :: Word32 }
  deriving stock (Show)
  deriving newtype (Eq, NFData, Hashable)

instance ByteSized VarInt where
  byteSize (VarInt w) =
    if
      | w .&. 0xFFFFFF80 == 0 -> 1
      | w .&. 0xFFFFC000 == 0 -> 2
      | w .&. 0xFFE00000 == 0 -> 3
      | otherwise -> 4

instance Binary VarInt where
  get = go 0 0 where
    go !off !acc = do
      w <- get @Word8
      let !wLow = fromIntegral (w .&. 0x7F)
          !wShift = shiftL wLow off
          !accNext = acc .|. wShift
      if w .&. 0x80 == 0
        then pure $! VarInt accNext
        else go (off + 7) accNext

  put (VarInt acc) = go acc where
    go !w = do
      let !wLow = fromIntegral (w .&. 0x7F)
          !wShift = shiftR w 7
      putWord8 wLow
      unless (wShift == 0) (go wShift)

data MidiEvent = MidiEvent
  { meTimeDelta :: !VarInt
  , meMessage :: !MidiMsg
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)
    deriving (ByteSized, Binary) via (ViaGeneric MidiEvent)

-- -- TODO finish this
-- data MidiTrackHeader = MidiTrackHeader
--   , mthLength :: !Word32LE
--   } deriving stock (Eq, Show, Generic)
--     deriving (ByteSized, StaticByteSized, Binary) via (ViaStaticGeneric MidiTrackHeader)

newtype MidiRun = MidiRun { unMidiRun :: Seq MidiEvent }
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
  } deriving stock (Eq, Show, Generic)

instance ByteSized MidiTrack where
  byteSize (MidiTrack _ run) = 4 + byteSize run

instance Binary MidiTrack where
  get = undefined -- TODO
  put = undefined -- TODO

data MidiFileType =
    MidiFileTypeSingle
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
  } deriving stock (Eq, Show, Generic)

instance ByteSized MidiFile where
  byteSize (MidiFile _ _ _ tracks) = 14 + byteSizeFoldable tracks

instance Binary MidiFile where
  get = undefined -- TODO
  put = undefined -- TODO
