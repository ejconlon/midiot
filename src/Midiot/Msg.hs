{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

module Midiot.Msg
  ( Channel (..)
  , ChannelCount (..)
  , Note (..)
  , Velocity (..)
  , ControlNum (..)
  , ControlVal (..)
  , Pressure (..)
  , ProgramNum (..)
  , PitchBend (..)
  , Song (..)
  , Position (..)
  , ShortManf (..)
  , LongManf (..)
  , Manf (..)
  , QuarterTimeUnit (..)
  , QuarterTime (..)
  , ChanStatus (..)
  , RtStatus (..)
  , CommonStatus (..)
  , LiveStatus (..)
  , RecStatus (..)
  , ChanStatusType (..)
  , ChanVoiceData (..)
  , ChanModeData (..)
  , ChanData (..)
  , UnivSysEx (..)
  , ManfSysEx (..)
  , SysExData (..)
  , CommonData (..)
  , LiveMsg (..)
  , MetaString (..)
  , MetaData (..)
  , RecMsg (..)
  , msgNoteOn
  , msgNoteOff
  , Event (..)
  , Track (..)
  , MidFileType (..)
  , MidFile (..)
  , SysExDump (..)
  )
where

import Control.DeepSeq (NFData)
import Control.Monad (unless, void)
import Control.Newtype (Newtype)
import Dahdit (Binary (..), BinaryRep (..), ByteCount, ByteSized (..), ExactBytes (..), Get, Put, PutM, StaticByteSized (..), ViaBinaryRep (..), ViaGeneric (..), ViaStaticByteSized (..), Word16BE (..), Word32BE (..), byteSizeFoldable, getByteString, getExact, getExpect, getLookAhead, getRemainingSeq, getRemainingSize, getSeq, putByteString, putSeq)
import Data.Bits (Bits (..))
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as BSS
import Data.Hashable (Hashable)
import qualified Data.List.NonEmpty as NE
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.ShortWord (Word4)
import Data.String (IsString (..))
import Data.Word (Word16, Word8)
import GHC.Generics (Generic)
import Midiot.Arb (Arb (..), ArbEnum (..), ArbGeneric (..), arbList, arbSBS, arbSeq, genSum, genUnsigned)
import Midiot.Binary (BoundedBinary (..), MidiInt14 (..), MidiWord14 (..), MidiWord7 (..), VarWord (..))
import Test.Falsify.Generator (Gen)
import qualified Test.Falsify.Generator as FG
import qualified Test.Falsify.Range as FR

newtype Channel = Channel {unChannel :: MidiWord7}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num, Real, Integral, NFData, Hashable, ByteSized, StaticByteSized)
  deriving (Binary) via (BoundedBinary "Channel" Channel MidiWord7)
  deriving (Arb) via (ArbEnum Channel)

instance Newtype Channel MidiWord7

instance Bounded Channel where
  minBound = 0
  maxBound = 15

newtype ChannelCount = ChannelCount {unChannelCount :: MidiWord7}
  deriving stock (Show)
  deriving newtype
    (Eq, Ord, Enum, Num, Real, Integral, NFData, Hashable, ByteSized, StaticByteSized, Binary, Arb)

newtype Note = Note {unNote :: MidiWord7}
  deriving stock (Show)
  deriving newtype
    (Eq, Ord, Enum, Num, Real, Integral, NFData, Hashable, ByteSized, StaticByteSized, Binary, Arb)

newtype Velocity = Velocity {unVelocity :: MidiWord7}
  deriving stock (Show)
  deriving newtype
    (Eq, Ord, Enum, Num, Real, Integral, NFData, Hashable, ByteSized, StaticByteSized, Binary, Arb)

newtype ControlNum = ControlNum {unControlNum :: MidiWord7}
  deriving stock (Show)
  deriving newtype
    (Eq, Ord, Enum, Num, Real, Integral, NFData, Hashable, ByteSized, StaticByteSized, Binary, Arb)

newtype ControlVal = ControlVal {unControlVal :: MidiWord7}
  deriving stock (Show)
  deriving newtype
    (Eq, Ord, Enum, Num, Real, Integral, NFData, Hashable, ByteSized, StaticByteSized, Binary, Arb)

newtype Pressure = Pressure {unPressure :: MidiWord7}
  deriving stock (Show)
  deriving newtype
    (Eq, Ord, Enum, Num, Real, Integral, NFData, Hashable, ByteSized, StaticByteSized, Binary, Arb)

newtype ProgramNum = ProgramNum {unProgramNum :: MidiWord7}
  deriving stock (Show)
  deriving newtype
    (Eq, Ord, Enum, Num, Real, Integral, NFData, Hashable, ByteSized, StaticByteSized, Binary, Arb)

newtype PitchBend = PitchBend {unPitchBend :: MidiInt14}
  deriving stock (Show)
  deriving newtype
    (Eq, Ord, Enum, Num, Real, Integral, NFData, Hashable, ByteSized, StaticByteSized, Binary, Arb)

newtype Song = Song {unSong :: MidiWord7}
  deriving stock (Show)
  deriving newtype
    (Eq, Ord, Enum, Num, Real, Integral, NFData, Hashable, ByteSized, StaticByteSized, Binary, Arb)

newtype Position = Position {unPosition :: MidiWord14}
  deriving stock (Show)
  deriving newtype
    (Eq, Ord, Enum, Num, Real, Integral, NFData, Hashable, ByteSized, StaticByteSized, Binary, Arb)

newtype ShortManf = ShortManf {unShortManf :: MidiWord7}
  deriving stock (Show)
  deriving newtype
    (Eq, Ord, Enum, Num, Real, Integral, NFData, Hashable, ByteSized, StaticByteSized, Binary)

instance Arb ShortManf where
  arb = do
    i <- arb @MidiWord7
    if i == 0x00 || i == 0x7E || i == 0x7F
      then arb
      else pure (ShortManf i)

newtype LongManf = LongManf {unLongManf :: Word16}
  deriving stock (Show)
  deriving newtype
    (Eq, Ord, Enum, Num, Real, Integral, NFData, Hashable, ByteSized, StaticByteSized, Binary, Arb)

data Manf = ManfShort !ShortManf | ManfLong !LongManf
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arb) via (ArbGeneric Manf)

instance ByteSized Manf where
  byteSize = \case
    ManfShort _ -> 1
    ManfLong _ -> 3

instance Binary Manf where
  get = do
    sm <- get @ShortManf
    if sm == 0
      then fmap (ManfLong . LongManf) get
      else pure (ManfShort sm)
  put = \case
    ManfShort sm -> put sm
    ManfLong lm -> put @Word8 0 *> put lm

-- | Manf id usable for non-commercial applications
eduManf :: Manf
eduManf = ManfShort (ShortManf 0x7D)

data QuarterTimeUnit
  = QTUFramesLow
  | QTUFramesHigh
  | QTUSecondsLow
  | QTUSecondsHigh
  | QTUMinutesLow
  | QTUMinutesHigh
  | QTUHoursLow
  | QTUHoursHigh
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving (Arb) via (ArbGeneric QuarterTimeUnit)
  deriving anyclass (NFData)

data QuarterTime = QuarterTime
  { qtUnit :: !QuarterTimeUnit
  , qtValue :: !Word4
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

instance Arb QuarterTime where
  arb = QuarterTime <$> arb <*> genUnsigned

instance ByteSized QuarterTime where
  byteSize _ = 1

instance StaticByteSized QuarterTime where
  staticByteSize _ = 1

instance Binary QuarterTime where
  get = do
    w <- get @Word8
    let x = shiftR w 4
    unless (x < 8) (fail ("Invalid quarter time unit: " ++ show x))
    let unit = toEnum (fromIntegral x)
        val = fromIntegral (0x0F .&. w)
    pure (QuarterTime unit val)
  put (QuarterTime unit val) =
    put @Word8 (shiftL (fromIntegral (fromEnum unit)) 4 .|. fromIntegral val)

data ChanStatusType
  = ChanStatusNoteOff
  | ChanStatusNoteOn
  | ChanStatusKeyAftertouch
  | ChanStatusControlChange
  | ChanStatusProgramChange
  | ChanStatusChanAftertouch
  | ChanStatusPitchBend
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving (Arb) via (ArbEnum ChanStatusType)

data CommonStatus
  = CommonStatusTimeFrame
  | CommonStatusSongPointer
  | CommonStatusSongSelect
  | CommonStatusTuneRequest
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving (Arb) via (ArbEnum CommonStatus)

data RtStatus
  = RtStatusTimingClock
  | RtStatusStart
  | RtStatusContinue
  | RtStatusStop
  | RtStatusActiveSensing
  | RtStatusSystemReset
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving (Arb) via (ArbEnum RtStatus)

data ChanStatus = ChanStatus !Channel !ChanStatusType
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arb) via (ArbGeneric ChanStatus)

-- private
data StatusPeek
  = StatusPeekYes
  | StatusPeekNo !Word8
  deriving stock (Eq, Show)

-- private
peekStatus :: Get StatusPeek
peekStatus = getLookAhead $ do
  b <- get @Word8
  pure $!
    if b .&. 0x80 == 0
      then StatusPeekNo b
      else StatusPeekYes

class HasChanStatus s where
  statusIsChan :: s -> Bool
  statusAsChan :: s -> Maybe ChanStatus
  statusFromChan :: ChanStatus -> s

data LiveStatus
  = LiveStatusChan !ChanStatus
  | LiveStatusSysEx
  | LiveStatusSysCommon !CommonStatus
  | LiveStatusSysRt !RtStatus
  deriving stock (Eq, Ord, Show, Generic)
  deriving (ByteSized) via (ViaStaticByteSized LiveStatus)
  deriving (Arb) via (ArbGeneric LiveStatus)

instance StaticByteSized LiveStatus where
  staticByteSize _ = 1

instance HasChanStatus LiveStatus where
  statusIsChan = \case
    LiveStatusChan _ -> True
    _ -> False

  statusAsChan = \case
    LiveStatusChan cs -> Just cs
    _ -> Nothing

  statusFromChan = LiveStatusChan

instance Binary LiveStatus where
  get = do
    b <- get @Word8
    let x = b .&. 0xF0
    if
        | x < 0x80 -> fail ("Live status byte with high bit clear: " ++ show b)
        | x == 0xF0 ->
            case b of
              0xF0 -> pure LiveStatusSysEx
              0xF1 -> pure (LiveStatusSysCommon CommonStatusTimeFrame)
              0xF2 -> pure (LiveStatusSysCommon CommonStatusSongPointer)
              0xF3 -> pure (LiveStatusSysCommon CommonStatusSongSelect)
              0xF6 -> pure (LiveStatusSysCommon CommonStatusTuneRequest)
              0xF8 -> pure (LiveStatusSysRt RtStatusTimingClock)
              0xFA -> pure (LiveStatusSysRt RtStatusStart)
              0xFB -> pure (LiveStatusSysRt RtStatusContinue)
              0xFC -> pure (LiveStatusSysRt RtStatusStop)
              0xFE -> pure (LiveStatusSysRt RtStatusActiveSensing)
              0xFF -> pure (LiveStatusSysRt RtStatusSystemReset)
              _ -> fail ("Unknown system status byte: " ++ show b)
        | otherwise -> do
            let c = Channel (fromIntegral (b .&. 0x0F))
            pure $ LiveStatusChan $ ChanStatus c $ case x of
              0x80 -> ChanStatusNoteOff
              0x90 -> ChanStatusNoteOn
              0xA0 -> ChanStatusKeyAftertouch
              0xB0 -> ChanStatusControlChange
              0xC0 -> ChanStatusProgramChange
              0xD0 -> ChanStatusChanAftertouch
              0xE0 -> ChanStatusPitchBend
              _ -> error "impossible"
  put = \case
    LiveStatusChan (ChanStatus c cs) ->
      let d = fromIntegral (unChannel c)
          x = case cs of
            ChanStatusNoteOff -> 0x80
            ChanStatusNoteOn -> 0x90
            ChanStatusKeyAftertouch -> 0xA0
            ChanStatusControlChange -> 0xB0
            ChanStatusProgramChange -> 0xC0
            ChanStatusChanAftertouch -> 0xD0
            ChanStatusPitchBend -> 0xE0
      in  put @Word8 (d .|. x)
    LiveStatusSysEx -> put @Word8 0xF0
    LiveStatusSysCommon cs ->
      let x = case cs of
            CommonStatusTimeFrame -> 0x01
            CommonStatusSongPointer -> 0x02
            CommonStatusSongSelect -> 0x03
            CommonStatusTuneRequest -> 0x06
      in  put @Word8 (0xF0 .|. x)
    LiveStatusSysRt rs ->
      let !x = case rs of
            RtStatusTimingClock -> 0x00
            RtStatusStart -> 0x02
            RtStatusContinue -> 0x03
            RtStatusStop -> 0x04
            RtStatusActiveSensing -> 0x06
            RtStatusSystemReset -> 0x7
      in  put @Word8 (0xF8 .|. x)

data RecStatus
  = RecStatusChan !ChanStatus
  | RecStatusSysEx
  | RecStatusMeta
  deriving stock (Eq, Ord, Show, Generic)
  deriving (ByteSized) via (ViaStaticByteSized RecStatus)
  deriving (Arb) via (ArbGeneric RecStatus)

instance StaticByteSized RecStatus where
  staticByteSize _ = 1

instance HasChanStatus RecStatus where
  statusIsChan = \case
    RecStatusChan _ -> True
    _ -> False

  statusAsChan = \case
    RecStatusChan cs -> Just cs
    _ -> Nothing

  statusFromChan = RecStatusChan

instance Binary RecStatus where
  get = do
    b <- get @Word8
    let x = b .&. 0xF0
    if
        | x < 0x80 -> fail ("Rec status byte with high bit clear: " ++ show b)
        | x == 0xF0 ->
            case b of
              0xF0 -> pure RecStatusSysEx
              0xFF -> pure RecStatusMeta
              _ -> fail ("Unknown rec status byte: " ++ show b)
        | otherwise -> do
            let c = Channel (fromIntegral (b .&. 0x0F))
            pure $ RecStatusChan $ ChanStatus c $ case x of
              0x80 -> ChanStatusNoteOff
              0x90 -> ChanStatusNoteOn
              0xA0 -> ChanStatusKeyAftertouch
              0xB0 -> ChanStatusControlChange
              0xC0 -> ChanStatusProgramChange
              0xD0 -> ChanStatusChanAftertouch
              0xE0 -> ChanStatusPitchBend
              _ -> error "impossible"
  put = \case
    RecStatusChan (ChanStatus c cs) ->
      let d = fromIntegral (unChannel c)
          x = case cs of
            ChanStatusNoteOff -> 0x80
            ChanStatusNoteOn -> 0x90
            ChanStatusKeyAftertouch -> 0xA0
            ChanStatusControlChange -> 0xB0
            ChanStatusProgramChange -> 0xC0
            ChanStatusChanAftertouch -> 0xD0
            ChanStatusPitchBend -> 0xE0
      in  put @Word8 (d .|. x)
    RecStatusSysEx -> put @Word8 0xF0
    RecStatusMeta -> put @Word8 0xFF

-- | A string prefixed by a single-byte length
newtype MetaString = MetaString {unMetaString :: ShortByteString}
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

instance ByteSized MetaString where
  byteSize = succ . fromIntegral . BSS.length . unMetaString

instance Binary MetaString where
  get = do
    len <- get @Word8
    bss <- getByteString (fromIntegral len)
    pure (MetaString bss)
  put (MetaString s) = do
    let len = BSS.length s
    if len > 255
      then do
        put @Word8 255
        putByteString (BSS.take 255 s)
      else do
        put @Word8 (fromIntegral len)
        putByteString s

data MetaData = MetaData
  { mdType :: !Word8
  , mdBody :: !MetaString
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (ByteSized, Binary) via (ViaGeneric MetaData)

instance Arb MetaData where
  arb = MetaData <$> arb <*> fmap MetaString (arbSBS 0 3)

-- newtype Tempo = Tempo {unTempo :: Word24BE}
--   deriving stock (Show)
--   deriving newtype (Eq, Ord, ByteSized, StaticByteSized, Binary)

-- data MetaData
--   = MDSeqNum !Word16
--   | MDText !MetaString
--   | MDCopyright !MetaString
--   | MDSeqName !MetaString
--   | MDInstName !MetaString
--   | MDLyrics !MetaString
--   | MDMarker !MetaString
--   | MDCuePoint !MetaString
--   | MDChanPrefix !Channel
--   | MDEndTrack
--   | MDSetTempo !Tempo
--   | MDSmpteOffset !Word8 !Word8 !Word8 !Word8 !Word8
--   | MDTimeSig !Word8 !Word8 !Word8 !Word8
--   | MDKeySig !Word8 !Word8
--   | MDSeqSpecific !SysExString
--   deriving stock (Eq, Ord, Show)

-- instance Arb MetaData where
--   arb = genMD where
--     genMD = genSum $ NE.fromList
--       [ MDSeqNum <$> arb
--       , MDText <$> genS
--       , MDCopyright <$> genS
--       , MDSeqName <$> genS
--       , MDInstName <$> genS
--       , MDLyrics <$> genS
--       , MDMarker <$> genS
--       , MDCuePoint <$> genS
--       , MDChanPrefix <$> arb
--       , pure MDEndTrack
--       -- TODO fill in the rest
--       ]
--     genS = MetaString <$> arbSBS 0 3

-- instance ByteSized MetaData where
--   byteSize = succ . \case
--     MDSeqNum _ -> 2
--     MDText t -> byteSize t
--     MDCopyright t -> byteSize t
--     MDSeqName t -> byteSize t
--     MDInstName t -> byteSize t
--     MDLyrics t -> byteSize t
--     MDMarker t -> byteSize t
--     MDCuePoint t -> byteSize t
--     MDChanPrefix _ -> 1
--     MDEndTrack -> 0
--     MDSetTempo _ -> 3
--     MDSmpteOffset {} -> 5
--     MDTimeSig {} -> 4
--     MDKeySig _ _ -> 2
--     MDSeqSpecific ss -> byteSize ss

-- instance Binary MetaData where
--   get = do
--     m <- get @Word8
--     case m of
--       0x00 -> fmap (MDSeqNum . unWord16BE) get
--       0x01 -> fmap MDText get
--       0x02 -> fmap MDCopyright get
--       0x03 -> fmap MDSeqName get
--       0x04 -> fmap MDInstName get
--       0x05 -> fmap MDLyrics get
--       0x06 -> fmap MDMarker get
--       0x07 -> fmap MDCuePoint get
--       0x20 -> fmap MDChanPrefix get
--       0x2F -> pure MDEndTrack
--       0x51 -> fmap MDSetTempo get
--       0x54 -> MDSmpteOffset <$> get <*> get <*> get <*> get <*> get
--       0x58 -> MDTimeSig <$> get <*> get <*> get <*> get
--       0x59 -> MDKeySig <$> get <*> get
--       0x7F -> fmap MDSeqSpecific get
--       _ -> fail ("Unknown metadata type: " ++ show m)
--   put = \case
--     MDSeqNum w -> put @Word8 0x00 *> put (Word16BE w)
--     MDText s -> put @Word8 0x01 *> put s
--     MDCopyright s -> put @Word8 0x02 *> put s
--     MDSeqName s -> put @Word8 0x03 *> put s
--     MDInstName s -> put @Word8 0x04 *> put s
--     MDLyrics s -> put @Word8 0x05 *> put s
--     MDMarker s -> put @Word8 0x06 *> put s
--     MDCuePoint s -> put @Word8 0x07 *> put s
--     MDChanPrefix c -> put @Word8 0x20 *> put c
--     MDEndTrack -> put @Word8 0x2F
--     MDSetTempo t -> put @Word8 0x51 *> put t
--     MDSmpteOffset {} -> put @Word8 0x54 *> error "TODO"
--     MDTimeSig {} -> put @Word8 0x58 *> error "TODO"
--     MDKeySig {} -> put @Word8 0x59 *> error "TODO"
--     MDSeqSpecific ss -> put @Word8 0x7F *> put ss

data ChanVoiceData
  = ChanVoiceDataNoteOff !Note !Velocity
  | ChanVoiceDataNoteOn !Note !Velocity
  | ChanVoiceKeyAftertouch !Note !Pressure
  | ChanVoiceControlChange !ControlNum !ControlVal
  | ChanVoiceProgramChange !ProgramNum
  | ChanVoiceChanAftertouch !Pressure
  | ChanVoicePitchBend !PitchBend
  deriving stock (Eq, Ord, Show)

instance Arb ChanVoiceData where
  arb = genCVD
   where
    genCVD =
      genSum $
        NE.fromList
          [ ChanVoiceDataNoteOff <$> arb <*> arb
          , ChanVoiceDataNoteOn <$> arb <*> arb
          , ChanVoiceKeyAftertouch <$> arb <*> arb
          , ChanVoiceControlChange <$> genCN <*> arb
          , ChanVoiceProgramChange <$> arb
          , ChanVoiceChanAftertouch <$> arb
          , ChanVoicePitchBend <$> arb
          ]
    genCN = fmap (ControlNum . MidiWord7) (FG.integral (FR.between (0x00, 0x77)))

instance ByteSized ChanVoiceData where
  byteSize = \case
    ChanVoiceDataNoteOff _ _ -> 2
    ChanVoiceDataNoteOn _ _ -> 2
    ChanVoiceKeyAftertouch _ _ -> 2
    ChanVoiceControlChange _ _ -> 2
    ChanVoiceProgramChange _ -> 1
    ChanVoiceChanAftertouch _ -> 1
    ChanVoicePitchBend _ -> 2

-- private
putChanVoiceData :: ChanVoiceData -> Put
putChanVoiceData = \case
  ChanVoiceDataNoteOff n v -> put n *> put v
  ChanVoiceDataNoteOn n v -> put n *> put v
  ChanVoiceKeyAftertouch n p -> put n *> put p
  ChanVoiceControlChange cn cv -> put cn *> put cv
  ChanVoiceProgramChange pn -> put pn
  ChanVoiceChanAftertouch p -> put p
  ChanVoicePitchBend pb -> put pb

data ChanModeData
  = ChanModeAllSoundOff
  | ChanModeResetAllControllers
  | ChanModeLocalControlOff
  | ChanModeLocalControlOn
  | ChanModeAllNotesOff
  | ChanModeOmniOff
  | ChanModeOmniOn
  | ChanModeMonoOn !ChannelCount
  | ChanModeMonoOff
  deriving stock (Eq, Ord, Show, Generic)
  deriving (ByteSized) via (ViaStaticByteSized ChanModeData)
  deriving (Arb) via (ArbGeneric ChanModeData)

instance StaticByteSized ChanModeData where
  staticByteSize _ = 2

-- private
putChanModeData :: ChanModeData -> Put
putChanModeData = \case
  ChanModeAllSoundOff -> do
    put @Word8 0x78
    put @Word8 0
  ChanModeResetAllControllers -> do
    put @Word8 0x79
    put @Word8 0
  ChanModeLocalControlOff -> do
    put @Word8 0x7A
    put @Word8 0
  ChanModeLocalControlOn -> do
    put @Word8 0x7A
    put @Word8 0x7F
  ChanModeAllNotesOff -> do
    put @Word8 0x7B
    put @Word8 0
  ChanModeOmniOff -> do
    put @Word8 0x7C
    put @Word8 0
  ChanModeOmniOn -> do
    put @Word8 0x7D
    put @Word8 0
  ChanModeMonoOn cc -> do
    put @Word8 0x7E
    put cc
  ChanModeMonoOff -> do
    put @Word8 0x7F
    put @Word8 0

data ChanData
  = ChanDataVoice !ChanVoiceData
  | ChanDataMode !ChanModeData
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arb) via (ArbGeneric ChanData)

instance ByteSized ChanData where
  byteSize = \case
    ChanDataVoice cvd -> byteSize cvd
    ChanDataMode cmd -> byteSize cmd

chanDataType :: ChanData -> ChanStatusType
chanDataType = \case
  ChanDataVoice cvd -> case cvd of
    ChanVoiceDataNoteOff _ _ -> ChanStatusNoteOff
    ChanVoiceDataNoteOn _ _ -> ChanStatusNoteOn
    ChanVoiceKeyAftertouch _ _ -> ChanStatusKeyAftertouch
    ChanVoiceControlChange _ _ -> ChanStatusControlChange
    ChanVoiceProgramChange _ -> ChanStatusProgramChange
    ChanVoiceChanAftertouch _ -> ChanStatusChanAftertouch
    ChanVoicePitchBend _ -> ChanStatusPitchBend
  ChanDataMode _ -> ChanStatusControlChange

-- private
getChanData :: ChanStatus -> Get ChanData
getChanData (ChanStatus _ ty) = case ty of
  ChanStatusNoteOff -> do
    n <- get @Note
    v <- get @Velocity
    pure (ChanDataVoice (ChanVoiceDataNoteOff n v))
  ChanStatusNoteOn -> do
    n <- get @Note
    v <- get @Velocity
    pure (ChanDataVoice (ChanVoiceDataNoteOn n v))
  ChanStatusKeyAftertouch -> do
    n <- get @Note
    p <- get @Pressure
    pure (ChanDataVoice (ChanVoiceKeyAftertouch n p))
  ChanStatusControlChange -> do
    cn <- get @ControlNum
    cv <- get @ControlVal
    case unControlNum cn of
      0x78 -> do
        unless (cv == 0) (fail "Chan mode all sound off must have value 0")
        pure (ChanDataMode ChanModeAllSoundOff)
      0x79 -> do
        unless (cv == 0) (fail "Chan mode reset all controllers must have value 0")
        pure (ChanDataMode ChanModeResetAllControllers)
      0x7A -> do
        case unControlVal cv of
          0 -> pure (ChanDataMode ChanModeLocalControlOff)
          0x7F -> pure (ChanDataMode ChanModeLocalControlOn)
          _ -> fail "Chan mode local control must be 0 or 127"
      0x7B -> do
        unless (cv == 0) (fail "Chan mode all notes off must have value 0")
        pure (ChanDataMode ChanModeAllNotesOff)
      0x7C -> do
        unless (cv == 0) (fail "Chan mode omni off must have value 0")
        pure (ChanDataMode ChanModeOmniOff)
      0x7D -> do
        unless (cv == 0) (fail "Chan mode omni on must have value 0")
        pure (ChanDataMode ChanModeOmniOn)
      0x7E ->
        pure (ChanDataMode (ChanModeMonoOn (ChannelCount (unControlVal cv))))
      0x7F -> do
        unless (cv == 0) (fail "Chan mode mono off must have value 0")
        pure (ChanDataMode ChanModeMonoOff)
      _ -> pure (ChanDataVoice (ChanVoiceControlChange cn cv))
  ChanStatusProgramChange -> do
    pn <- get @ProgramNum
    pure (ChanDataVoice (ChanVoiceProgramChange pn))
  ChanStatusChanAftertouch -> do
    p <- get @Pressure
    pure (ChanDataVoice (ChanVoiceChanAftertouch p))
  ChanStatusPitchBend -> do
    pb <- get @PitchBend
    pure (ChanDataVoice (ChanVoicePitchBend pb))

putChanData :: ChanData -> Put
putChanData = \case
  ChanDataVoice cvd -> putChanVoiceData cvd
  ChanDataMode cmd -> putChanModeData cmd

-- Gets bytestring until delimiter (consuming delimiter but not including in string)
getPayload :: Get ShortByteString
getPayload = go
 where
  go = do
    len <- getLookAhead (goFind 0)
    s <- getByteString len
    _ <- get @Word8
    pure s
  goFind !i = do
    w <- get @Word8
    if w == 0xF7
      then pure i
      else goFind (i + 1)

-- Generate a bytestring not including the delimiter
genPayload :: Gen ShortByteString
genPayload = fmap (BSS.pack . fmap fromIntegral) (arbList @MidiWord7 0 3)

data UnivSysEx = UnivSysEx
  { useSubId :: !Word8
  , usePayload :: !ShortByteString
  }
  deriving stock (Eq, Ord, Show)

instance Arb UnivSysEx where
  arb = UnivSysEx <$> FG.choose (pure 0x7E) (pure 0x7F) <*> genPayload

instance ByteSized UnivSysEx where
  byteSize (UnivSysEx _ p) = 2 + byteSize p

instance Binary UnivSysEx where
  get = do
    i <- get @Word8
    unless (i == 0x7E || i == 0x7F) (fail ("Expected universal sys ex id: " ++ show i))
    fmap (UnivSysEx i) getPayload
  put (UnivSysEx i s) = do
    put i
    putByteString s
    put @Word8 0xF7

data ManfSysEx = ManfSysEx
  { mseManf :: !Manf
  , msePayload :: !ShortByteString
  }
  deriving stock (Eq, Ord, Show)

instance Arb ManfSysEx where
  arb = ManfSysEx <$> arb <*> genPayload

instance ByteSized ManfSysEx where
  byteSize (ManfSysEx m p) = 1 + byteSize m + byteSize p

instance Binary ManfSysEx where
  get = do
    m <- get
    fmap (ManfSysEx m) getPayload
  put (ManfSysEx m s) = do
    put m
    putByteString s
    put @Word8 0xF7

data SysExData
  = SysExDataUniv !UnivSysEx
  | SysExDataManf !ManfSysEx
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arb) via (ArbGeneric SysExData)

instance ByteSized SysExData where
  byteSize = \case
    SysExDataUniv x -> byteSize x
    SysExDataManf y -> byteSize y

instance Binary SysExData where
  get = do
    peek <- getLookAhead (get @Word8)
    if peek == 0x7E || peek == 0x7F
      then fmap SysExDataUniv get
      else fmap SysExDataManf get
  put = \case
    SysExDataUniv x -> put x
    SysExDataManf y -> put y

data CommonData
  = CommonDataTimeFrame !QuarterTime
  | CommonDataSongPointer !Position
  | CommonDataSongSelect !Song
  | CommonDataTuneRequest
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arb) via (ArbGeneric CommonData)

instance ByteSized CommonData where
  byteSize = \case
    CommonDataTimeFrame _ -> 1
    CommonDataSongPointer _ -> 2
    CommonDataSongSelect _ -> 1
    CommonDataTuneRequest -> 0

getCommonData :: CommonStatus -> Get CommonData
getCommonData = \case
  CommonStatusTimeFrame -> fmap CommonDataTimeFrame get
  CommonStatusSongPointer -> fmap CommonDataSongPointer get
  CommonStatusSongSelect -> fmap CommonDataSongSelect get
  CommonStatusTuneRequest -> pure CommonDataTuneRequest

putCommonData :: CommonData -> Put
putCommonData = \case
  CommonDataTimeFrame qt -> put qt
  CommonDataSongPointer po -> put po
  CommonDataSongSelect so -> put so
  CommonDataTuneRequest -> pure ()

class HasChanStatus s => HasChanData s c | c -> s where
  extractStatus :: c -> s
  embedChanData :: Channel -> ChanData -> c

data LiveMsg
  = LiveMsgChan !Channel !ChanData
  | LiveMsgSysEx !SysExData
  | LiveMsgSysCommon !CommonData
  | LiveMsgSysRt !RtStatus
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arb) via (ArbGeneric LiveMsg)

instance HasChanData LiveStatus LiveMsg where
  extractStatus = liveMsgStatus
  embedChanData = LiveMsgChan

instance ByteSized LiveMsg where
  byteSize =
    succ . \case
      LiveMsgChan _ cd -> byteSize cd
      LiveMsgSysEx sed -> byteSize sed
      LiveMsgSysCommon cd -> byteSize cd
      LiveMsgSysRt _ -> 0

instance Binary LiveMsg where
  get = get @LiveStatus >>= getLiveMsgWithStatus
  put = void . putMsgRunning putLiveMsgData Nothing

-- private
getLiveMsgWithStatus :: LiveStatus -> Get LiveMsg
getLiveMsgWithStatus = \case
  LiveStatusChan cs@(ChanStatus chan _) -> fmap (LiveMsgChan chan) (getChanData cs)
  LiveStatusSysEx -> fmap LiveMsgSysEx get
  LiveStatusSysCommon cs -> fmap LiveMsgSysCommon (getCommonData cs)
  LiveStatusSysRt rs -> pure (LiveMsgSysRt rs)

-- private
-- Running status is for Voice and Mode messages only!
getMsgRunning :: (Binary s, HasChanStatus s) => (s -> Get c) -> Maybe ChanStatus -> Get c
getMsgRunning getter mayLastStatus = do
  peeked <- peekStatus
  case peeked of
    StatusPeekYes -> do
      status <- get
      getter status
    StatusPeekNo dat ->
      case mayLastStatus of
        Nothing -> fail ("Expected status byte (no running status): " ++ show dat)
        Just lastStatus -> getter (statusFromChan lastStatus)

-- private
putMsgRunning :: (HasChanData s c, Binary s) => (c -> Put) -> Maybe ChanStatus -> c -> PutM (Maybe ChanStatus)
putMsgRunning putter mayLastStatus msg = do
  mayCurStatus <- putMsgStatusRunning mayLastStatus msg
  putter msg
  pure mayCurStatus

-- private
putMsgStatusRunning :: (HasChanData s c, Binary s) => Maybe ChanStatus -> c -> PutM (Maybe ChanStatus)
putMsgStatusRunning mayLastStatus msg =
  let curStatus = extractStatus msg
      mayChanStatus = statusAsChan curStatus
  in  case mayLastStatus of
        Nothing -> do
          put curStatus
          pure mayChanStatus
        Just lastStatus ->
          case mayChanStatus of
            Just chanStatus ->
              if chanStatus == lastStatus
                then pure mayLastStatus
                else Just chanStatus <$ put curStatus
            _ -> Nothing <$ put curStatus

-- private
putLiveMsgData :: LiveMsg -> Put
putLiveMsgData = \case
  LiveMsgChan _ cd -> putChanData cd
  LiveMsgSysEx sed -> put sed
  LiveMsgSysCommon cd -> putCommonData cd
  LiveMsgSysRt _ -> pure ()

liveMsgStatus :: LiveMsg -> LiveStatus
liveMsgStatus = \case
  LiveMsgChan chan cd -> LiveStatusChan (ChanStatus chan (chanDataType cd))
  LiveMsgSysEx _ -> LiveStatusSysEx
  LiveMsgSysCommon cd -> LiveStatusSysCommon $ case cd of
    CommonDataTimeFrame _ -> CommonStatusTimeFrame
    CommonDataSongPointer _ -> CommonStatusSongPointer
    CommonDataSongSelect _ -> CommonStatusSongSelect
    CommonDataTuneRequest -> CommonStatusTuneRequest
  LiveMsgSysRt rs -> LiveStatusSysRt rs

msgNoteOn :: HasChanData s c => Channel -> Note -> Velocity -> c
msgNoteOn c k v = embedChanData c (ChanDataVoice (ChanVoiceDataNoteOn k v))

msgNoteOff :: Channel -> Note -> LiveMsg
msgNoteOff c k = msgNoteOn c k 0

data RecMsg
  = RecMsgChan !Channel !ChanData
  | RecMsgSysEx !SysExData
  | RecMsgMeta !MetaData
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arb) via (ArbGeneric RecMsg)

recMsgStatus :: RecMsg -> RecStatus
recMsgStatus = \case
  RecMsgChan chan cd -> RecStatusChan (ChanStatus chan (chanDataType cd))
  RecMsgSysEx _ -> RecStatusSysEx
  RecMsgMeta _ -> RecStatusMeta

instance HasChanData RecStatus RecMsg where
  extractStatus = recMsgStatus
  embedChanData = RecMsgChan

instance ByteSized RecMsg where
  byteSize =
    succ . \case
      RecMsgChan _ cd -> byteSize cd
      RecMsgSysEx sed -> byteSize sed
      RecMsgMeta md -> byteSize md

instance Binary RecMsg where
  get = get @RecStatus >>= getRecMsgWithStatus
  put = void . putMsgRunning putRecMsgData Nothing

getRecMsgWithStatus :: RecStatus -> Get RecMsg
getRecMsgWithStatus = \case
  RecStatusChan cs@(ChanStatus chan _) -> fmap (RecMsgChan chan) (getChanData cs)
  RecStatusSysEx -> fmap RecMsgSysEx get
  RecStatusMeta -> fmap RecMsgMeta get

putRecMsgData :: RecMsg -> Put
putRecMsgData = \case
  RecMsgChan _ cd -> putChanData cd
  RecMsgSysEx sed -> put sed
  RecMsgMeta md -> put md

-- | NOTE: Time delta is in number of ticks since previous message
data Event = Event
  { evDelta :: !VarWord
  , evMsg :: !RecMsg
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arb) via (ArbGeneric Event)

-- private
type TrackMagic = ExactBytes "MTrk"

newtype Track = Track {unTrack :: Seq Event}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

instance Arb Track where
  arb = fmap Track (arbSeq 0 3)

-- private
byteSizeEventsLoop :: ByteCount -> Maybe ChanStatus -> Seq Event -> ByteCount
byteSizeEventsLoop !bc !mayLastStatus = \case
  Empty -> bc
  Event td msg :<| mes ->
    let !tc = byteSize td
        !mayNextStatus = statusAsChan (extractStatus msg)
        !mc = byteSize msg
        !sc = case mayNextStatus of
          Just _ | mayNextStatus == mayLastStatus -> mc - 1
          _ -> mc
    in  byteSizeEventsLoop (bc + tc + sc) mayNextStatus mes

-- private
byteSizeEvents :: Seq Event -> ByteCount
byteSizeEvents = byteSizeEventsLoop 0 Nothing

instance ByteSized Track where
  byteSize (Track events) = 8 + byteSizeEvents events

-- private
putEventsLoop :: Maybe ChanStatus -> Seq Event -> Put
putEventsLoop !mayLastStatus = \case
  Empty -> pure ()
  Event td msg :<| mes -> do
    put td
    mayNextStatus <- putMsgRunning putRecMsgData mayLastStatus msg
    putEventsLoop mayNextStatus mes

-- private
putEvents :: Seq Event -> Put
putEvents = putEventsLoop Nothing

getEventsScope :: ByteCount -> Get (Seq Event)
getEventsScope bc = getExact bc (go Empty Nothing)
 where
  go !acc !mayLastStatus = do
    sz <- getRemainingSize
    -- traceM $ "SIZE LEFT : " ++ show sz
    if sz == 0
      then pure acc
      else do
        td <- get
        msg <- getMsgRunning getRecMsgWithStatus mayLastStatus
        let !me = Event td msg
            !mayNextStatus = statusAsChan (extractStatus msg)
        go (acc :|> me) mayNextStatus

instance Binary Track where
  get = do
    _ <- get @TrackMagic
    chunkSize <- get @Word32BE
    fmap Track (getEventsScope (fromIntegral chunkSize))

  put t@(Track events) = do
    put @TrackMagic (ExactBytes ())
    put @Word32BE (fromIntegral (byteSize t) - 8)
    putEvents events

data MidFileType
  = MidFileTypeSingle
  | MidFileTypeMultiSync
  | MidFileTypeMultiAsync
  deriving stock (Eq, Ord, Enum, Bounded, Show)
  deriving (ByteSized, StaticByteSized, Binary) via (ViaBinaryRep MidFileType)
  deriving (Arb) via (ArbEnum MidFileType)

instance BinaryRep Word16BE MidFileType where
  fromBinaryRep = \case
    0 -> Right MidFileTypeSingle
    1 -> Right MidFileTypeMultiSync
    2 -> Right MidFileTypeMultiAsync
    other -> Left ("invalid midi file type: " ++ show other)
  toBinaryRep = \case
    MidFileTypeSingle -> 0
    MidFileTypeMultiSync -> 1
    MidFileTypeMultiAsync -> 2

-- private
type MidFileMagic = ExactBytes "MThd\NUL\NUL\NUL\ACK"

-- | NOTE: Ticks could also be SMTPE-related, but we don't support that here
data MidFile = MidFile
  { mfType :: !MidFileType
  , mfTicks :: !Word16
  , mfTracks :: !(Seq Track)
  }
  deriving stock (Eq, Ord, Show)

instance Arb MidFile where
  arb = MidFile <$> arb <*> arb <*> arbSeq 0 3

instance ByteSized MidFile where
  byteSize (MidFile _ _ tracks) = 14 + byteSizeFoldable tracks

instance Binary MidFile where
  get = do
    _ <- get @MidFileMagic
    ty <- get
    Word16BE numTracks <- get
    Word16BE ticks <- get
    -- traceM ("NUM TRACKS : " ++ show numTracks)
    tracks <- getSeq (fromIntegral numTracks) get
    pure (MidFile ty ticks tracks)
  put (MidFile ty ticks tracks) = do
    put @MidFileMagic (ExactBytes ())
    put ty
    put (Word16BE (fromIntegral (Seq.length tracks)))
    put (Word16BE ticks)
    putSeq put tracks

newtype SysExDump = SysExDump {unSysExDump :: Seq SysExData}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

instance Arb SysExDump where
  arb = fmap SysExDump (arbSeq 0 3)

instance ByteSized SysExDump where
  byteSize (SysExDump ds) = fromIntegral (Seq.length ds) + byteSizeFoldable ds

instance Binary SysExDump where
  get = fmap SysExDump $ getRemainingSeq $ do
    getExpect @Word8 "sysex status byte" get 0xF0
    get
  put = putSeq (\d -> put @Word8 0xF0 *> put d) . unSysExDump
