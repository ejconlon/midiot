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
  , Manf (..)
  , QuarterTimeUnit (..)
  , QuarterTime (..)
  , SysExString (..)
  , ChanStatus (..)
  , RtStatus (..)
  , CommonStatus (..)
  , Status (..)
  , ChanStatusType (..)
  , ChanVoiceData (..)
  , ChanModeData (..)
  , ChanData (..)
  , SysExData (..)
  , CommonData (..)
  , Msg (..)
  , msgStatus
  , msgNoteOn
  , msgNoteOff
  , Event (..)
  , Track (..)
  , FileType (..)
  , File (..)
  )
where

import Control.DeepSeq (NFData)
import Control.Monad (unless, void)
import Control.Newtype (Newtype)
import Dahdit (Binary (..), BinaryRep (..), ByteCount, ByteSized (..), ExactBytes (..), Get, Put, PutM, StaticByteSized (..), ViaBinaryRep (..), ViaGeneric (..), ViaStaticByteSized (..), Word16BE (..), Word32BE (..), byteSizeFoldable, getExact, getLookAhead, getRemainingSize, getSeq, putSeq)
import Data.Bits (Bits (..))
import Data.ByteString.Internal (c2w)
import Data.Hashable (Hashable)
import qualified Data.List.NonEmpty as NE
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.ShortWord (Word4)
import Data.String (IsString (..))
import Data.Word (Word16, Word8)
import GHC.Generics (Generic)
import Midiot.Arb (Arb (..), ArbEnum (..), ArbGeneric (..), arbSeq, genSum, genUnsigned)
import Midiot.Binary (BoundedBinary (..), MidiInt14 (..), MidiWord14 (..), MidiWord7 (..), VarWord (..))
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

newtype Manf = Manf {unManf :: MidiWord7}
  deriving stock (Show)
  deriving newtype
    (Eq, Ord, Enum, Num, Real, Integral, NFData, Hashable, ByteSized, StaticByteSized, Binary, Arb)

eduManf :: Manf
eduManf = Manf 0x7D

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

newtype SysExString = SysExString {unSysExString :: Seq MidiWord7}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

instance IsString SysExString where
  fromString = SysExString . Seq.fromList . fmap (fromIntegral . c2w)

instance ByteSized SysExString where
  byteSize = fromIntegral . Seq.length . unSysExString

instance Arb SysExString where
  arb = fmap SysExString (arbSeq 1 3)

-- private
getFlaggedBytes :: Get (Seq MidiWord7)
getFlaggedBytes = go Empty
 where
  go !acc = do
    w <- get @Word8
    let acc' = acc :|> fromIntegral (w .&. 0x7F)
    if w .&. 0x80 == 0
      then pure acc'
      else go acc'

-- private
putFlaggedBytes :: Seq MidiWord7 -> Put
putFlaggedBytes = \case
  Empty -> pure ()
  x :<| rest ->
    case rest of
      Empty -> put x
      _ -> put @Word8 (0x80 .|. fromIntegral x) *> putFlaggedBytes rest

instance Binary SysExString where
  get = fmap SysExString getFlaggedBytes
  put = putFlaggedBytes . unSysExString

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
  | CommonStatusEndExclusive
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

data Status
  = StatusChan !ChanStatus
  | StatusSysEx
  | StatusSysCommon !CommonStatus
  | StatusSysRt !RtStatus
  deriving stock (Eq, Ord, Show, Generic)
  deriving (ByteSized) via (ViaStaticByteSized Status)
  deriving (Arb) via (ArbGeneric Status)

instance StaticByteSized Status where
  staticByteSize _ = 1

-- private
statusIsChan :: Status -> Bool
statusIsChan = \case
  StatusChan _ -> True
  _ -> False

-- private
statusAsChan :: Status -> Maybe ChanStatus
statusAsChan = \case
  StatusChan cs -> Just cs
  _ -> Nothing

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

instance Binary Status where
  get = do
    b <- get @Word8
    let x = b .&. 0xF0
    if
        | x < 0x80 -> fail ("Status byte with high bit clear: " ++ show b)
        | x == 0xF0 ->
            case b of
              0xF0 -> pure StatusSysEx
              0xF1 -> pure (StatusSysCommon CommonStatusTimeFrame)
              0xF2 -> pure (StatusSysCommon CommonStatusSongPointer)
              0xF3 -> pure (StatusSysCommon CommonStatusSongSelect)
              0xF6 -> pure (StatusSysCommon CommonStatusTuneRequest)
              0xF7 -> pure (StatusSysCommon CommonStatusEndExclusive)
              0xF8 -> pure (StatusSysRt RtStatusTimingClock)
              0xFA -> pure (StatusSysRt RtStatusStart)
              0xFB -> pure (StatusSysRt RtStatusContinue)
              0xFC -> pure (StatusSysRt RtStatusStop)
              0xFE -> pure (StatusSysRt RtStatusActiveSensing)
              0xFF -> pure (StatusSysRt RtStatusSystemReset)
              _ -> fail ("Unknown system status byte: " ++ show b)
        | otherwise -> do
            let c = Channel (fromIntegral (b .&. 0x0F))
            pure $ StatusChan $ ChanStatus c $ case x of
              0x80 -> ChanStatusNoteOff
              0x90 -> ChanStatusNoteOn
              0xA0 -> ChanStatusKeyAftertouch
              0xB0 -> ChanStatusControlChange
              0xC0 -> ChanStatusProgramChange
              0xD0 -> ChanStatusChanAftertouch
              0xE0 -> ChanStatusPitchBend
              _ -> error "impossible"
  put = \case
    StatusChan (ChanStatus c cs) ->
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
    StatusSysEx -> put @Word8 0xF0
    StatusSysCommon cs ->
      let x = case cs of
            CommonStatusTimeFrame -> 0x01
            CommonStatusSongPointer -> 0x02
            CommonStatusSongSelect -> 0x03
            CommonStatusTuneRequest -> 0x06
            CommonStatusEndExclusive -> 0x07
      in  put @Word8 (0xF0 .|. x)
    StatusSysRt rs ->
      let !x = case rs of
            RtStatusTimingClock -> 0x00
            RtStatusStart -> 0x02
            RtStatusContinue -> 0x03
            RtStatusStop -> 0x04
            RtStatusActiveSensing -> 0x06
            RtStatusSystemReset -> 0x7
      in  put @Word8 (0xF8 .|. x)

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

data SysExData = SysExData
  { sedManf :: !Manf
  , sedBody :: !SysExString
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (ByteSized, Binary) via (ViaGeneric SysExData)
  deriving (Arb) via (ArbGeneric SysExData)

data CommonData
  = CommonDataTimeFrame !QuarterTime
  | CommonDataSongPointer !Position
  | CommonDataSongSelect !Song
  | CommonDataTuneRequest
  | CommonDataEndExclusive
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arb) via (ArbGeneric CommonData)

instance ByteSized CommonData where
  byteSize = \case
    CommonDataTimeFrame _ -> 1
    CommonDataSongPointer _ -> 2
    CommonDataSongSelect _ -> 1
    CommonDataTuneRequest -> 0
    CommonDataEndExclusive -> 0

getCommonData :: CommonStatus -> Get CommonData
getCommonData = \case
  CommonStatusTimeFrame -> fmap CommonDataTimeFrame get
  CommonStatusSongPointer -> fmap CommonDataSongPointer get
  CommonStatusSongSelect -> fmap CommonDataSongSelect get
  CommonStatusTuneRequest -> pure CommonDataTuneRequest
  CommonStatusEndExclusive -> pure CommonDataEndExclusive

putCommonData :: CommonData -> Put
putCommonData = \case
  CommonDataTimeFrame qt -> put qt
  CommonDataSongPointer po -> put po
  CommonDataSongSelect so -> put so
  CommonDataTuneRequest -> pure ()
  CommonDataEndExclusive -> pure ()

data Msg
  = MsgChanVoice !Channel !ChanVoiceData
  | MsgChanMode !Channel !ChanModeData
  | MsgSysEx !SysExData
  | MsgSysCommon !CommonData
  | MsgSysRt !RtStatus
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arb) via (ArbGeneric Msg)

instance ByteSized Msg where
  byteSize msg =
    1
      + case msg of
        MsgChanVoice _ cvd -> byteSize cvd
        MsgChanMode _ cmd -> byteSize cmd
        MsgSysEx sed -> byteSize sed
        MsgSysCommon cd -> byteSize cd
        MsgSysRt _ -> 0

instance Binary Msg where
  get = get @Status >>= getMsgWithStatus
  put = void . putMsgRunning Nothing

-- private
getMsgWithStatus :: Status -> Get Msg
getMsgWithStatus = \case
  StatusChan cs@(ChanStatus chan _) -> do
    flip fmap (getChanData cs) $ \case
      ChanDataVoice cvd -> MsgChanVoice chan cvd
      ChanDataMode cmd -> MsgChanMode chan cmd
  StatusSysEx -> fmap MsgSysEx get
  StatusSysCommon cs -> fmap MsgSysCommon (getCommonData cs)
  StatusSysRt rs -> pure (MsgSysRt rs)

-- private
-- Running status is for Voice and Mode messages only!
getMsgRunning :: Maybe ChanStatus -> Get Msg
getMsgRunning mayLastStatus = do
  peeked <- peekStatus
  case peeked of
    StatusPeekYes -> do
      status <- get @Status
      getMsgWithStatus status
    StatusPeekNo dat ->
      case mayLastStatus of
        Nothing -> fail ("Expected status byte (no running status): " ++ show dat)
        Just lastStatus -> getMsgWithStatus (StatusChan lastStatus)

-- private
putMsgRunning :: Maybe ChanStatus -> Msg -> PutM (Maybe ChanStatus)
putMsgRunning mayLastStatus msg = do
  mayCurStatus <- putMsgStatusRunning mayLastStatus msg
  putMsgData msg
  pure mayCurStatus

-- private
putMsgStatusRunning :: Maybe ChanStatus -> Msg -> PutM (Maybe ChanStatus)
putMsgStatusRunning mayLastStatus msg =
  let curStatus = msgStatus msg
  in  case mayLastStatus of
        Nothing -> do
          put curStatus
          pure $ case curStatus of
            StatusChan chanStatus -> Just chanStatus
            _ -> Nothing
        Just lastStatus ->
          case curStatus of
            StatusChan chanStatus ->
              if chanStatus == lastStatus
                then pure mayLastStatus
                else Just chanStatus <$ put curStatus
            _ -> Nothing <$ put curStatus

-- private
putMsgData :: Msg -> Put
putMsgData = \case
  MsgChanVoice _ cvd -> putChanVoiceData cvd
  MsgChanMode _ cmd -> putChanModeData cmd
  MsgSysEx sed -> put sed
  MsgSysCommon cd -> putCommonData cd
  MsgSysRt _ -> pure ()

msgStatus :: Msg -> Status
msgStatus = \case
  MsgChanVoice chan cvd -> StatusChan $ ChanStatus chan $ case cvd of
    ChanVoiceDataNoteOff _ _ -> ChanStatusNoteOff
    ChanVoiceDataNoteOn _ _ -> ChanStatusNoteOn
    ChanVoiceKeyAftertouch _ _ -> ChanStatusKeyAftertouch
    ChanVoiceControlChange _ _ -> ChanStatusControlChange
    ChanVoiceProgramChange _ -> ChanStatusProgramChange
    ChanVoiceChanAftertouch _ -> ChanStatusChanAftertouch
    ChanVoicePitchBend _ -> ChanStatusPitchBend
  MsgChanMode chan _ -> StatusChan (ChanStatus chan ChanStatusControlChange)
  MsgSysEx _ -> StatusSysEx
  MsgSysCommon cd -> StatusSysCommon $ case cd of
    CommonDataTimeFrame _ -> CommonStatusTimeFrame
    CommonDataSongPointer _ -> CommonStatusSongPointer
    CommonDataSongSelect _ -> CommonStatusSongSelect
    CommonDataTuneRequest -> CommonStatusTuneRequest
    CommonDataEndExclusive -> CommonStatusEndExclusive
  MsgSysRt rs -> StatusSysRt rs

msgNoteOn :: Channel -> Note -> Velocity -> Msg
msgNoteOn c k v = MsgChanVoice c (ChanVoiceDataNoteOn k v)

msgNoteOff :: Channel -> Note -> Msg
msgNoteOff c k = msgNoteOn c k 0

-- | NOTE: Time delta is in number of ticks since previous message
data Event = Event
  { evDelta :: !VarWord
  , evMsg :: !Msg
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
        !mayNextStatus = statusAsChan (msgStatus msg)
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
getEventsLoop :: Int -> Seq Event -> Maybe ChanStatus -> Get (Seq Event)
getEventsLoop !numLeft !acc !mayLastStatus =
  if numLeft <= 0
    then pure acc
    else do
      td <- get
      msg <- getMsgRunning mayLastStatus
      let !me = Event td msg
          !mayNextStatus = statusAsChan (msgStatus msg)
      getEventsLoop (numLeft - 1) (acc :|> me) mayNextStatus

-- private
getEvents :: Int -> Get (Seq Event)
getEvents numLeft = getEventsLoop numLeft Empty Nothing

-- private
putEventsLoop :: Maybe ChanStatus -> Seq Event -> Put
putEventsLoop !mayLastStatus = \case
  Empty -> pure ()
  Event td msg :<| mes -> do
    put td
    mayNextStatus <- putMsgRunning mayLastStatus msg
    putEventsLoop mayNextStatus mes

-- private
putEvents :: Seq Event -> Put
putEvents = putEventsLoop Nothing

getEventsScope :: ByteCount -> Get (Seq Event)
getEventsScope bc = getExact bc (go Empty Nothing)
 where
  go !acc !mayLastStatus = do
    sz <- getRemainingSize
    if sz == 0
      then pure acc
      else do
        td <- get
        msg <- getMsgRunning mayLastStatus
        let !me = Event td msg
            !mayNextStatus = statusAsChan (msgStatus msg)
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

data FileType
  = FileTypeSingle
  | FileTypeMultiSync
  | FileTypeMultiAsync
  deriving stock (Eq, Ord, Enum, Bounded, Show)
  deriving (ByteSized, StaticByteSized, Binary) via (ViaBinaryRep FileType)
  deriving (Arb) via (ArbEnum FileType)

instance BinaryRep Word16BE FileType where
  fromBinaryRep = \case
    0 -> Right FileTypeSingle
    1 -> Right FileTypeMultiSync
    2 -> Right FileTypeMultiAsync
    other -> Left ("invalid midi file type: " ++ show other)
  toBinaryRep = \case
    FileTypeSingle -> 0
    FileTypeMultiSync -> 1
    FileTypeMultiAsync -> 2

-- private
-- TODO this is wrong
type FileMagic = ExactBytes "MThd\NUL\NUL\NUL\ACK"

-- | NOTE: Ticks could also be SMTPE-related, but we don't support that here
data File = File
  { fileType :: !FileType
  , fileTicks :: !Word16
  , fileTracks :: !(Seq Track)
  }
  deriving stock (Eq, Ord, Show)

instance Arb File where
  arb = File <$> arb <*> arb <*> arbSeq 0 3

instance ByteSized File where
  byteSize (File _ _ tracks) = 14 + byteSizeFoldable tracks

instance Binary File where
  get = do
    _ <- get @FileMagic
    ty <- get
    Word16BE ticks <- get
    Word16BE numTracks <- get
    tracks <- getSeq (fromIntegral numTracks) get
    pure (File ty ticks tracks)
  put (File ty ticks tracks) = do
    put @FileMagic (ExactBytes ())
    put ty
    put (Word16BE ticks)
    put (Word16BE (fromIntegral (Seq.length tracks)))
    putSeq put tracks
