{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Midiot.Osc where

import Control.Monad (replicateM_)
import Dahdit (Binary (..), ByteCount (..), DoubleBE (..), FloatBE (..), Get, Int32BE (..), Int64BE (..), Put, StaticByteSized (..), TermBytes8 (..), Word64BE (..), byteSizeFoldable, byteSizeViaStatic, getExact, getExpect, getLookAhead, getRemainingSeq, getRemainingSize)
import Data.ByteString.Internal (c2w, w2c)
import Data.ByteString.Short (ShortByteString)
import Data.Foldable (foldMap', for_)
import Data.Int (Int32, Int64)
import Data.Monoid (Sum (..))
import Data.Proxy (Proxy (..))
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Text.Short (ShortText)
import qualified Data.Text.Short as TS
import qualified Data.Text.Short.Unsafe as TSU
import Data.Word (Word32, Word64, Word8)
import GHC.Generics (Generic)
import Midiot.Arb (Arb (..), ArbEnum (..), ArbGeneric (..), I, genFractional, genSBS, genSeq, genSigned, genUnsigned)
import Midiot.Midi (ShortMsg)
import Midiot.OscAddr (RawAddrPat (..))
import Midiot.Pad (byteSizePad32, getPad32, pad32, putPad32)
import Midiot.Time (NtpTime (..))
import qualified Test.Falsify.Generator as FG

data DatumType
  = DatumTypeInt32
  | DatumTypeInt64
  | DatumTypeFloat
  | DatumTypeDouble
  | DatumTypeString
  | DatumTypeBlob
  | DatumTypeTime
  | DatumTypeMidi
  deriving stock (Eq, Ord, Show, Enum, Bounded)
  deriving (Arb I) via (ArbEnum DatumType)

datumTypeRep :: DatumType -> Char
datumTypeRep = \case
  DatumTypeInt32 -> 'i'
  DatumTypeInt64 -> 'h'
  DatumTypeFloat -> 'f'
  DatumTypeDouble -> 'd'
  DatumTypeString -> 's'
  DatumTypeBlob -> 'b'
  DatumTypeTime -> 't'
  DatumTypeMidi -> 'm'

datumTypeUnRep :: Char -> Maybe DatumType
datumTypeUnRep = \case
  'i' -> Just DatumTypeInt32
  'h' -> Just DatumTypeInt64
  'f' -> Just DatumTypeFloat
  'd' -> Just DatumTypeDouble
  's' -> Just DatumTypeString
  'b' -> Just DatumTypeBlob
  't' -> Just DatumTypeTime
  'm' -> Just DatumTypeMidi
  _ -> Nothing

newtype Port = Port {unPort :: Word8}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Binary, StaticByteSized, Arb I)

data PortMsg = PortMsg !Port !ShortMsg
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arb I) via (ArbGeneric I PortMsg)

instance StaticByteSized PortMsg where
  type StaticSize PortMsg = 4
  staticByteSize _ = 4

instance Binary PortMsg where
  byteSize = byteSizeViaStatic
  get = do
    p <- get
    m <- get
    replicateM_ (3 - unByteCount (byteSize m)) (getExpect "port msg pad" (get @Word8) 0)
    pure (PortMsg p m)
  put (PortMsg p m) = do
    put p
    put m
    replicateM_ (3 - unByteCount (byteSize m)) (put @Word8 0)

-- In OSC Time is NTP64 https://atolab.github.io/uhlc-rs/uhlc/struct.NTP64.html
-- In SC it is seconds since start

data Datum
  = DatumInt32 !Int32
  | DatumInt64 !Int64
  | DatumFloat !Float
  | DatumDouble !Double
  | DatumString !ShortText
  | DatumBlob !ShortByteString
  | DatumTime !NtpTime
  | DatumMidi !PortMsg
  deriving stock (Eq, Ord, Show)

instance Arb I Datum where
  arb p _ =
    foldr1
      FG.choose
      [ DatumInt32 <$> genSigned
      , DatumInt64 <$> genSigned
      , DatumFloat <$> genFractional
      , DatumDouble <$> genFractional
      , DatumString . TSU.fromShortByteStringUnsafe <$> genSBS 0 3
      , DatumBlob <$> genSBS 0 3
      , DatumTime . NtpTime <$> genUnsigned
      , DatumMidi <$> arb p Proxy
      ]

datumSizer :: Datum -> ByteCount
datumSizer = \case
  DatumInt32 _ -> 4
  DatumInt64 _ -> 8
  DatumFloat _ -> 4
  DatumDouble _ -> 8
  DatumString x -> byteSize (TermBytes8 (TS.toShortByteString x))
  DatumBlob x -> byteSize (TermBytes8 x)
  DatumTime _ -> 4
  DatumMidi _ -> 4

datumGetter :: DatumType -> Get Datum
datumGetter = \case
  DatumTypeInt32 -> DatumInt32 . unInt32BE <$> get
  DatumTypeInt64 -> DatumInt64 . unInt64BE <$> get
  DatumTypeFloat -> DatumFloat . unFloatBE <$> get
  DatumTypeDouble -> DatumDouble . unDoubleBE <$> get
  DatumTypeString -> DatumString . TSU.fromShortByteStringUnsafe . unTermBytes8 <$> get
  DatumTypeBlob -> DatumBlob . unTermBytes8 <$> get
  DatumTypeTime -> DatumTime . NtpTime . unWord64BE <$> get
  DatumTypeMidi -> DatumMidi <$> get

datumPutter :: Datum -> Put
datumPutter = \case
  DatumInt32 x -> put (Int32BE x)
  DatumInt64 x -> put (Int64BE x)
  DatumFloat x -> put (FloatBE x)
  DatumDouble x -> put (DoubleBE x)
  DatumString x -> put (TermBytes8 (TS.toShortByteString x))
  DatumBlob x -> put (TermBytes8 x)
  DatumTime x -> put (Word64BE (unNtpTime x))
  DatumMidi x -> put x

datumType :: Datum -> DatumType
datumType = \case
  DatumInt32 _ -> DatumTypeInt32
  DatumInt64 _ -> DatumTypeInt64
  DatumFloat _ -> DatumTypeFloat
  DatumDouble _ -> DatumTypeDouble
  DatumString _ -> DatumTypeString
  DatumBlob _ -> DatumTypeBlob
  DatumTime _ -> DatumTypeTime
  DatumMidi _ -> DatumTypeMidi

newtype Sig = Sig {unSig :: Seq DatumType}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

instance Arb I Sig where
  arb p _ = fmap Sig (genSeq 0 3 (arb p Proxy))

commaByte :: Word8
commaByte = c2w ','

hashByte :: Word8
hashByte = c2w '#'

getNextNonPad :: Get (Maybe Word8)
getNextNonPad = do
  sz <- getRemainingSize
  if sz == 0
    then pure Nothing
    else do
      w <- getLookAhead (get @Word8)
      if w == 0
        then pure Nothing
        else fmap Just (get @Word8)

sigSizer :: Sig -> ByteCount
sigSizer (Sig dts) = ByteCount (1 + Seq.length dts)

instance Binary Sig where
  byteSize = byteSizePad32 sigSizer
  get = getPad32 (getExpect "comma" get commaByte *> fmap Sig (go Empty))
   where
    go !acc = do
      mnext <- getNextNonPad
      case mnext of
        Just w -> do
          case datumTypeUnRep (w2c w) of
            Nothing -> fail ("Unknown data type rep: " ++ show w)
            Just dt -> go (acc :|> dt)
        Nothing -> pure acc
  put = putPad32 sigSizer $ \(Sig dts) -> do
    put commaByte
    for_ dts (put . c2w . datumTypeRep)

data Msg = Msg !RawAddrPat !(Seq Datum)
  deriving stock (Eq, Ord, Show)

instance Arb I Msg where
  arb p _ = Msg <$> arb p Proxy <*> genSeq 0 3 (arb p Proxy)

instance Binary Msg where
  byteSize (Msg r ds) =
    byteSize r
      + pad32 (ByteCount (1 + Seq.length ds))
      + getSum (foldMap' (Sum . pad32 . datumSizer) ds)
  get = do
    r <- get @RawAddrPat
    s <- get @Sig
    ds <- traverse datumGetter (unSig s)
    pure (Msg r ds)
  put (Msg r ds) = do
    put r
    put (Sig (fmap datumType ds))
    for_ ds datumPutter

data Bundle = Bundle !NtpTime !(Seq Packet)
  deriving stock (Eq, Ord, Show)

instance Arb I Bundle where
  arb p _ = Bundle <$> (NtpTime <$> genUnsigned) <*> genSeq 0 3 (arb p Proxy)

bundleTag :: TermBytes8
bundleTag = TermBytes8 "#bundle"

instance Binary Bundle where
  byteSize (Bundle _ packs) = 12 + ByteCount (4 * Seq.length packs) + byteSizeFoldable packs
  get = do
    getExpect "bundle tag" (get @TermBytes8) bundleTag
    t <- fmap NtpTime (get @Word64)
    packs <- getRemainingSeq $ do
      sz <- fmap (ByteCount . fromIntegral) (get @Word32)
      getExact sz get
    pure (Bundle t packs)
  put (Bundle (NtpTime k) packs) = do
    put bundleTag
    put k
    for_ packs $ \pack -> do
      put @Word32 (fromIntegral (unByteCount (byteSize pack)))
      put pack

data Packet
  = PacketMsg !Msg
  | PacketBundle !Bundle
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arb I) via (ArbGeneric I Packet)

instance Binary Packet where
  byteSize = \case
    PacketMsg msg -> byteSize msg
    PacketBundle bun -> byteSize bun
  get = do
    w <- getLookAhead (get @Word8)
    if w == hashByte
      then fmap PacketBundle get
      else fmap PacketMsg get
  put = \case
    PacketMsg msg -> put msg
    PacketBundle bun -> put bun

immediately :: NtpTime
immediately = NtpTime 1
