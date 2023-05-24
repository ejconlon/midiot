{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Midiot.Osc where

import Control.Monad (replicateM_)
import Dahdit (Binary (..), ByteCount (..), DoubleBE (..), FloatBE (..), Get, Int32BE (..), Int64BE (..), Put, TermBytes8 (..), byteSizeFoldable, getExact, getExpect, getLookAhead, getRemainingSeq, getRemainingSize)
import Data.ByteString.Internal (c2w, w2c)
import Data.ByteString.Short (ShortByteString)
import Data.Foldable (foldMap', for_)
import Data.Int (Int32, Int64)
import Data.Monoid (Sum (..))
import Data.Proxy (Proxy (..))
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.String (IsString (..))
import Data.Text.Short (ShortText)
import qualified Data.Text.Short as TS
import qualified Data.Text.Short.Unsafe as TSU
import Data.Word (Word32, Word64, Word8)
import Midiot.Midi (ShortMsg)
import Midiot.Time (NtpTime (..))

pad4 :: ByteCount -> ByteCount
pad4 x = x + (4 - rem x 4)

staticByteSizePad4 :: (Proxy a -> ByteCount) -> Proxy a -> ByteCount
staticByteSizePad4 staticSizer p = pad4 (staticSizer p)

byteSizePad4 :: (a -> ByteCount) -> a -> ByteCount
byteSizePad4 sizer a = pad4 (sizer a)

getPad4 :: Get a -> Get a
getPad4 getter = do
  x <- getRemainingSize
  a <- getter
  y <- getRemainingSize
  replicateM_ (rem (unByteCount (y - x)) 4) (getExpect "pad" (get @Word8) 0)
  pure a

putPad4 :: (a -> ByteCount) -> (a -> Put) -> a -> Put
putPad4 sizer putter a = do
  let x = sizer a
  putter a
  replicateM_ (4 - rem (unByteCount x) 4) (put @Word8 0)

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

type Port = Word8

data PortMsg = PortMsg !Port !ShortMsg
  deriving stock (Eq, Ord, Show)

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
  DatumTypeTime -> error "TODO"
  DatumTypeMidi -> error "TODO"

datumPutter :: Datum -> Put
datumPutter = \case
  DatumInt32 x -> put (Int32BE x)
  DatumInt64 x -> put (Int64BE x)
  DatumFloat x -> put (FloatBE x)
  DatumDouble x -> put (DoubleBE x)
  DatumString x -> put (TermBytes8 (TS.toShortByteString x))
  DatumBlob x -> put (TermBytes8 x)
  DatumTime _ -> error "TODO"
  DatumMidi _ -> error "TODO"

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
  byteSize = byteSizePad4 sigSizer
  get = getPad4 (getExpect "comma" get commaByte *> fmap Sig (go Empty))
   where
    go !acc = do
      mnext <- getNextNonPad
      case mnext of
        Just w -> do
          case datumTypeUnRep (w2c w) of
            Nothing -> fail ("Unknown data type rep: " ++ show w)
            Just dt -> go (acc :|> dt)
        Nothing -> pure acc
  put = putPad4 sigSizer $ \(Sig dts) -> do
    put commaByte
    for_ dts (put . c2w . datumTypeRep)

newtype RawAddrPat = RawAddrPat {unRawAddrPat :: ShortText}
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

rawAddrPatSizer :: RawAddrPat -> ByteCount
rawAddrPatSizer = ByteCount . succ . TS.length . unRawAddrPat

instance Binary RawAddrPat where
  byteSize = byteSizePad4 rawAddrPatSizer
  get =
    getPad4 $
      fmap (RawAddrPat . TSU.fromShortByteStringUnsafe . unTermBytes8) (get @TermBytes8)
  put = putPad4 rawAddrPatSizer (put . TermBytes8 . TS.toShortByteString . unRawAddrPat)

data Msg = Msg !RawAddrPat !(Seq Datum)
  deriving stock (Eq, Ord, Show)

instance Binary Msg where
  byteSize (Msg r ds) =
    byteSize r
      + pad4 (ByteCount (1 + Seq.length ds))
      + getSum (foldMap' (Sum . pad4 . datumSizer) ds)
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
  deriving stock (Eq, Ord, Show)

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
