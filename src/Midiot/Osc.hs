{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Midiot.Osc where

import Control.Exception (Exception)
import Control.Monad (replicateM_)
import Dahdit (getExpect, getLookAhead, getRemainingSize, ByteCount (..), StaticByteSized (..), Binary (..), Get, Put, Int32BE (..), Int64BE (..), TermBytes8 (..), FloatBE (..), DoubleBE (..), putByteString)
import Data.Proxy (Proxy (..))
import Data.ByteString.Short (ShortByteString)
import Data.Int (Int32, Int64)
import Data.Sequence (Seq (..))
import Data.Word (Word8)
import GHC.TypeLits (KnownNat, type Mod, type (+), type (-))
import Midiot.Midi (ShortMsg)
import Midiot.Time (MonoTime)
import qualified Data.Sequence as Seq
import Data.ByteString.Internal (c2w, w2c)
import Data.Foldable (for_, toList, foldMap')
import Data.Text.Short (ShortText)
import qualified Data.Text.Short as TS
import qualified Data.Text.Short.Unsafe as TSU
import Data.Monoid (Sum (..))

newtype Pad4 a = Pad4 {unPad4 :: a}

instance (StaticByteSized a, KnownNat (StaticSize (Pad4 a))) => StaticByteSized (Pad4 a) where
  type StaticSize (Pad4 a) = StaticSize a + (4 - Mod (StaticSize a) 4)
  staticByteSize = staticByteSizePad4 staticByteSize

instance Binary a => Binary (Pad4 a) where
  byteSize (Pad4 a) = byteSizePad4 byteSize a
  get = fmap Pad4 (getPad4 get)
  put (Pad4 a) = putPad4 byteSize put a

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
  | DatumTime !MonoTime
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

slashByte :: Word8
slashByte = c2w '/'

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
  get = getPad4 (getExpect "comma" get commaByte *> fmap Sig (go Empty)) where
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

newtype Addr = Addr { unAddr :: Seq ShortText }
  deriving stock (Show)
  deriving newtype (Eq, Ord)

addrSizer :: Addr -> ByteCount
addrSizer (Addr parts) =
  ByteCount (Seq.length parts + getSum (foldMap' (Sum . TS.length) parts))

instance Binary Addr where
  byteSize = byteSizePad4 addrSizer
  get = getPad4 $ do
    addr <- error "TODO"
    getExpect "null" (get @Word8) 0
    pure addr
  put = putPad4 addrSizer $ \(Addr parts) -> do
    for_ parts $ \part -> do
      put slashByte
      putByteString (TS.toShortByteString part)
    put @Word8 0

isInvalidAddrPartChar :: Char -> Bool
isInvalidAddrPartChar c =
  c == ' ' ||
    c == '#' ||
    c == '*' ||
    c == ',' ||
    c == '/' ||
    c == '?' ||
    c == '[' ||
    c == ']' ||
    c == '{' ||
    c == '}'

data AddrErr = AddrErrPartEmpty | AddrErrInvalidPartChar !Char | AddrErrExpectSlash !Char
  deriving stock (Eq, Ord, Show)

instance Exception AddrErr

parseAddr :: ShortText -> Either AddrErr Addr
parseAddr = goStart . TS.unpack where
  goStart = \case
    [] -> Right (Addr Empty)
    c:cs ->
      if c == '/'
        then goRest Empty Empty cs
        else Left (AddrErrExpectSlash c)
  pack = TS.pack . toList
  goRest !acc !pacc = \case
    [] ->
      if Seq.null pacc
        then Left AddrErrPartEmpty
        else Right (Addr (acc :|> pack pacc))
    c:cs ->
      if c == '/'
        then
          if Seq.null pacc
            then Left AddrErrPartEmpty
            else goRest (acc :|> pack pacc) Empty cs
        else
          if isInvalidAddrPartChar c
            then Left (AddrErrInvalidPartChar c)
            else goRest acc (pacc :|> c) cs

printAddr :: Addr -> ShortText
printAddr (Addr xs) =
  if Seq.null xs
    then TS.empty
    else TS.cons '/' (TS.intercalate (TS.singleton '/') (toList xs))

data PatFrag =
    PatFragText !ShortText
  | PatFragAnyMany
  | PatFragAnyOne
  | PatFragChoose !(Seq ShortText)
  | PatFragRange !Bool !ShortText !ShortText
  deriving stock (Eq, Ord, Show)

type PatPart = Seq PatFrag

-- Addr encoding: zero-terminated, aligned to 4-byte boundary
newtype AddrPat = AddrPat { unAddrPat :: Seq PatPart }
  deriving stock (Show)
  deriving newtype (Eq, Ord)

matchPart :: PatPart -> ShortText -> Bool
matchPart = error "TODO"

matchAddr :: AddrPat -> Addr -> Bool
matchAddr (AddrPat patParts) (Addr parts) =
  (Seq.length patParts == Seq.length parts) &&
    and (zipWith matchPart (toList patParts) (toList parts))

-- Encoding: addr encoding, type descriptor (same 0-term + aligned),
data Msg = Msg !AddrPat !(Seq Datum)
  deriving stock (Eq, Ord, Show)

data Bundle = Bundle !MonoTime !(Seq Msg)
  deriving stock (Eq, Ord, Show)

data Packet
  = PacketMsg !Msg
  | PacketBundle !Bundle
  deriving stock (Eq, Ord, Show)
