module Midiot.Osc where

import Control.Monad (replicateM_)
import Dahdit
import Data.ByteString.Short (ShortByteString)
import Data.Int (Int32, Int64)
import Data.Sequence (Seq)
import Data.Word (Word8)
import Midiot.Midi (ShortMsg)
import Midiot.Time (MonoTime)

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
  | DatumString !String
  | DatumBlob !ShortByteString
  | DatumTime !MonoTime
  | DatumMidi !PortMsg
  deriving stock (Eq, Ord, Show)

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

pad4 :: ByteCount -> ByteCount
pad4 x = x + (4 - rem x 4)

byteSizePad4 :: Binary a => a -> ByteCount
byteSizePad4 = pad4 . byteSize

staticByteSizePad4 :: StaticByteSized a => Proxy a -> ByteCount
staticByteSizePad4 = pad4 . staticByteSize

getPad4 :: Binary a => Get a
getPad4 = do
  x <- getRemainingSize
  a <- get
  y <- getRemainingSize
  replicateM_ (rem (unByteCount (y - x)) 4) (getExpect "pad" getWord8 0)
  pure a

putPad4 :: Binary a => a -> Put
putPad4 a = do
  let x = byteSize a
  put a
  replicateM_ (rem (unByteCount x) 4) (put @Word8 0)

newtype Pad4 a = Pad4 {unPad4 :: a}

instance StaticByteSized a => StaticByteSized (Pad4 a) where
  staticByteSize = staticByteSizePad4

instance Binary a => Binary (Pad4 a) where
  byteSize = byteSizePad4
  get = getPad4
  put = putPad4

newtype Sig = Sig {unSig :: Seq DatumType}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

-- Addr encoding: zero-terminated, aligned to 4-byte boundary
type AddrPat = ShortByteString

-- Encoding: addr encoding, type descriptor (same 0-term + aligned),
data Msg = Msg !AddrPat !(Seq Datum)
  deriving stock (Eq, Ord, Show)

data Bundle = Bundle !MonoTime !(Seq Msg)
  deriving stock (Eq, Ord, Show)

data Packet
  = PacketMsg !Msg
  | PacketBundle !Bundle
  deriving stock (Eq, Ord, Show)
