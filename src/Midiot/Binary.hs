{-# LANGUAGE TemplateHaskell #-}

module Midiot.Binary
  ( BoundedBinary (..)
  , MidiWord7 (..)
  , MidiInt7 (..)
  , MidiWord14 (..)
  , MidiInt14 (..)
  , VarWord (..)
  , expandW14
  , contractW14
  )
where

import Control.DeepSeq (NFData)
import Control.Monad (unless)
import Dahdit (Binary (..), ByteSized (..), StaticByteSized (..), Word16LE (..))
import Data.Bits (Bits (..))
import Data.Hashable (Hashable)
import Data.Proxy (Proxy (..))
import Data.ShortWord (Int7, Word7)
import Data.ShortWord.TH (mkShortWord)
import Data.Word (Word16, Word32, Word8)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Midiot.Arb (Arb (..), ArbSigned (..), ArbUnsigned (..))

newtype BoundedBinary (s :: Symbol) a = BoundedBinary {unBoundedBinary :: a}

instance (KnownSymbol s, Bounded a, Binary a, Ord a, Show a) => Binary (BoundedBinary s a) where
  get = do
    v <- get
    if v < minBound || v > maxBound
      then fail (symbolVal (Proxy :: Proxy s) ++ " value out of bounds: " ++ show v)
      else pure (BoundedBinary v)
  put = put . unBoundedBinary

newtype MidiWord7 = MidiWord7 {unMidiWord7 :: Word7}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Bounded, Num, Real, Integral, NFData, Hashable)
  deriving (Arb) via (ArbUnsigned Word7)

instance ByteSized MidiWord7 where
  byteSize _ = 1

instance StaticByteSized MidiWord7 where
  staticByteSize _ = 1

instance Binary MidiWord7 where
  get = do
    w <- get @Word8
    if w .&. 0x80 == 0
      then pure (MidiWord7 (fromIntegral w))
      else fail ("Word7 high bit set: " ++ show w)
  put = put @Word8 . fromIntegral . unMidiWord7

newtype MidiInt7 = MidiInt7 {unMidiInt7 :: Int7}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Bounded, Num, Real, Integral, NFData, Hashable)
  deriving (Arb) via (ArbSigned Int7)

instance ByteSized MidiInt7 where
  byteSize _ = 1

instance StaticByteSized MidiInt7 where
  staticByteSize _ = 1

instance Binary MidiInt7 where
  get = do
    w <- get @Word8
    if w .&. 0x80 == 0
      then pure (MidiInt7 (fromIntegral w))
      else fail ("Int7 high bit set: " ++ show w)
  put = put @Word8 . fromIntegral . unMidiInt7

mkShortWord "Word14" "Word14" "aWord14" "Int14" "Int14" "anInt14" ''Word16 14 []

expandW14 :: Word14 -> Word16
expandW14 w =
  let x = fromIntegral w :: Word16
      xLo = x .&. 0x007F
      xHi = shiftL x 1 .&. 0x7F00
  in  xHi .|. xLo

contractW14 :: Word16 -> Word14
contractW14 v =
  let vLo = v .&. 0x007F
      vHi = v .&. 0x7F00
      x = shiftR vHi 1 .|. vLo
  in  fromIntegral x

newtype MidiWord14 = MidiWord14 {unMidiWord14 :: Word14}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Bounded, Num, Real, Integral, NFData, Hashable)
  deriving (Arb) via (ArbUnsigned Word14)

instance ByteSized MidiWord14 where
  byteSize _ = 2

instance StaticByteSized MidiWord14 where
  staticByteSize _ = 2

instance Binary MidiWord14 where
  get = fmap (MidiWord14 . contractW14 . unWord16LE) get
  put = put . Word16LE . expandW14 . unMidiWord14

newtype MidiInt14 = MidiInt14 {unMidiInt14 :: Int14}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Bounded, Num, Real, Integral, NFData, Hashable)
  deriving (Arb) via (ArbSigned Int14)

instance ByteSized MidiInt14 where
  byteSize _ = 2

instance StaticByteSized MidiInt14 where
  staticByteSize _ = 2

instance Binary MidiInt14 where
  get = fmap (MidiInt14 . fromIntegral . contractW14 . unWord16LE) get
  put = put . Word16LE . expandW14 . fromIntegral . unMidiInt14

newtype VarWord = VarWord {unVarInt :: Word32}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Bounded, Num, Integral, Real, NFData, Hashable)
  deriving (Arb) via (ArbUnsigned Word32)

instance ByteSized VarWord where
  byteSize (VarWord w) =
    if
        | w .&. 0xFFFFFF80 == 0 -> 1
        | w .&. 0xFFFFC000 == 0 -> 2
        | w .&. 0xFFE00000 == 0 -> 3
        | otherwise -> 4

instance Binary VarWord where
  get = go 0 0
   where
    go !off !acc = do
      w <- get @Word8
      let !wLow = fromIntegral (w .&. 0x7F)
          !wShift = shiftL wLow off
          !accNext = acc .|. wShift
      if w .&. 0x80 == 0
        then pure $! VarWord accNext
        else go (off + 7) accNext

  put (VarWord acc) = go acc
   where
    go !w = do
      let !wLow = fromIntegral (w .&. 0x7F)
          !wShift = shiftR w 7
      put @Word8 wLow
      unless (wShift == 0) (go wShift)
