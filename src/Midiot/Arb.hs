{-# LANGUAGE UndecidableInstances #-}

module Midiot.Arb
  ( genFor
  , genSigned
  , genUnsigned
  , genEnum
  , Arb (..)
  , ArbSigned (..)
  , ArbUnsigned (..)
  , ArbEnum (..)
  , ArbGeneric (..)
  )
where

import Control.Applicative (liftA2)
import Data.Bits (FiniteBits (..))
import Data.Int (Int16, Int8)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy (Proxy)
import Data.Word (Word16, Word8)
import GHC.Generics (Generic (..), K1 (..), M1 (..), U1 (..), (:*:) (..), (:+:) (..))
import Test.Falsify.Generator (Gen)
import qualified Test.Falsify.Generator as FG
import qualified Test.Falsify.Range as FR

genFor :: Arb a => Proxy a -> Gen a
genFor _ = arb

genSigned :: (Integral a, FiniteBits a, Bounded a) => Gen a
genSigned = FG.integral (FR.withOrigin (minBound, maxBound) 0)

genUnsigned :: (Integral a, FiniteBits a, Bounded a) => Gen a
genUnsigned = FG.integral (FR.between (0, maxBound))

genEnum :: (Enum a, Bounded a) => Gen a
genEnum = let b = minBound in FG.elem (b :| [succ b .. maxBound])

class Arb a where
  arb :: Gen a

deriving via (ArbUnsigned Word8) instance Arb Word8

deriving via (ArbSigned Int8) instance Arb Int8

deriving via (ArbUnsigned Word16) instance Arb Word16

deriving via (ArbSigned Int16) instance Arb Int16

newtype ArbSigned a = ArbSigned {unArbSigned :: a}

instance (Integral a, FiniteBits a, Bounded a) => Arb (ArbSigned a) where
  arb = fmap ArbSigned genSigned

newtype ArbUnsigned a = ArbUnsigned {unArbUnsigned :: a}

instance (Integral a, FiniteBits a, Bounded a) => Arb (ArbUnsigned a) where
  arb = fmap ArbUnsigned genUnsigned

newtype ArbEnum a = ArbEnum {unArbEnum :: a}

instance (Enum a, Bounded a) => Arb (ArbEnum a) where
  arb = fmap ArbEnum genEnum

class GArb f where
  garb :: Gen (f a)

-- Unit
instance GArb U1 where
  garb = pure U1

-- Metadata
instance GArb a => GArb (M1 i c a) where
  garb = fmap M1 garb

-- Product
instance (GArb a, GArb b) => GArb (a :*: b) where
  garb = liftA2 (:*:) garb garb

-- Sum
instance (GArb a, GArb b) => GArb (a :+: b) where
  garb = FG.choose (fmap L1 garb) (fmap R1 garb)

-- Field
instance Arb a => GArb (K1 i a) where
  garb = fmap K1 arb

newtype ArbGeneric a = ArbGeneric {unArbGeneric :: a}

instance (Generic t, GArb (Rep t)) => Arb (ArbGeneric t) where
  arb = fmap (ArbGeneric . to) garb
