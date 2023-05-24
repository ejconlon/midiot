module Midiot.Arb
  ( module Test.Dahdit.Arb
  , I
  , arbI
  )
where

import Data.Proxy (Proxy (..))
import Test.Dahdit.Arb
import Test.Falsify.Generator (Gen)

data P

type I = DahditIdx P

arbI :: Arb I a => Gen a
arbI = arb (Proxy @I) Proxy
