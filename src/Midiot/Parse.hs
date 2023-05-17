module Midiot.Parse
  ( Eof (..)
  )
where

import Control.Monad (unless)
import Dahdit (Binary (..), ByteCount (..), ByteSized, StaticByteSized, getRemainingSize)

newtype Eof a = Eof {unEof :: a}
  deriving stock (Show)
  deriving newtype (Eq, Ord, ByteSized, StaticByteSized)

instance Binary a => Binary (Eof a) where
  get = do
    a <- get
    b <- getRemainingSize
    unless (b == 0) (fail ("Expected end of input but had bytes remaining: " ++ show (unByteCount b)))
    pure (Eof a)
  put = put . unEof
