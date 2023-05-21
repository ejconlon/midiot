module Midiot.Parse
  ( decodeEof
  , decodeFileEof
  , reDecodeEof
  )
where

import Control.Monad (unless)
import Dahdit (Binary (..), BinaryTarget, ByteCount (..), GetError, StaticByteSized, decode, decodeFile, encode, getRemainingSize)
import Data.Bifunctor (first)
import Data.ByteString.Short (ShortByteString)

newtype Eof a = Eof {unEof :: a}
  deriving stock (Show)
  deriving newtype (Eq, Ord, StaticByteSized)

instance Binary a => Binary (Eof a) where
  get = do
    a <- get
    b <- getRemainingSize
    unless (b == 0) (fail ("Expected end of input but had bytes remaining: " ++ show (unByteCount b)))
    pure (Eof a)
  put = put . unEof

decodeEof :: (BinaryTarget z, Binary a) => z -> (Either GetError a, ByteCount)
decodeEof = first (fmap unEof) . decode

decodeFileEof :: Binary a => FilePath -> IO (Either GetError a, ByteCount)
decodeFileEof = fmap (first (fmap unEof)) . decodeFile

reDecodeEof :: Binary a => a -> (Either GetError a, ByteCount)
reDecodeEof = decodeEof . encode @_ @ShortByteString
