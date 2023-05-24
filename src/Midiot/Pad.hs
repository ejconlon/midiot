module Midiot.Pad
  ( pad16
  , staticByteSizePad16
  , byteSizePad16
  , getPad16
  , putPad16
  )
where

import Control.Monad (replicateM_)
import Dahdit (Binary (..), ByteCount (..), Get, Put, getExpect, getRemainingSize)
import Data.Proxy (Proxy)
import Data.Word (Word8)

pad16 :: ByteCount -> ByteCount
pad16 x = x + (4 - rem x 4)

staticByteSizePad16 :: (Proxy a -> ByteCount) -> Proxy a -> ByteCount
staticByteSizePad16 staticSizer p = pad16 (staticSizer p)

byteSizePad16 :: (a -> ByteCount) -> a -> ByteCount
byteSizePad16 sizer a = pad16 (sizer a)

getPad16 :: Get a -> Get a
getPad16 getter = do
  x <- getRemainingSize
  a <- getter
  y <- getRemainingSize
  replicateM_ (rem (unByteCount (y - x)) 4) (getExpect "pad" (get @Word8) 0)
  pure a

putPad16 :: (a -> ByteCount) -> (a -> Put) -> a -> Put
putPad16 sizer putter a = do
  let x = sizer a
  putter a
  replicateM_ (4 - rem (unByteCount x) 4) (put @Word8 0)
