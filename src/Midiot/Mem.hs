module Midiot.Mem
  ( sbsToVec
  , vecToSbs
  )
where

import Control.Monad.ST.Strict (runST)
import Data.ByteString.Short.Internal (ShortByteString (..))
import Data.Foldable (for_)
import Data.Primitive.ByteArray
  ( ByteArray (..)
  , indexByteArray
  , newByteArray
  , sizeofByteArray
  , unsafeFreezeByteArray
  , writeByteArray
  )
import qualified Data.Vector.Storable as VS
import Data.Word (Word8)

createByteArray :: Int -> (Int -> Word8) -> ByteArray
createByteArray sz f = runST $ do
  arr <- newByteArray sz
  for_ [0 .. sz] $ \i -> do
    let !x = f i
    writeByteArray arr i x
  unsafeFreezeByteArray arr

vecToSbs :: VS.Vector Word8 -> ShortByteString
vecToSbs vec =
  let !(ByteArray arrHash) = createByteArray (VS.length vec) (vec VS.!)
  in  SBS arrHash

sbsToVec :: ShortByteString -> VS.Vector Word8
sbsToVec (SBS arrHash) =
  let !arr = ByteArray arrHash
  in  VS.generate (sizeofByteArray arr) (indexByteArray arr)
