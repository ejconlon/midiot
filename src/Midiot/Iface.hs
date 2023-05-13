module Midiot.Iface
  ( mutSendMessage
  )
where

import Dahdit (ByteCount (..))
import Data.Vector.Storable.Mutable (IOVector)
import qualified Data.Vector.Storable.Mutable as VSM
import Data.Word (Word8)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr (plusPtr)
import Sound.RtMidi (OutputDevice)
import qualified Sound.RtMidi as R

mutSendMessage :: OutputDevice -> IOVector Word8 -> ByteCount -> ByteCount -> IO ()
mutSendMessage d mvec off len =
  let (fp, _) = VSM.unsafeToForeignPtr0 mvec
      p = plusPtr (unsafeForeignPtrToPtr fp) (unByteCount off)
  in  R.sendUnsafeMessage d p (unByteCount len)
