module Midiot.OscAddr where

import Control.Exception (Exception)
import Dahdit (Binary (..), ByteCount (..), TermBytes8 (..), putByteString)
import Data.ByteString.Internal (c2w)
import Data.Foldable (foldMap', for_, toList)
import Data.Monoid (Sum (..))
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.String (IsString (..))
import Data.Text.Short (ShortText)
import qualified Data.Text.Short as TS
import qualified Data.Text.Short.Unsafe as TSU
import Data.Word (Word8)
import Midiot.Osc (byteSizePad4, getPad4, putPad4)

slashByte :: Word8
slashByte = c2w '/'

newtype Addr = Addr {unAddr :: Seq ShortText}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

instance IsString Addr where
  fromString s =
    let t = TS.pack s
    in  case parseAddr t of
          Left e -> error ("Invalid address " ++ show s ++ " : " ++ show e)
          Right a -> a

addrSizer :: Addr -> ByteCount
addrSizer (Addr parts) =
  ByteCount (Seq.length parts + getSum (foldMap' (Sum . TS.length) parts))

instance Binary Addr where
  byteSize = byteSizePad4 addrSizer
  get = getPad4 $ do
    s <- fmap (TSU.fromShortByteStringUnsafe . unTermBytes8) (get @TermBytes8)
    case parseAddr s of
      Left e -> fail ("Invalid address " ++ show s ++ " : " ++ show e)
      Right a -> pure a
  put = putPad4 addrSizer $ \(Addr parts) -> do
    for_ parts $ \part -> do
      put slashByte
      putByteString (TS.toShortByteString part)
    put @Word8 0

isInvalidAddrPartChar :: Char -> Bool
isInvalidAddrPartChar c =
  c == ' '
    || c == '#'
    || c == '*'
    || c == ','
    || c == '/'
    || c == '?'
    || c == '['
    || c == ']'
    || c == '{'
    || c == '}'

data AddrErr = AddrErrPartEmpty | AddrErrInvalidPartChar !Char | AddrErrExpectSlash !Char
  deriving stock (Eq, Ord, Show)

instance Exception AddrErr

parseAddr :: ShortText -> Either AddrErr Addr
parseAddr = goStart . TS.unpack
 where
  goStart = \case
    [] -> Right (Addr Empty)
    c : cs ->
      if c == '/'
        then goRest Empty Empty cs
        else Left (AddrErrExpectSlash c)
  pack = TS.pack . toList
  goRest !acc !pacc = \case
    [] ->
      if Seq.null pacc
        then Left AddrErrPartEmpty
        else Right (Addr (acc :|> pack pacc))
    c : cs ->
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

data Negate = NegateNo | NegateYes
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data PatFrag
  = PatFragText !ShortText
  | PatFragAnyMany
  | PatFragAnyOne
  | PatFragChoose !(Seq ShortText)
  | PatFragRange !Negate !ShortText !ShortText
  deriving stock (Eq, Ord, Show)

patFragSizer :: PatFrag -> ByteCount
patFragSizer = \case
  PatFragText t -> ByteCount (TS.length t)
  PatFragAnyMany -> 1
  PatFragAnyOne -> 1
  PatFragChoose ts -> ByteCount (1 + Seq.length ts + getSum (foldMap' (Sum . TS.length) ts))
  PatFragRange n t1 t2 -> ByteCount (3 + TS.length t1 + TS.length t2 + if n == NegateNo then 0 else 1)

type PatPart = Seq PatFrag

-- Addr encoding: zero-terminated, aligned to 4-byte boundary
newtype AddrPat = AddrPat {unAddrPat :: Seq PatPart}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

instance IsString AddrPat where
  fromString s =
    let t = TS.pack s
    in  case parseAddrPat t of
          Left e -> error ("Invalid address pattern " ++ show s ++ " : " ++ show e)
          Right a -> a

addrPatSizer :: AddrPat -> ByteCount
addrPatSizer (AddrPat _patParts) = undefined

instance Binary AddrPat where
  byteSize = byteSizePad4 addrPatSizer
  get = getPad4 $ do
    s <- fmap (TSU.fromShortByteStringUnsafe . unTermBytes8) (get @TermBytes8)
    case parseAddrPat s of
      Left e -> fail ("Invalid address pattern " ++ show s ++ " : " ++ show e)
      Right a -> pure a
  put = putPad4 addrPatSizer $ \(AddrPat _patParts) -> error "TODO"

data AddrPatErr = AddrPadErr
  deriving stock (Eq, Ord, Show)

instance Exception AddrPatErr

parseAddrPat :: ShortText -> Either AddrPatErr AddrPat
parseAddrPat = error "TODO"

printAddrPat :: AddrPat -> ShortText
printAddrPat = error "TODO"

matchPart :: PatPart -> ShortText -> Bool
matchPart = error "TODO"

matchAddr :: AddrPat -> Addr -> Bool
matchAddr (AddrPat patParts) (Addr parts) =
  (Seq.length patParts == Seq.length parts)
    && and (zipWith matchPart (toList patParts) (toList parts))