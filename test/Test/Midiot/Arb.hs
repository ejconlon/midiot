{-# LANGUAGE OverloadedStrings #-}

module Test.Midiot.Arb
  ( arbI
  )
where

import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as BSS
import qualified Data.List.NonEmpty as NE
import Data.Proxy (Proxy (..))
import Data.ShortWord (Int7, Word7)
import qualified Data.Text as T
import qualified Midiot.Binary as MB
import qualified Midiot.Midi as MM
import qualified Midiot.Osc as MO
import qualified Midiot.OscAddr as MOA
import qualified Midiot.Time as MT
import Test.Dahdit.Arb
import Test.Falsify.Generator (Gen)
import qualified Test.Falsify.Generator as FG
import qualified Test.Falsify.Range as FR

data P

type I = DahditIdx P

arbI :: Arb I a => Gen a
arbI = arb (Proxy @I) Proxy

-- Binary

deriving via (ArbUnsigned Word7) instance Arb I MB.MidiWord7

deriving via (ArbSigned Int7) instance Arb I MB.MidiInt7

deriving via (ArbUnsigned MB.Word14) instance Arb I MB.MidiWord14

deriving via (ArbSigned MB.Int14) instance Arb I MB.MidiInt14

instance Arb I MB.VarWord where
  arb _ _ = fmap MB.VarWord (FG.integral (FR.between (0, 0x00FFFFFF)))

-- OscAddr

-- TODO generate addr pat and serialize
instance Arb I MOA.RawAddrPat where
  arb _ _ = MOA.RawAddrPat . ("/" <>) . T.intercalate "/" <$> genList 1 3 g
   where
    g = FG.choose (pure "x") (pure "y")

-- Midi

deriving via (ArbEnum MM.Channel) instance Arb I MM.Channel

deriving newtype instance Arb I MM.ChannelCount

deriving newtype instance Arb I MM.Note

deriving newtype instance Arb I MM.Velocity

deriving newtype instance Arb I MM.ControlNum

deriving newtype instance Arb I MM.ControlVal

deriving newtype instance Arb I MM.Pressure

deriving newtype instance Arb I MM.ProgramNum

deriving newtype instance Arb I MM.PitchBend

deriving newtype instance Arb I MM.Song

deriving newtype instance Arb I MM.Position

instance Arb I MM.ShortManf where
  arb p _ = go
   where
    go = do
      i <- arb p Proxy
      if i == 0x00 || i == 0x7E || i == 0x7F
        then go
        else pure (MM.ShortManf i)

deriving newtype instance (Arb I MM.LongManf)

deriving via (ArbGeneric I MM.Manf) instance Arb I MM.Manf

deriving via (ArbGeneric I MM.QuarterTimeUnit) instance Arb I MM.QuarterTimeUnit

instance Arb I MM.QuarterTime where
  arb p _ = MM.QuarterTime <$> arb p Proxy <*> genUnsigned

deriving via (ArbEnum MM.ChanStatusType) instance Arb I MM.ChanStatusType

deriving via (ArbEnum MM.CommonStatus) instance Arb I MM.CommonStatus

deriving via (ArbEnum MM.RtStatus) instance Arb I MM.RtStatus

deriving via (ArbGeneric I MM.ChanStatus) instance Arb I MM.ChanStatus

deriving via (ArbGeneric I MM.LiveStatus) instance Arb I MM.LiveStatus

deriving via (ArbGeneric I MM.RecStatus) instance Arb I MM.RecStatus

deriving via (ArbGeneric I MM.ShortStatus) instance Arb I MM.ShortStatus

instance Arb I MM.ChanVoiceData where
  arb p _ = genCVD
   where
    genCVD =
      genSum $
        NE.fromList
          [ MM.ChanVoiceDataNoteOff <$> arb p Proxy <*> arb p Proxy
          , MM.ChanVoiceDataNoteOn <$> arb p Proxy <*> arb p Proxy
          , MM.ChanVoiceKeyAftertouch <$> arb p Proxy <*> arb p Proxy
          , MM.ChanVoiceControlChange <$> genCN <*> arb p Proxy
          , MM.ChanVoiceProgramChange <$> arb p Proxy
          , MM.ChanVoiceChanAftertouch <$> arb p Proxy
          , MM.ChanVoicePitchBend <$> arb p Proxy
          ]
    genCN = fmap (MM.ControlNum . MB.MidiWord7) (FG.integral (FR.between (0x00, 0x77)))

instance Arb I MM.MetaString where
  arb _ _ = fmap MM.MetaString (genSBS 0 3)

deriving via (ArbGeneric I MM.MetaData) instance Arb I MM.MetaData

deriving via (ArbGeneric I MM.ChanModeData) instance Arb I MM.ChanModeData

deriving via (ArbGeneric I MM.ChanData) instance Arb I MM.ChanData

-- Generate a bytestring not including the delimiter
genPayload :: Gen ShortByteString
genPayload = fmap (BSS.pack . fmap fromIntegral) (genList 0 3 (arb (Proxy @I) (Proxy @MB.MidiWord7)))

instance Arb I MM.UnivSysEx where
  arb _ _ = MM.UnivSysEx <$> FG.choose (pure 0x7E) (pure 0x7F) <*> genPayload

instance Arb I MM.ManfSysEx where
  arb p _ = MM.ManfSysEx <$> arb p Proxy <*> genPayload

deriving via (ArbGeneric I MM.SysExData) instance Arb I MM.SysExData

deriving via (ArbGeneric I MM.CommonData) instance Arb I MM.CommonData

deriving via (ArbGeneric I MM.LiveMsg) instance Arb I MM.LiveMsg

deriving via (ArbGeneric I MM.RecMsg) instance Arb I MM.RecMsg

deriving via (ArbGeneric I MM.ShortMsg) instance Arb I MM.ShortMsg

deriving via (ArbGeneric I MM.Event) instance Arb I MM.Event

instance Arb I MM.Track where
  arb p _ = fmap MM.Track (genSeq 0 3 (arb p Proxy))

deriving via (ArbEnum MM.MidFileType) instance Arb I MM.MidFileType

instance Arb I MM.MidFile where
  arb p _ = MM.MidFile <$> arb p Proxy <*> arb p Proxy <*> genSeq 0 3 (arb p Proxy)

instance Arb I MM.SysExDump where
  arb p _ = fmap MM.SysExDump (genSeq 0 3 (arb p Proxy))

-- Osc

deriving via (ArbEnum MO.DatumType) instance Arb I MO.DatumType

deriving newtype instance Arb I MO.Port

deriving via (ArbGeneric I MO.PortMsg) instance Arb I MO.PortMsg

instance Arb I MO.Sig where
  arb p _ = fmap MO.Sig (genSeq 0 3 (arb p Proxy))

instance Arb I MO.Datum where
  arb p _ =
    foldr1
      FG.choose
      [ MO.DatumInt32 <$> genSigned
      , MO.DatumInt64 <$> genSigned
      , MO.DatumFloat <$> genFractional
      , MO.DatumDouble <$> genFractional
      , MO.DatumString . T.pack <$> genList 0 3 (FG.choose (pure 'x') (pure 'y'))
      , MO.DatumBlob <$> genSBS 0 3
      , MO.DatumTime . MT.NtpTime <$> genUnsigned
      , MO.DatumMidi <$> arb p Proxy
      ]

instance Arb I MO.Msg where
  arb p _ = MO.Msg <$> arb p Proxy <*> genSeq 0 3 (arb p Proxy)

instance Arb I MO.Bundle where
  arb p _ = MO.Bundle <$> (MT.NtpTime <$> genUnsigned) <*> genSeq 0 3 (arb p Proxy)

deriving via (ArbGeneric I MO.Packet) instance Arb I MO.Packet
