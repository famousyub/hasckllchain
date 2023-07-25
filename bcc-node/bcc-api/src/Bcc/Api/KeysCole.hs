{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Cole key types and their 'Key' class instances
--
module Bcc.Api.KeysCole (

    -- * Key types
    ColeKey,
    ColeKeyLegacy,

    -- * Data family instances
    AsType(..),
    VerificationKey(..),
    SigningKey(..),
    Hash(..),

    -- * Legacy format
    IsColeKey(..),
    ColeKeyFormat(..),

    SomeColeSigningKey(..),
    toColeSigningKey
  ) where

import           Bcc.Prelude (cborError, toCborError)
import           Prelude

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import           Control.Monad
import qualified Data.ByteString.Lazy as LB
import           Data.String (IsString)
import           Data.Text (Text)
import qualified Data.Text as Text

import qualified Bcc.Crypto.DSIGN.Class as Crypto
import qualified Bcc.Crypto.Seed as Crypto
import qualified Bcc.Crypto.Signing as Crypto
import qualified Bcc.Crypto.Wallet as Crypto.HD

import           Bcc.Binary (toStrictByteString)
import qualified Bcc.Chain.Common as Cole
import qualified Bcc.Crypto.Hashing as Cole
import qualified Bcc.Crypto.Signing as Cole
import qualified Bcc.Crypto.Wallet as Wallet

import           Bcc.Api.HasTypeProxy
import           Bcc.Api.Hash
import           Bcc.Api.Key
import           Bcc.Api.KeysSophie
import           Bcc.Api.SerialiseCBOR
import           Bcc.Api.SerialiseRaw
import           Bcc.Api.SerialiseTextEnvelope
import           Bcc.Api.SerialiseUsing


-- | Cole-era payment keys. Used for Cole addresses and witnessing
-- transactions that spend from these addresses.
--
-- These use Ed25519 but with a 32byte \"chaincode\" used in HD derivation.
-- The inclusion of the chaincode is a design mistake but one that cannot
-- be corrected for the Cole era. The Sophie era 'PaymentKey's do not include
-- a chaincode. It is safe to use a zero or random chaincode for new Cole keys.
--
-- This is a type level tag, used with other interfaces like 'Key'.
--
data ColeKey
data ColeKeyLegacy

class IsColeKey key where
    coleKeyFormat :: ColeKeyFormat key

data ColeKeyFormat key where
  ColeLegacyKeyFormat :: ColeKeyFormat ColeKeyLegacy
  ColeModernKeyFormat :: ColeKeyFormat ColeKey

data SomeColeSigningKey
  = AColeSigningKeyLegacy (SigningKey ColeKeyLegacy)
  | AColeSigningKey (SigningKey ColeKey)

toColeSigningKey :: SomeColeSigningKey -> Cole.SigningKey
toColeSigningKey bWit =
  case bWit of
    AColeSigningKeyLegacy (ColeSigningKeyLegacy sKey) -> sKey
    AColeSigningKey (ColeSigningKey sKey) -> sKey

--
-- Cole key
--

instance Key ColeKey where

    newtype VerificationKey ColeKey =
           ColeVerificationKey Cole.VerificationKey
      deriving stock Eq
      deriving (Show, IsString) via UsingRawBytesHex (VerificationKey ColeKey)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    newtype SigningKey ColeKey =
           ColeSigningKey Cole.SigningKey
      deriving (Show, IsString) via UsingRawBytesHex (SigningKey ColeKey)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    deterministicSigningKey :: AsType ColeKey -> Crypto.Seed -> SigningKey ColeKey
    deterministicSigningKey AsColeKey seed =
       ColeSigningKey (snd (Crypto.runMonadRandomWithSeed seed Cole.keyGen))

    deterministicSigningKeySeedSize :: AsType ColeKey -> Word
    deterministicSigningKeySeedSize AsColeKey = 32

    getVerificationKey :: SigningKey ColeKey -> VerificationKey ColeKey
    getVerificationKey (ColeSigningKey sk) =
      ColeVerificationKey (Cole.toVerification sk)

    verificationKeyHash :: VerificationKey ColeKey -> Hash ColeKey
    verificationKeyHash (ColeVerificationKey vkey) =
      ColeKeyHash (Cole.hashKey vkey)

instance HasTypeProxy ColeKey where
    data AsType ColeKey = AsColeKey
    proxyToAsType _ = AsColeKey

instance HasTextEnvelope (VerificationKey ColeKey) where
    textEnvelopeType _ = "PaymentVerificationKeyCole_ed25519_bip32"

instance HasTextEnvelope (SigningKey ColeKey) where
    textEnvelopeType _ = "PaymentSigningKeyCole_ed25519_bip32"

instance SerialiseAsRawBytes (VerificationKey ColeKey) where
    serialiseToRawBytes (ColeVerificationKey (Cole.VerificationKey xvk)) =
      Crypto.HD.unXPub xvk

    deserialiseFromRawBytes (AsVerificationKey AsColeKey) bs =
      either (const Nothing) (Just . ColeVerificationKey . Cole.VerificationKey)
             (Crypto.HD.xpub bs)

instance SerialiseAsRawBytes (SigningKey ColeKey) where
    serialiseToRawBytes (ColeSigningKey (Cole.SigningKey xsk)) =
      toStrictByteString $ Crypto.toCBORXPrv xsk

    deserialiseFromRawBytes (AsSigningKey AsColeKey) bs =
      either (const Nothing) (Just . ColeSigningKey . Cole.SigningKey)
             (snd <$> CBOR.deserialiseFromBytes Cole.fromCBORXPrv (LB.fromStrict bs))

newtype instance Hash ColeKey = ColeKeyHash Cole.KeyHash
  deriving (Eq, Ord)
  deriving (Show, IsString) via UsingRawBytesHex (Hash ColeKey)
  deriving (ToCBOR, FromCBOR) via UsingRawBytes (Hash ColeKey)
  deriving anyclass SerialiseAsCBOR

instance SerialiseAsRawBytes (Hash ColeKey) where
    serialiseToRawBytes (ColeKeyHash (Cole.KeyHash vkh)) =
      Cole.abstractHashToBytes vkh

    deserialiseFromRawBytes (AsHash AsColeKey) bs =
      ColeKeyHash . Cole.KeyHash <$> Cole.abstractHashFromBytes bs

instance CastVerificationKeyRole ColeKey PaymentExtendedKey where
    castVerificationKey (ColeVerificationKey vk) =
        PaymentExtendedVerificationKey
          (Cole.unVerificationKey vk)

instance CastVerificationKeyRole ColeKey PaymentKey where
    castVerificationKey =
        (castVerificationKey :: VerificationKey PaymentExtendedKey
                             -> VerificationKey PaymentKey)
      . (castVerificationKey :: VerificationKey ColeKey
                             -> VerificationKey PaymentExtendedKey)

instance IsColeKey ColeKey where
  coleKeyFormat = ColeModernKeyFormat

--
-- Legacy Cole key
--

instance Key ColeKeyLegacy where

    newtype VerificationKey ColeKeyLegacy =
           ColeVerificationKeyLegacy Cole.VerificationKey
      deriving stock (Eq)
      deriving (Show, IsString) via UsingRawBytesHex (VerificationKey ColeKeyLegacy)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    newtype SigningKey ColeKeyLegacy =
           ColeSigningKeyLegacy Cole.SigningKey
      deriving (Show, IsString) via UsingRawBytesHex (SigningKey ColeKeyLegacy)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    deterministicSigningKey :: AsType ColeKeyLegacy -> Crypto.Seed -> SigningKey ColeKeyLegacy
    deterministicSigningKey _ _ = error "Please generate a non legacy Cole key instead"

    deterministicSigningKeySeedSize :: AsType ColeKeyLegacy -> Word
    deterministicSigningKeySeedSize AsColeKeyLegacy = 32

    getVerificationKey :: SigningKey ColeKeyLegacy -> VerificationKey ColeKeyLegacy
    getVerificationKey (ColeSigningKeyLegacy sk) =
      ColeVerificationKeyLegacy (Cole.toVerification sk)

    verificationKeyHash :: VerificationKey ColeKeyLegacy -> Hash ColeKeyLegacy
    verificationKeyHash (ColeVerificationKeyLegacy vkey) =
      ColeKeyHashLegacy (Cole.hashKey vkey)

instance HasTypeProxy ColeKeyLegacy where
  data AsType ColeKeyLegacy = AsColeKeyLegacy
  proxyToAsType _ = AsColeKeyLegacy

instance HasTextEnvelope (VerificationKey ColeKeyLegacy) where
    textEnvelopeType _ = "PaymentVerificationKeyColeLegacy_ed25519_bip32"

instance HasTextEnvelope (SigningKey ColeKeyLegacy) where
    textEnvelopeType _ = "PaymentSigningKeyColeLegacy_ed25519_bip32"

newtype instance Hash ColeKeyLegacy = ColeKeyHashLegacy Cole.KeyHash
  deriving (Eq, Ord)
  deriving (Show, IsString) via UsingRawBytesHex (Hash ColeKeyLegacy)
  deriving (ToCBOR, FromCBOR) via UsingRawBytes (Hash ColeKeyLegacy)
  deriving anyclass SerialiseAsCBOR

instance SerialiseAsRawBytes (Hash ColeKeyLegacy) where
    serialiseToRawBytes (ColeKeyHashLegacy (Cole.KeyHash vkh)) =
      Cole.abstractHashToBytes vkh

    deserialiseFromRawBytes (AsHash AsColeKeyLegacy) bs =
      ColeKeyHashLegacy . Cole.KeyHash <$> Cole.abstractHashFromBytes bs

instance SerialiseAsRawBytes (VerificationKey ColeKeyLegacy) where
    serialiseToRawBytes (ColeVerificationKeyLegacy (Cole.VerificationKey xvk)) =
      Crypto.HD.unXPub xvk

    deserialiseFromRawBytes (AsVerificationKey AsColeKeyLegacy) bs =
      either (const Nothing) (Just . ColeVerificationKeyLegacy . Cole.VerificationKey)
             (Crypto.HD.xpub bs)

instance SerialiseAsRawBytes (SigningKey ColeKeyLegacy) where
    serialiseToRawBytes (ColeSigningKeyLegacy (Cole.SigningKey xsk)) =
      Crypto.HD.unXPrv xsk

    deserialiseFromRawBytes (AsSigningKey AsColeKeyLegacy) bs =
      either (const Nothing) (Just . ColeSigningKeyLegacy . snd)
             (CBOR.deserialiseFromBytes decodeLegacyDelegateKey $ LB.fromStrict bs)
     where
      -- Stolen from: bcc-sl/binary/src/Pos/Binary/Class/Core.hs
      -- | Enforces that the input size is the same as the decoded one, failing in
      -- case it's not.
      enforceSize :: Text -> Int -> CBOR.Decoder s ()
      enforceSize lbl requestedSize = CBOR.decodeListLenCanonical >>= matchSize requestedSize lbl

      -- Stolen from: bcc-sl/binary/src/Pos/Binary/Class/Core.hs
      -- | Compare two sizes, failing if they are not equal.
      matchSize :: Int -> Text -> Int -> CBOR.Decoder s ()
      matchSize requestedSize lbl actualSize =
        when (actualSize /= requestedSize) $
          cborError ( lbl <> " failed the size check. Expected " <> Text.pack (show requestedSize)
                          <> ", found " <> Text.pack (show actualSize)
                    )

      decodeXPrv :: CBOR.Decoder s Wallet.XPrv
      decodeXPrv = CBOR.decodeBytesCanonical >>= toCborError . Wallet.xprv


      -- | Decoder for a Cole/Classic signing key.
      --   Lifted from bcc-sl legacy codebase.
      decodeLegacyDelegateKey :: CBOR.Decoder s Cole.SigningKey
      decodeLegacyDelegateKey = do
          enforceSize "UserSecret" 4
          _    <- do
            enforceSize "vss" 1
            CBOR.decodeBytes
          pkey <- do
            enforceSize "pkey" 1
            Cole.SigningKey <$> decodeXPrv
          _    <- do
            CBOR.decodeListLenIndef
            CBOR.decodeSequenceLenIndef (flip (:)) [] reverse CBOR.decodeNull
          _    <- do
            enforceSize "wallet" 0
          pure pkey

instance CastVerificationKeyRole ColeKeyLegacy ColeKey where
    castVerificationKey (ColeVerificationKeyLegacy vk) =
        ColeVerificationKey vk

instance IsColeKey ColeKeyLegacy where
  coleKeyFormat = ColeLegacyKeyFormat
