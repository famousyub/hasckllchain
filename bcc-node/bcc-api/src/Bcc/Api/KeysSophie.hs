{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- The Sophie ledger uses promoted data kinds which we have to use, but we do
-- not export any from this API. We also use them unticked as nature intended.
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

-- | Sophie key types and their 'Key' class instances
--
module Bcc.Api.KeysSophie (

    -- * Key types
    PaymentKey,
    PaymentExtendedKey,
    StakeKey,
    StakeExtendedKey,
    StakePoolKey,
    GenesisKey,
    GenesisExtendedKey,
    GenesisDelegateKey,
    GenesisDelegateExtendedKey,
    GenesisVestedKey,
    GenesisVestedExtendedKey,
    GenesisVestedDelegateKey,
    GenesisVestedDelegateExtendedKey,
    GenesisUTxOKey,
    VestedKey,
    VestedExtendedKey,
    VestedDelegateKey,
    VestedDelegateExtendedKey,
    VestedUTxOKey,

    -- * Data family instances
    AsType(..),
    VerificationKey(..),
    SigningKey(..),
    Hash(..),
  ) where

import           Prelude

import           Data.Aeson.Types (ToJSONKey (..), toJSONKeyText)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Maybe
import           Data.String (IsString (..))

import qualified Bcc.Crypto.DSIGN.Class as Crypto
import qualified Bcc.Crypto.Hash.Class as Crypto
import qualified Bcc.Crypto.Seed as Crypto
import qualified Bcc.Crypto.Wallet as Crypto.HD
import qualified Bcc.Ledger.Crypto as Sophie (DSIGN)
import qualified Bcc.Ledger.Keys as Sophie

import           Bcc.Ledger.Crypto (StandardCrypto)

import           Bcc.Api.Hash
import           Bcc.Api.HasTypeProxy
import           Bcc.Api.Key
import           Bcc.Api.SerialiseBech32
import           Bcc.Api.SerialiseCBOR
import           Bcc.Api.SerialiseJSON
import           Bcc.Api.SerialiseRaw
import           Bcc.Api.SerialiseTextEnvelope
import           Bcc.Api.SerialiseUsing


--
-- Sophie payment keys
--

-- | Sophie-era payment keys. Used for Sophie payment addresses and witnessing
-- transactions that spend from these addresses.
--
-- This is a type level tag, used with other interfaces like 'Key'.
--
data PaymentKey

instance HasTypeProxy PaymentKey where
    data AsType PaymentKey = AsPaymentKey
    proxyToAsType _ = AsPaymentKey

instance Key PaymentKey where

    newtype VerificationKey PaymentKey =
        PaymentVerificationKey (Sophie.VKey Sophie.Payment StandardCrypto)
      deriving stock (Eq)
      deriving (Show, IsString) via UsingRawBytesHex (VerificationKey PaymentKey)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    newtype SigningKey PaymentKey =
        PaymentSigningKey (Sophie.SignKeyDSIGN StandardCrypto)
      deriving (Show, IsString) via UsingRawBytesHex (SigningKey PaymentKey)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    deterministicSigningKey :: AsType PaymentKey -> Crypto.Seed -> SigningKey PaymentKey
    deterministicSigningKey AsPaymentKey seed =
        PaymentSigningKey (Crypto.genKeyDSIGN seed)

    deterministicSigningKeySeedSize :: AsType PaymentKey -> Word
    deterministicSigningKeySeedSize AsPaymentKey =
        Crypto.seedSizeDSIGN proxy
      where
        proxy :: Proxy (Sophie.DSIGN StandardCrypto)
        proxy = Proxy

    getVerificationKey :: SigningKey PaymentKey -> VerificationKey PaymentKey
    getVerificationKey (PaymentSigningKey sk) =
        PaymentVerificationKey (Sophie.VKey (Crypto.deriveVerKeyDSIGN sk))

    verificationKeyHash :: VerificationKey PaymentKey -> Hash PaymentKey
    verificationKeyHash (PaymentVerificationKey vkey) =
        PaymentKeyHash (Sophie.hashKey vkey)

instance SerialiseAsRawBytes (VerificationKey PaymentKey) where
    serialiseToRawBytes (PaymentVerificationKey (Sophie.VKey vk)) =
      Crypto.rawSerialiseVerKeyDSIGN vk

    deserialiseFromRawBytes (AsVerificationKey AsPaymentKey) bs =
      PaymentVerificationKey . Sophie.VKey <$>
        Crypto.rawDeserialiseVerKeyDSIGN bs

instance SerialiseAsRawBytes (SigningKey PaymentKey) where
    serialiseToRawBytes (PaymentSigningKey sk) =
      Crypto.rawSerialiseSignKeyDSIGN sk

    deserialiseFromRawBytes (AsSigningKey AsPaymentKey) bs =
      PaymentSigningKey <$> Crypto.rawDeserialiseSignKeyDSIGN bs

instance SerialiseAsBech32 (VerificationKey PaymentKey) where
    bech32PrefixFor         _ =  "addr_vk"
    bech32PrefixesPermitted _ = ["addr_vk"]

instance SerialiseAsBech32 (SigningKey PaymentKey) where
    bech32PrefixFor         _ =  "addr_sk"
    bech32PrefixesPermitted _ = ["addr_sk"]

newtype instance Hash PaymentKey =
    PaymentKeyHash (Sophie.KeyHash Sophie.Payment StandardCrypto)
  deriving stock (Eq, Ord)
  deriving (Show, IsString) via UsingRawBytesHex (Hash PaymentKey)
  deriving (ToCBOR, FromCBOR) via UsingRawBytes (Hash PaymentKey)
  deriving anyclass SerialiseAsCBOR

instance SerialiseAsRawBytes (Hash PaymentKey) where
    serialiseToRawBytes (PaymentKeyHash (Sophie.KeyHash vkh)) =
      Crypto.hashToBytes vkh

    deserialiseFromRawBytes (AsHash AsPaymentKey) bs =
      PaymentKeyHash . Sophie.KeyHash <$> Crypto.hashFromBytes bs

instance HasTextEnvelope (VerificationKey PaymentKey) where
    textEnvelopeType _ = "PaymentVerificationKeySophie_"
                      <> fromString (Crypto.algorithmNameDSIGN proxy)
      where
        proxy :: Proxy (Sophie.DSIGN StandardCrypto)
        proxy = Proxy

instance HasTextEnvelope (SigningKey PaymentKey) where
    textEnvelopeType _ = "PaymentSigningKeySophie_"
                      <> fromString (Crypto.algorithmNameDSIGN proxy)
      where
        proxy :: Proxy (Sophie.DSIGN StandardCrypto)
        proxy = Proxy


--
-- Sophie payment extended ed25519 keys
--

-- | Sophie-era payment keys using extended ed25519 cryptographic keys.
--
-- They can be used for Sophie payment addresses and witnessing
-- transactions that spend from these addresses.
--
-- These extended keys are used by HD wallets. So this type provides
-- interoperability with HD wallets. The ITN CLI also supported this key type.
--
-- The extended verification keys can be converted (via 'castVerificationKey')
-- to ordinary keys (i.e. 'VerificationKey' 'PaymentKey') but this is /not/ the
-- case for the signing keys. The signing keys can be used to witness
-- transactions directly, with verification via their non-extended verification
-- key ('VerificationKey' 'PaymentKey').
--
-- This is a type level tag, used with other interfaces like 'Key'.
--
data PaymentExtendedKey

instance HasTypeProxy PaymentExtendedKey where
    data AsType PaymentExtendedKey = AsPaymentExtendedKey
    proxyToAsType _ = AsPaymentExtendedKey

instance Key PaymentExtendedKey where

    newtype VerificationKey PaymentExtendedKey =
        PaymentExtendedVerificationKey Crypto.HD.XPub
      deriving stock (Eq)
      deriving anyclass SerialiseAsCBOR
      deriving (Show, IsString) via UsingRawBytesHex (VerificationKey PaymentExtendedKey)

    newtype SigningKey PaymentExtendedKey =
        PaymentExtendedSigningKey Crypto.HD.XPrv
      deriving anyclass SerialiseAsCBOR
      deriving (Show, IsString) via UsingRawBytesHex (SigningKey PaymentExtendedKey)

    deterministicSigningKey :: AsType PaymentExtendedKey
                            -> Crypto.Seed
                            -> SigningKey PaymentExtendedKey
    deterministicSigningKey AsPaymentExtendedKey seed =
        PaymentExtendedSigningKey
          (Crypto.HD.generate seedbs BS.empty)
      where
       (seedbs, _) = Crypto.getBytesFromSeedT 32 seed

    deterministicSigningKeySeedSize :: AsType PaymentExtendedKey -> Word
    deterministicSigningKeySeedSize AsPaymentExtendedKey = 32

    getVerificationKey :: SigningKey PaymentExtendedKey
                       -> VerificationKey PaymentExtendedKey
    getVerificationKey (PaymentExtendedSigningKey sk) =
        PaymentExtendedVerificationKey (Crypto.HD.toXPub sk)

    -- | We use the hash of the normal non-extended pub key so that it is
    -- consistent with the one used in addresses and signatures.
    --
    verificationKeyHash :: VerificationKey PaymentExtendedKey
                        -> Hash PaymentExtendedKey
    verificationKeyHash (PaymentExtendedVerificationKey vk) =
        PaymentExtendedKeyHash
      . Sophie.KeyHash
      . Crypto.castHash
      $ Crypto.hashWith Crypto.HD.xpubPublicKey vk


instance ToCBOR (VerificationKey PaymentExtendedKey) where
    toCBOR (PaymentExtendedVerificationKey xpub) =
      toCBOR (Crypto.HD.unXPub xpub)

instance FromCBOR (VerificationKey PaymentExtendedKey) where
    fromCBOR = do
      bs <- fromCBOR
      either fail (return . PaymentExtendedVerificationKey)
             (Crypto.HD.xpub (bs :: ByteString))

instance ToCBOR (SigningKey PaymentExtendedKey) where
    toCBOR (PaymentExtendedSigningKey xprv) =
      toCBOR (Crypto.HD.unXPrv xprv)

instance FromCBOR (SigningKey PaymentExtendedKey) where
    fromCBOR = do
      bs <- fromCBOR
      either fail (return . PaymentExtendedSigningKey)
             (Crypto.HD.xprv (bs :: ByteString))

instance SerialiseAsRawBytes (VerificationKey PaymentExtendedKey) where
    serialiseToRawBytes (PaymentExtendedVerificationKey xpub) =
      Crypto.HD.unXPub xpub

    deserialiseFromRawBytes (AsVerificationKey AsPaymentExtendedKey) bs =
      either (const Nothing) (Just . PaymentExtendedVerificationKey)
             (Crypto.HD.xpub bs)

instance SerialiseAsRawBytes (SigningKey PaymentExtendedKey) where
    serialiseToRawBytes (PaymentExtendedSigningKey xprv) =
      Crypto.HD.unXPrv xprv

    deserialiseFromRawBytes (AsSigningKey AsPaymentExtendedKey) bs =
      either (const Nothing) (Just . PaymentExtendedSigningKey)
             (Crypto.HD.xprv bs)

instance SerialiseAsBech32 (VerificationKey PaymentExtendedKey) where
    bech32PrefixFor         _ =  "addr_xvk"
    bech32PrefixesPermitted _ = ["addr_xvk"]

instance SerialiseAsBech32 (SigningKey PaymentExtendedKey) where
    bech32PrefixFor         _ =  "addr_xsk"
    bech32PrefixesPermitted _ = ["addr_xsk"]


newtype instance Hash PaymentExtendedKey =
    PaymentExtendedKeyHash (Sophie.KeyHash Sophie.Payment StandardCrypto)
  deriving stock (Eq, Ord)
  deriving (Show, IsString) via UsingRawBytesHex (Hash PaymentExtendedKey)
  deriving (ToCBOR, FromCBOR) via UsingRawBytes (Hash PaymentExtendedKey)
  deriving anyclass SerialiseAsCBOR

instance SerialiseAsRawBytes (Hash PaymentExtendedKey) where
    serialiseToRawBytes (PaymentExtendedKeyHash (Sophie.KeyHash vkh)) =
      Crypto.hashToBytes vkh

    deserialiseFromRawBytes (AsHash AsPaymentExtendedKey) bs =
      PaymentExtendedKeyHash . Sophie.KeyHash <$> Crypto.hashFromBytes bs

instance HasTextEnvelope (VerificationKey PaymentExtendedKey) where
    textEnvelopeType _ = "PaymentExtendedVerificationKeySophie_ed25519_bip32"

instance HasTextEnvelope (SigningKey PaymentExtendedKey) where
    textEnvelopeType _ = "PaymentExtendedSigningKeySophie_ed25519_bip32"

instance CastVerificationKeyRole PaymentExtendedKey PaymentKey where
    castVerificationKey (PaymentExtendedVerificationKey vk) =
        PaymentVerificationKey
      . Sophie.VKey
      . fromMaybe impossible
      . Crypto.rawDeserialiseVerKeyDSIGN
      . Crypto.HD.xpubPublicKey
      $ vk
      where
        impossible =
          error "castVerificationKey: cole and sophie key sizes do not match!"


--
-- Stake keys
--

data StakeKey

instance HasTypeProxy StakeKey where
    data AsType StakeKey = AsStakeKey
    proxyToAsType _ = AsStakeKey

instance Key StakeKey where

    newtype VerificationKey StakeKey =
        StakeVerificationKey (Sophie.VKey Sophie.Staking StandardCrypto)
      deriving stock (Eq)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR
      deriving (Show, IsString) via UsingRawBytesHex (VerificationKey StakeKey)

    newtype SigningKey StakeKey =
        StakeSigningKey (Sophie.SignKeyDSIGN StandardCrypto)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR
      deriving (Show, IsString) via UsingRawBytesHex (SigningKey StakeKey)

    deterministicSigningKey :: AsType StakeKey -> Crypto.Seed -> SigningKey StakeKey
    deterministicSigningKey AsStakeKey seed =
        StakeSigningKey (Crypto.genKeyDSIGN seed)

    deterministicSigningKeySeedSize :: AsType StakeKey -> Word
    deterministicSigningKeySeedSize AsStakeKey =
        Crypto.seedSizeDSIGN proxy
      where
        proxy :: Proxy (Sophie.DSIGN StandardCrypto)
        proxy = Proxy

    getVerificationKey :: SigningKey StakeKey -> VerificationKey StakeKey
    getVerificationKey (StakeSigningKey sk) =
        StakeVerificationKey (Sophie.VKey (Crypto.deriveVerKeyDSIGN sk))

    verificationKeyHash :: VerificationKey StakeKey -> Hash StakeKey
    verificationKeyHash (StakeVerificationKey vkey) =
        StakeKeyHash (Sophie.hashKey vkey)


instance SerialiseAsRawBytes (VerificationKey StakeKey) where
    serialiseToRawBytes (StakeVerificationKey (Sophie.VKey vk)) =
      Crypto.rawSerialiseVerKeyDSIGN vk

    deserialiseFromRawBytes (AsVerificationKey AsStakeKey) bs =
      StakeVerificationKey . Sophie.VKey <$>
        Crypto.rawDeserialiseVerKeyDSIGN bs

instance SerialiseAsRawBytes (SigningKey StakeKey) where
    serialiseToRawBytes (StakeSigningKey sk) =
      Crypto.rawSerialiseSignKeyDSIGN sk

    deserialiseFromRawBytes (AsSigningKey AsStakeKey) bs =
      StakeSigningKey <$> Crypto.rawDeserialiseSignKeyDSIGN bs

instance SerialiseAsBech32 (VerificationKey StakeKey) where
    bech32PrefixFor         _ =  "stake_vk"
    bech32PrefixesPermitted _ = ["stake_vk"]

instance SerialiseAsBech32 (SigningKey StakeKey) where
    bech32PrefixFor         _ =  "stake_sk"
    bech32PrefixesPermitted _ = ["stake_sk"]


newtype instance Hash StakeKey =
    StakeKeyHash (Sophie.KeyHash Sophie.Staking StandardCrypto)
  deriving stock (Eq, Ord)
  deriving (Show, IsString) via UsingRawBytesHex (Hash StakeKey)
  deriving (ToCBOR, FromCBOR) via UsingRawBytes (Hash StakeKey)
  deriving anyclass SerialiseAsCBOR

instance SerialiseAsRawBytes (Hash StakeKey) where
    serialiseToRawBytes (StakeKeyHash (Sophie.KeyHash vkh)) =
      Crypto.hashToBytes vkh

    deserialiseFromRawBytes (AsHash AsStakeKey) bs =
      StakeKeyHash . Sophie.KeyHash <$> Crypto.hashFromBytes bs

instance HasTextEnvelope (VerificationKey StakeKey) where
    textEnvelopeType _ = "StakeVerificationKeySophie_"
                      <> fromString (Crypto.algorithmNameDSIGN proxy)
      where
        proxy :: Proxy (Sophie.DSIGN StandardCrypto)
        proxy = Proxy

instance HasTextEnvelope (SigningKey StakeKey) where
    textEnvelopeType _ = "StakeSigningKeySophie_"
                      <> fromString (Crypto.algorithmNameDSIGN proxy)
      where
        proxy :: Proxy (Sophie.DSIGN StandardCrypto)
        proxy = Proxy


--
-- Sophie stake extended ed25519 keys
--

-- | Sophie-era stake keys using extended ed25519 cryptographic keys.
--
-- They can be used for Sophie stake addresses and witnessing transactions
-- that use stake addresses.
--
-- These extended keys are used by HD wallets. So this type provides
-- interoperability with HD wallets. The ITN CLI also supported this key type.
--
-- The extended verification keys can be converted (via 'castVerificationKey')
-- to ordinary keys (i.e. 'VerificationKey' 'StakeKey') but this is /not/ the
-- case for the signing keys. The signing keys can be used to witness
-- transactions directly, with verification via their non-extended verification
-- key ('VerificationKey' 'StakeKey').
--
-- This is a type level tag, used with other interfaces like 'Key'.
--
data StakeExtendedKey

instance HasTypeProxy StakeExtendedKey where
    data AsType StakeExtendedKey = AsStakeExtendedKey
    proxyToAsType _ = AsStakeExtendedKey

instance Key StakeExtendedKey where

    newtype VerificationKey StakeExtendedKey =
        StakeExtendedVerificationKey Crypto.HD.XPub
      deriving stock (Eq)
      deriving anyclass SerialiseAsCBOR
      deriving (Show, IsString) via UsingRawBytesHex (VerificationKey StakeExtendedKey)

    newtype SigningKey StakeExtendedKey =
        StakeExtendedSigningKey Crypto.HD.XPrv
      deriving anyclass SerialiseAsCBOR
      deriving (Show, IsString) via UsingRawBytesHex (SigningKey StakeExtendedKey)

    deterministicSigningKey :: AsType StakeExtendedKey
                            -> Crypto.Seed
                            -> SigningKey StakeExtendedKey
    deterministicSigningKey AsStakeExtendedKey seed =
        StakeExtendedSigningKey
          (Crypto.HD.generate seedbs BS.empty)
      where
       (seedbs, _) = Crypto.getBytesFromSeedT 32 seed

    deterministicSigningKeySeedSize :: AsType StakeExtendedKey -> Word
    deterministicSigningKeySeedSize AsStakeExtendedKey = 32

    getVerificationKey :: SigningKey StakeExtendedKey
                       -> VerificationKey StakeExtendedKey
    getVerificationKey (StakeExtendedSigningKey sk) =
        StakeExtendedVerificationKey (Crypto.HD.toXPub sk)

    -- | We use the hash of the normal non-extended pub key so that it is
    -- consistent with the one used in addresses and signatures.
    --
    verificationKeyHash :: VerificationKey StakeExtendedKey
                        -> Hash StakeExtendedKey
    verificationKeyHash (StakeExtendedVerificationKey vk) =
        StakeExtendedKeyHash
      . Sophie.KeyHash
      . Crypto.castHash
      $ Crypto.hashWith Crypto.HD.xpubPublicKey vk


instance ToCBOR (VerificationKey StakeExtendedKey) where
    toCBOR (StakeExtendedVerificationKey xpub) =
      toCBOR (Crypto.HD.unXPub xpub)

instance FromCBOR (VerificationKey StakeExtendedKey) where
    fromCBOR = do
      bs <- fromCBOR
      either fail (return . StakeExtendedVerificationKey)
             (Crypto.HD.xpub (bs :: ByteString))

instance ToCBOR (SigningKey StakeExtendedKey) where
    toCBOR (StakeExtendedSigningKey xprv) =
      toCBOR (Crypto.HD.unXPrv xprv)

instance FromCBOR (SigningKey StakeExtendedKey) where
    fromCBOR = do
      bs <- fromCBOR
      either fail (return . StakeExtendedSigningKey)
             (Crypto.HD.xprv (bs :: ByteString))

instance SerialiseAsRawBytes (VerificationKey StakeExtendedKey) where
    serialiseToRawBytes (StakeExtendedVerificationKey xpub) =
      Crypto.HD.unXPub xpub

    deserialiseFromRawBytes (AsVerificationKey AsStakeExtendedKey) bs =
      either (const Nothing) (Just . StakeExtendedVerificationKey)
             (Crypto.HD.xpub bs)

instance SerialiseAsRawBytes (SigningKey StakeExtendedKey) where
    serialiseToRawBytes (StakeExtendedSigningKey xprv) =
      Crypto.HD.unXPrv xprv

    deserialiseFromRawBytes (AsSigningKey AsStakeExtendedKey) bs =
      either (const Nothing) (Just . StakeExtendedSigningKey)
             (Crypto.HD.xprv bs)

instance SerialiseAsBech32 (VerificationKey StakeExtendedKey) where
    bech32PrefixFor         _ =  "stake_xvk"
    bech32PrefixesPermitted _ = ["stake_xvk"]

instance SerialiseAsBech32 (SigningKey StakeExtendedKey) where
    bech32PrefixFor         _ =  "stake_xsk"
    bech32PrefixesPermitted _ = ["stake_xsk"]


newtype instance Hash StakeExtendedKey =
    StakeExtendedKeyHash (Sophie.KeyHash Sophie.Staking StandardCrypto)
  deriving stock (Eq, Ord)
  deriving (Show, IsString) via UsingRawBytesHex (Hash StakeExtendedKey)
  deriving (ToCBOR, FromCBOR) via UsingRawBytes (Hash StakeExtendedKey)
  deriving anyclass SerialiseAsCBOR

instance SerialiseAsRawBytes (Hash StakeExtendedKey) where
    serialiseToRawBytes (StakeExtendedKeyHash (Sophie.KeyHash vkh)) =
      Crypto.hashToBytes vkh

    deserialiseFromRawBytes (AsHash AsStakeExtendedKey) bs =
      StakeExtendedKeyHash . Sophie.KeyHash <$> Crypto.hashFromBytes bs

instance HasTextEnvelope (VerificationKey StakeExtendedKey) where
    textEnvelopeType _ = "StakeExtendedVerificationKeySophie_ed25519_bip32"

instance HasTextEnvelope (SigningKey StakeExtendedKey) where
    textEnvelopeType _ = "StakeExtendedSigningKeySophie_ed25519_bip32"

instance CastVerificationKeyRole StakeExtendedKey StakeKey where
    castVerificationKey (StakeExtendedVerificationKey vk) =
        StakeVerificationKey
      . Sophie.VKey
      . fromMaybe impossible
      . Crypto.rawDeserialiseVerKeyDSIGN
      . Crypto.HD.xpubPublicKey
      $ vk
      where
        impossible =
          error "castVerificationKey: cole and sophie key sizes do not match!"


--
-- Genesis keys
--

data GenesisKey

instance HasTypeProxy GenesisKey where
    data AsType GenesisKey = AsGenesisKey
    proxyToAsType _ = AsGenesisKey

instance Key GenesisKey where

    newtype VerificationKey GenesisKey =
        GenesisVerificationKey (Sophie.VKey Sophie.Genesis StandardCrypto)
      deriving stock (Eq)
      deriving (Show, IsString) via UsingRawBytesHex (VerificationKey GenesisKey)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    newtype SigningKey GenesisKey =
        GenesisSigningKey (Sophie.SignKeyDSIGN StandardCrypto)
      deriving (Show, IsString) via UsingRawBytesHex (SigningKey GenesisKey)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    deterministicSigningKey :: AsType GenesisKey -> Crypto.Seed -> SigningKey GenesisKey
    deterministicSigningKey AsGenesisKey seed =
        GenesisSigningKey (Crypto.genKeyDSIGN seed)

    deterministicSigningKeySeedSize :: AsType GenesisKey -> Word
    deterministicSigningKeySeedSize AsGenesisKey =
        Crypto.seedSizeDSIGN proxy
      where
        proxy :: Proxy (Sophie.DSIGN StandardCrypto)
        proxy = Proxy

    getVerificationKey :: SigningKey GenesisKey -> VerificationKey GenesisKey
    getVerificationKey (GenesisSigningKey sk) =
        GenesisVerificationKey (Sophie.VKey (Crypto.deriveVerKeyDSIGN sk))

    verificationKeyHash :: VerificationKey GenesisKey -> Hash GenesisKey
    verificationKeyHash (GenesisVerificationKey vkey) =
        GenesisKeyHash (Sophie.hashKey vkey)


instance SerialiseAsRawBytes (VerificationKey GenesisKey) where
    serialiseToRawBytes (GenesisVerificationKey (Sophie.VKey vk)) =
      Crypto.rawSerialiseVerKeyDSIGN vk

    deserialiseFromRawBytes (AsVerificationKey AsGenesisKey) bs =
      GenesisVerificationKey . Sophie.VKey <$>
        Crypto.rawDeserialiseVerKeyDSIGN bs

instance SerialiseAsRawBytes (SigningKey GenesisKey) where
    serialiseToRawBytes (GenesisSigningKey sk) =
      Crypto.rawSerialiseSignKeyDSIGN sk

    deserialiseFromRawBytes (AsSigningKey AsGenesisKey) bs =
      GenesisSigningKey <$> Crypto.rawDeserialiseSignKeyDSIGN bs


newtype instance Hash GenesisKey =
    GenesisKeyHash (Sophie.KeyHash Sophie.Genesis StandardCrypto)
  deriving stock (Eq, Ord)
  deriving (Show, IsString) via UsingRawBytesHex (Hash GenesisKey)
  deriving (ToCBOR, FromCBOR) via UsingRawBytes (Hash GenesisKey)
  deriving anyclass SerialiseAsCBOR

instance SerialiseAsRawBytes (Hash GenesisKey) where
    serialiseToRawBytes (GenesisKeyHash (Sophie.KeyHash vkh)) =
      Crypto.hashToBytes vkh

    deserialiseFromRawBytes (AsHash AsGenesisKey) bs =
      GenesisKeyHash . Sophie.KeyHash <$> Crypto.hashFromBytes bs

instance HasTextEnvelope (VerificationKey GenesisKey) where
    textEnvelopeType _ = "GenesisVerificationKey_"
                      <> fromString (Crypto.algorithmNameDSIGN proxy)
      where
        proxy :: Proxy (Sophie.DSIGN StandardCrypto)
        proxy = Proxy

instance HasTextEnvelope (SigningKey GenesisKey) where
    textEnvelopeType _ = "GenesisSigningKey_"
                      <> fromString (Crypto.algorithmNameDSIGN proxy)
      where
        proxy :: Proxy (Sophie.DSIGN StandardCrypto)
        proxy = Proxy


--
-- Sophie vested extended ed25519 keys
--

-- | Sophie-era vested keys using extended ed25519 cryptographic keys.
--
-- These serve the same role as normal vested keys, but are here to support
-- legacy Cole vested keys which used extended keys.
--
-- The extended verification keys can be converted (via 'castVerificationKey')
-- to ordinary keys (i.e. 'VerificationKey' 'GenesisKey') but this is /not/ the
-- case for the signing keys. The signing keys can be used to witness
-- transactions directly, with verification via their non-extended verification
-- key ('VerificationKey' 'GenesisKey').
--
-- This is a type level tag, used with other interfaces like 'Key'.
--
data GenesisExtendedKey

instance HasTypeProxy GenesisExtendedKey where
    data AsType GenesisExtendedKey = AsGenesisExtendedKey
    proxyToAsType _ = AsGenesisExtendedKey

instance Key GenesisExtendedKey where

    newtype VerificationKey GenesisExtendedKey =
        GenesisExtendedVerificationKey Crypto.HD.XPub
      deriving stock (Eq)
      deriving anyclass SerialiseAsCBOR
      deriving (Show, IsString) via UsingRawBytesHex (VerificationKey GenesisExtendedKey)

    newtype SigningKey GenesisExtendedKey =
        GenesisExtendedSigningKey Crypto.HD.XPrv
      deriving anyclass SerialiseAsCBOR
      deriving (Show, IsString) via UsingRawBytesHex (SigningKey GenesisExtendedKey)

    deterministicSigningKey :: AsType GenesisExtendedKey
                            -> Crypto.Seed
                            -> SigningKey GenesisExtendedKey
    deterministicSigningKey AsGenesisExtendedKey seed =
        GenesisExtendedSigningKey
          (Crypto.HD.generate seedbs BS.empty)
      where
       (seedbs, _) = Crypto.getBytesFromSeedT 32 seed

    deterministicSigningKeySeedSize :: AsType GenesisExtendedKey -> Word
    deterministicSigningKeySeedSize AsGenesisExtendedKey = 32

    getVerificationKey :: SigningKey GenesisExtendedKey
                       -> VerificationKey GenesisExtendedKey
    getVerificationKey (GenesisExtendedSigningKey sk) =
        GenesisExtendedVerificationKey (Crypto.HD.toXPub sk)

    -- | We use the hash of the normal non-extended pub key so that it is
    -- consistent with the one used in addresses and signatures.
    --
    verificationKeyHash :: VerificationKey GenesisExtendedKey
                        -> Hash GenesisExtendedKey
    verificationKeyHash (GenesisExtendedVerificationKey vk) =
        GenesisExtendedKeyHash
      . Sophie.KeyHash
      . Crypto.castHash
      $ Crypto.hashWith Crypto.HD.xpubPublicKey vk


instance ToCBOR (VerificationKey GenesisExtendedKey) where
    toCBOR (GenesisExtendedVerificationKey xpub) =
      toCBOR (Crypto.HD.unXPub xpub)

instance FromCBOR (VerificationKey GenesisExtendedKey) where
    fromCBOR = do
      bs <- fromCBOR
      either fail (return . GenesisExtendedVerificationKey)
             (Crypto.HD.xpub (bs :: ByteString))

instance ToCBOR (SigningKey GenesisExtendedKey) where
    toCBOR (GenesisExtendedSigningKey xprv) =
      toCBOR (Crypto.HD.unXPrv xprv)

instance FromCBOR (SigningKey GenesisExtendedKey) where
    fromCBOR = do
      bs <- fromCBOR
      either fail (return . GenesisExtendedSigningKey)
             (Crypto.HD.xprv (bs :: ByteString))

instance SerialiseAsRawBytes (VerificationKey GenesisExtendedKey) where
    serialiseToRawBytes (GenesisExtendedVerificationKey xpub) =
      Crypto.HD.unXPub xpub

    deserialiseFromRawBytes (AsVerificationKey AsGenesisExtendedKey) bs =
      either (const Nothing) (Just . GenesisExtendedVerificationKey)
             (Crypto.HD.xpub bs)

instance SerialiseAsRawBytes (SigningKey GenesisExtendedKey) where
    serialiseToRawBytes (GenesisExtendedSigningKey xprv) =
      Crypto.HD.unXPrv xprv

    deserialiseFromRawBytes (AsSigningKey AsGenesisExtendedKey) bs =
      either (const Nothing) (Just . GenesisExtendedSigningKey)
             (Crypto.HD.xprv bs)


newtype instance Hash GenesisExtendedKey =
    GenesisExtendedKeyHash (Sophie.KeyHash Sophie.Staking StandardCrypto)
  deriving stock (Eq, Ord)
  deriving (Show, IsString) via UsingRawBytesHex (Hash GenesisExtendedKey)
  deriving (ToCBOR, FromCBOR) via UsingRawBytes (Hash GenesisExtendedKey)
  deriving anyclass SerialiseAsCBOR

instance SerialiseAsRawBytes (Hash GenesisExtendedKey) where
    serialiseToRawBytes (GenesisExtendedKeyHash (Sophie.KeyHash vkh)) =
      Crypto.hashToBytes vkh

    deserialiseFromRawBytes (AsHash AsGenesisExtendedKey) bs =
      GenesisExtendedKeyHash . Sophie.KeyHash <$> Crypto.hashFromBytes bs

instance HasTextEnvelope (VerificationKey GenesisExtendedKey) where
    textEnvelopeType _ = "GenesisExtendedVerificationKey_ed25519_bip32"

instance HasTextEnvelope (SigningKey GenesisExtendedKey) where
    textEnvelopeType _ = "GenesisExtendedSigningKey_ed25519_bip32"

instance CastVerificationKeyRole GenesisExtendedKey GenesisKey where
    castVerificationKey (GenesisExtendedVerificationKey vk) =
        GenesisVerificationKey
      . Sophie.VKey
      . fromMaybe impossible
      . Crypto.rawDeserialiseVerKeyDSIGN
      . Crypto.HD.xpubPublicKey
      $ vk
      where
        impossible =
          error "castVerificationKey: cole and sophie key sizes do not match!"


--
-- Genesis delegate keys
--

data GenesisDelegateKey

instance HasTypeProxy GenesisDelegateKey where
    data AsType GenesisDelegateKey = AsGenesisDelegateKey
    proxyToAsType _ = AsGenesisDelegateKey


instance Key GenesisDelegateKey where

    newtype VerificationKey GenesisDelegateKey =
        GenesisDelegateVerificationKey (Sophie.VKey Sophie.GenesisDelegate StandardCrypto)
      deriving stock (Eq)
      deriving (Show, IsString) via UsingRawBytesHex (VerificationKey GenesisDelegateKey)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    newtype SigningKey GenesisDelegateKey =
        GenesisDelegateSigningKey (Sophie.SignKeyDSIGN StandardCrypto)
      deriving (Show, IsString) via UsingRawBytesHex (SigningKey GenesisDelegateKey)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    deterministicSigningKey :: AsType GenesisDelegateKey -> Crypto.Seed -> SigningKey GenesisDelegateKey
    deterministicSigningKey AsGenesisDelegateKey seed =
        GenesisDelegateSigningKey (Crypto.genKeyDSIGN seed)

    deterministicSigningKeySeedSize :: AsType GenesisDelegateKey -> Word
    deterministicSigningKeySeedSize AsGenesisDelegateKey =
        Crypto.seedSizeDSIGN proxy
      where
        proxy :: Proxy (Sophie.DSIGN StandardCrypto)
        proxy = Proxy

    getVerificationKey :: SigningKey GenesisDelegateKey -> VerificationKey GenesisDelegateKey
    getVerificationKey (GenesisDelegateSigningKey sk) =
        GenesisDelegateVerificationKey (Sophie.VKey (Crypto.deriveVerKeyDSIGN sk))

    verificationKeyHash :: VerificationKey GenesisDelegateKey -> Hash GenesisDelegateKey
    verificationKeyHash (GenesisDelegateVerificationKey vkey) =
        GenesisDelegateKeyHash (Sophie.hashKey vkey)


instance SerialiseAsRawBytes (VerificationKey GenesisDelegateKey) where
    serialiseToRawBytes (GenesisDelegateVerificationKey (Sophie.VKey vk)) =
      Crypto.rawSerialiseVerKeyDSIGN vk

    deserialiseFromRawBytes (AsVerificationKey AsGenesisDelegateKey) bs =
      GenesisDelegateVerificationKey . Sophie.VKey <$>
        Crypto.rawDeserialiseVerKeyDSIGN bs

instance SerialiseAsRawBytes (SigningKey GenesisDelegateKey) where
    serialiseToRawBytes (GenesisDelegateSigningKey sk) =
      Crypto.rawSerialiseSignKeyDSIGN sk

    deserialiseFromRawBytes (AsSigningKey AsGenesisDelegateKey) bs =
      GenesisDelegateSigningKey <$> Crypto.rawDeserialiseSignKeyDSIGN bs


newtype instance Hash GenesisDelegateKey =
    GenesisDelegateKeyHash (Sophie.KeyHash Sophie.GenesisDelegate StandardCrypto)
  deriving stock (Eq, Ord)
  deriving (Show, IsString) via UsingRawBytesHex (Hash GenesisDelegateKey)
  deriving (ToCBOR, FromCBOR) via UsingRawBytes (Hash GenesisDelegateKey)
  deriving anyclass SerialiseAsCBOR

instance SerialiseAsRawBytes (Hash GenesisDelegateKey) where
    serialiseToRawBytes (GenesisDelegateKeyHash (Sophie.KeyHash vkh)) =
      Crypto.hashToBytes vkh

    deserialiseFromRawBytes (AsHash AsGenesisDelegateKey) bs =
      GenesisDelegateKeyHash . Sophie.KeyHash <$> Crypto.hashFromBytes bs

instance HasTextEnvelope (VerificationKey GenesisDelegateKey) where
    textEnvelopeType _ = "GenesisDelegateVerificationKey_"
                      <> fromString (Crypto.algorithmNameDSIGN proxy)
      where
        proxy :: Proxy (Sophie.DSIGN StandardCrypto)
        proxy = Proxy

instance HasTextEnvelope (SigningKey GenesisDelegateKey) where
    textEnvelopeType _ = "GenesisDelegateSigningKey_"
                      <> fromString (Crypto.algorithmNameDSIGN proxy)
      where
        proxy :: Proxy (Sophie.DSIGN StandardCrypto)
        proxy = Proxy

instance CastVerificationKeyRole GenesisDelegateKey StakePoolKey where
    castVerificationKey (GenesisDelegateVerificationKey (Sophie.VKey vkey)) =
      StakePoolVerificationKey (Sophie.VKey vkey)

instance CastSigningKeyRole GenesisDelegateKey StakePoolKey where
    castSigningKey (GenesisDelegateSigningKey skey) =
      StakePoolSigningKey skey


--
-- Sophie vested delegate extended ed25519 keys
--

-- | Sophie-era vested keys using extended ed25519 cryptographic keys.
--
-- These serve the same role as normal vested keys, but are here to support
-- legacy Cole vested keys which used extended keys.
--
-- The extended verification keys can be converted (via 'castVerificationKey')
-- to ordinary keys (i.e. 'VerificationKey' 'GenesisKey') but this is /not/ the
-- case for the signing keys. The signing keys can be used to witness
-- transactions directly, with verification via their non-extended verification
-- key ('VerificationKey' 'GenesisKey').
--
-- This is a type level tag, used with other interfaces like 'Key'.
--
data GenesisDelegateExtendedKey

instance HasTypeProxy GenesisDelegateExtendedKey where
    data AsType GenesisDelegateExtendedKey = AsGenesisDelegateExtendedKey
    proxyToAsType _ = AsGenesisDelegateExtendedKey

instance Key GenesisDelegateExtendedKey where

    newtype VerificationKey GenesisDelegateExtendedKey =
        GenesisDelegateExtendedVerificationKey Crypto.HD.XPub
      deriving stock (Eq)
      deriving anyclass SerialiseAsCBOR
      deriving (Show, IsString) via UsingRawBytesHex (VerificationKey GenesisDelegateExtendedKey)

    newtype SigningKey GenesisDelegateExtendedKey =
        GenesisDelegateExtendedSigningKey Crypto.HD.XPrv
      deriving anyclass SerialiseAsCBOR
      deriving (Show, IsString) via UsingRawBytesHex (SigningKey GenesisDelegateExtendedKey)

    deterministicSigningKey :: AsType GenesisDelegateExtendedKey
                            -> Crypto.Seed
                            -> SigningKey GenesisDelegateExtendedKey
    deterministicSigningKey AsGenesisDelegateExtendedKey seed =
        GenesisDelegateExtendedSigningKey
          (Crypto.HD.generate seedbs BS.empty)
      where
       (seedbs, _) = Crypto.getBytesFromSeedT 32 seed

    deterministicSigningKeySeedSize :: AsType GenesisDelegateExtendedKey -> Word
    deterministicSigningKeySeedSize AsGenesisDelegateExtendedKey = 32

    getVerificationKey :: SigningKey GenesisDelegateExtendedKey
                       -> VerificationKey GenesisDelegateExtendedKey
    getVerificationKey (GenesisDelegateExtendedSigningKey sk) =
        GenesisDelegateExtendedVerificationKey (Crypto.HD.toXPub sk)

    -- | We use the hash of the normal non-extended pub key so that it is
    -- consistent with the one used in addresses and signatures.
    --
    verificationKeyHash :: VerificationKey GenesisDelegateExtendedKey
                        -> Hash GenesisDelegateExtendedKey
    verificationKeyHash (GenesisDelegateExtendedVerificationKey vk) =
        GenesisDelegateExtendedKeyHash
      . Sophie.KeyHash
      . Crypto.castHash
      $ Crypto.hashWith Crypto.HD.xpubPublicKey vk


instance ToCBOR (VerificationKey GenesisDelegateExtendedKey) where
    toCBOR (GenesisDelegateExtendedVerificationKey xpub) =
      toCBOR (Crypto.HD.unXPub xpub)

instance FromCBOR (VerificationKey GenesisDelegateExtendedKey) where
    fromCBOR = do
      bs <- fromCBOR
      either fail (return . GenesisDelegateExtendedVerificationKey)
             (Crypto.HD.xpub (bs :: ByteString))

instance ToCBOR (SigningKey GenesisDelegateExtendedKey) where
    toCBOR (GenesisDelegateExtendedSigningKey xprv) =
      toCBOR (Crypto.HD.unXPrv xprv)

instance FromCBOR (SigningKey GenesisDelegateExtendedKey) where
    fromCBOR = do
      bs <- fromCBOR
      either fail (return . GenesisDelegateExtendedSigningKey)
             (Crypto.HD.xprv (bs :: ByteString))

instance SerialiseAsRawBytes (VerificationKey GenesisDelegateExtendedKey) where
    serialiseToRawBytes (GenesisDelegateExtendedVerificationKey xpub) =
      Crypto.HD.unXPub xpub

    deserialiseFromRawBytes (AsVerificationKey AsGenesisDelegateExtendedKey) bs =
      either (const Nothing) (Just . GenesisDelegateExtendedVerificationKey)
             (Crypto.HD.xpub bs)

instance SerialiseAsRawBytes (SigningKey GenesisDelegateExtendedKey) where
    serialiseToRawBytes (GenesisDelegateExtendedSigningKey xprv) =
      Crypto.HD.unXPrv xprv

    deserialiseFromRawBytes (AsSigningKey AsGenesisDelegateExtendedKey) bs =
      either (const Nothing) (Just . GenesisDelegateExtendedSigningKey)
             (Crypto.HD.xprv bs)


newtype instance Hash GenesisDelegateExtendedKey =
    GenesisDelegateExtendedKeyHash (Sophie.KeyHash Sophie.Staking StandardCrypto)
  deriving stock (Eq, Ord)
  deriving (Show, IsString) via UsingRawBytesHex (Hash GenesisDelegateExtendedKey)
  deriving (ToCBOR, FromCBOR) via UsingRawBytes (Hash GenesisDelegateExtendedKey)
  deriving anyclass SerialiseAsCBOR

instance SerialiseAsRawBytes (Hash GenesisDelegateExtendedKey) where
    serialiseToRawBytes (GenesisDelegateExtendedKeyHash (Sophie.KeyHash vkh)) =
      Crypto.hashToBytes vkh

    deserialiseFromRawBytes (AsHash AsGenesisDelegateExtendedKey) bs =
      GenesisDelegateExtendedKeyHash . Sophie.KeyHash <$> Crypto.hashFromBytes bs

instance HasTextEnvelope (VerificationKey GenesisDelegateExtendedKey) where
    textEnvelopeType _ = "GenesisDelegateExtendedVerificationKey_ed25519_bip32"

instance HasTextEnvelope (SigningKey GenesisDelegateExtendedKey) where
    textEnvelopeType _ = "GenesisDelegateExtendedSigningKey_ed25519_bip32"

instance CastVerificationKeyRole GenesisDelegateExtendedKey GenesisDelegateKey where
    castVerificationKey (GenesisDelegateExtendedVerificationKey vk) =
        GenesisDelegateVerificationKey
      . Sophie.VKey
      . fromMaybe impossible
      . Crypto.rawDeserialiseVerKeyDSIGN
      . Crypto.HD.xpubPublicKey
      $ vk
      where
        impossible =
          error "castVerificationKey: cole and sophie key sizes do not match!"


--
-- Genesis UTxO keys
--

data GenesisUTxOKey

instance HasTypeProxy GenesisUTxOKey where
    data AsType GenesisUTxOKey = AsGenesisUTxOKey
    proxyToAsType _ = AsGenesisUTxOKey


instance Key GenesisUTxOKey where

    newtype VerificationKey GenesisUTxOKey =
        GenesisUTxOVerificationKey (Sophie.VKey Sophie.Payment StandardCrypto)
      deriving stock (Eq)
      deriving (Show, IsString) via UsingRawBytesHex (VerificationKey GenesisUTxOKey)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    newtype SigningKey GenesisUTxOKey =
        GenesisUTxOSigningKey (Sophie.SignKeyDSIGN StandardCrypto)
      deriving (Show, IsString) via UsingRawBytesHex (SigningKey GenesisUTxOKey)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    deterministicSigningKey :: AsType GenesisUTxOKey -> Crypto.Seed -> SigningKey GenesisUTxOKey
    deterministicSigningKey AsGenesisUTxOKey seed =
        GenesisUTxOSigningKey (Crypto.genKeyDSIGN seed)

    deterministicSigningKeySeedSize :: AsType GenesisUTxOKey -> Word
    deterministicSigningKeySeedSize AsGenesisUTxOKey =
        Crypto.seedSizeDSIGN proxy
      where
        proxy :: Proxy (Sophie.DSIGN StandardCrypto)
        proxy = Proxy

    getVerificationKey :: SigningKey GenesisUTxOKey -> VerificationKey GenesisUTxOKey
    getVerificationKey (GenesisUTxOSigningKey sk) =
        GenesisUTxOVerificationKey (Sophie.VKey (Crypto.deriveVerKeyDSIGN sk))

    verificationKeyHash :: VerificationKey GenesisUTxOKey -> Hash GenesisUTxOKey
    verificationKeyHash (GenesisUTxOVerificationKey vkey) =
        GenesisUTxOKeyHash (Sophie.hashKey vkey)


instance SerialiseAsRawBytes (VerificationKey GenesisUTxOKey) where
    serialiseToRawBytes (GenesisUTxOVerificationKey (Sophie.VKey vk)) =
      Crypto.rawSerialiseVerKeyDSIGN vk

    deserialiseFromRawBytes (AsVerificationKey AsGenesisUTxOKey) bs =
      GenesisUTxOVerificationKey . Sophie.VKey <$>
        Crypto.rawDeserialiseVerKeyDSIGN bs

instance SerialiseAsRawBytes (SigningKey GenesisUTxOKey) where
    serialiseToRawBytes (GenesisUTxOSigningKey sk) =
      Crypto.rawSerialiseSignKeyDSIGN sk

    deserialiseFromRawBytes (AsSigningKey AsGenesisUTxOKey) bs =
      GenesisUTxOSigningKey <$> Crypto.rawDeserialiseSignKeyDSIGN bs


newtype instance Hash GenesisUTxOKey =
    GenesisUTxOKeyHash (Sophie.KeyHash Sophie.Payment StandardCrypto)
  deriving stock (Eq, Ord)
  deriving (Show, IsString) via UsingRawBytesHex (Hash GenesisUTxOKey)
  deriving (ToCBOR, FromCBOR) via UsingRawBytes (Hash GenesisUTxOKey)
  deriving anyclass SerialiseAsCBOR

instance SerialiseAsRawBytes (Hash GenesisUTxOKey) where
    serialiseToRawBytes (GenesisUTxOKeyHash (Sophie.KeyHash vkh)) =
      Crypto.hashToBytes vkh

    deserialiseFromRawBytes (AsHash AsGenesisUTxOKey) bs =
      GenesisUTxOKeyHash . Sophie.KeyHash <$> Crypto.hashFromBytes bs

instance HasTextEnvelope (VerificationKey GenesisUTxOKey) where
    textEnvelopeType _ = "GenesisUTxOVerificationKey_"
                      <> fromString (Crypto.algorithmNameDSIGN proxy)
      where
        proxy :: Proxy (Sophie.DSIGN StandardCrypto)
        proxy = Proxy

instance HasTextEnvelope (SigningKey GenesisUTxOKey) where
    textEnvelopeType _ = "GenesisUTxOSigningKey_"
                      <> fromString (Crypto.algorithmNameDSIGN proxy)
      where
        proxy :: Proxy (Sophie.DSIGN StandardCrypto)
        proxy = Proxy
    -- TODO: use a different type from the stake pool key, since some operations
    -- need a genesis key specifically

instance CastVerificationKeyRole GenesisUTxOKey PaymentKey where
    castVerificationKey (GenesisUTxOVerificationKey (Sophie.VKey vkey)) =
      PaymentVerificationKey (Sophie.VKey vkey)

instance CastSigningKeyRole GenesisUTxOKey PaymentKey where
    castSigningKey (GenesisUTxOSigningKey skey) =
      PaymentSigningKey skey


-- | Vested UTxO keys
data VestedUTxOKey

instance HasTypeProxy VestedUTxOKey where
    data AsType VestedUTxOKey = AsVestedUTxOKey
    proxyToAsType _ = AsVestedUTxOKey


instance Key VestedUTxOKey where

    newtype VerificationKey VestedUTxOKey =
        VestedUTxOVerificationKey (Sophie.VKey Sophie.Payment StandardCrypto)
      deriving stock (Eq)
      deriving (Show, IsString) via UsingRawBytesHex (VerificationKey VestedUTxOKey)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    newtype SigningKey VestedUTxOKey =
        VestedUTxOSigningKey (Sophie.SignKeyDSIGN StandardCrypto)
      deriving (Show, IsString) via UsingRawBytesHex (SigningKey VestedUTxOKey)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    deterministicSigningKey :: AsType VestedUTxOKey -> Crypto.Seed -> SigningKey VestedUTxOKey
    deterministicSigningKey AsVestedUTxOKey seed =
        VestedUTxOSigningKey (Crypto.genKeyDSIGN seed)

    deterministicSigningKeySeedSize :: AsType VestedUTxOKey -> Word
    deterministicSigningKeySeedSize AsVestedUTxOKey =
        Crypto.seedSizeDSIGN proxy
      where
        proxy :: Proxy (Sophie.DSIGN StandardCrypto)
        proxy = Proxy

    getVerificationKey :: SigningKey VestedUTxOKey -> VerificationKey VestedUTxOKey
    getVerificationKey (VestedUTxOSigningKey sk) =
        VestedUTxOVerificationKey (Sophie.VKey (Crypto.deriveVerKeyDSIGN sk))

    verificationKeyHash :: VerificationKey VestedUTxOKey -> Hash VestedUTxOKey
    verificationKeyHash (VestedUTxOVerificationKey vkey) =
        VestedUTxOKeyHash (Sophie.hashKey vkey)


instance SerialiseAsRawBytes (VerificationKey VestedUTxOKey) where
    serialiseToRawBytes (VestedUTxOVerificationKey (Sophie.VKey vk)) =
      Crypto.rawSerialiseVerKeyDSIGN vk

    deserialiseFromRawBytes (AsVerificationKey AsVestedUTxOKey) bs =
      VestedUTxOVerificationKey . Sophie.VKey <$>
        Crypto.rawDeserialiseVerKeyDSIGN bs

instance SerialiseAsRawBytes (SigningKey VestedUTxOKey) where
    serialiseToRawBytes (VestedUTxOSigningKey sk) =
      Crypto.rawSerialiseSignKeyDSIGN sk

    deserialiseFromRawBytes (AsSigningKey AsVestedUTxOKey) bs =
      VestedUTxOSigningKey <$> Crypto.rawDeserialiseSignKeyDSIGN bs


newtype instance Hash VestedUTxOKey =
    VestedUTxOKeyHash (Sophie.KeyHash Sophie.Payment StandardCrypto)
  deriving stock (Eq, Ord)
  deriving (Show, IsString) via UsingRawBytesHex (Hash VestedUTxOKey)
  deriving (ToCBOR, FromCBOR) via UsingRawBytes (Hash VestedUTxOKey)
  deriving anyclass SerialiseAsCBOR

instance SerialiseAsRawBytes (Hash VestedUTxOKey) where
    serialiseToRawBytes (VestedUTxOKeyHash (Sophie.KeyHash vkh)) =
      Crypto.hashToBytes vkh

    deserialiseFromRawBytes (AsHash AsVestedUTxOKey) bs =
      VestedUTxOKeyHash . Sophie.KeyHash <$> Crypto.hashFromBytes bs

instance HasTextEnvelope (VerificationKey VestedUTxOKey) where
    textEnvelopeType _ = "VestedUTxOVerificationKey_"
                      <> fromString (Crypto.algorithmNameDSIGN proxy)
      where
        proxy :: Proxy (Sophie.DSIGN StandardCrypto)
        proxy = Proxy

instance HasTextEnvelope (SigningKey VestedUTxOKey) where
    textEnvelopeType _ = "VestedUTxOSigningKey_"
                      <> fromString (Crypto.algorithmNameDSIGN proxy)
      where
        proxy :: Proxy (Sophie.DSIGN StandardCrypto)
        proxy = Proxy
    -- TODO: use a different type from the stake pool key, since some operations
    -- need a genesis key specifically

instance CastVerificationKeyRole VestedUTxOKey PaymentKey where
    castVerificationKey (VestedUTxOVerificationKey (Sophie.VKey vkey)) =
      PaymentVerificationKey (Sophie.VKey vkey)

instance CastSigningKeyRole VestedUTxOKey PaymentKey where
    castSigningKey (VestedUTxOSigningKey skey) =
      PaymentSigningKey skey


--
-- GenesisVestedKeys
-- 
data GenesisVestedKey

instance HasTypeProxy GenesisVestedKey where
    data AsType GenesisVestedKey = AsGenesisVestedKey
    proxyToAsType _ = AsGenesisVestedKey

instance Key GenesisVestedKey where

    newtype VerificationKey GenesisVestedKey =
        GenesisVestedVerificationKey (Sophie.VKey Sophie.Genesis StandardCrypto)
      deriving stock (Eq)
      deriving (Show, IsString) via UsingRawBytesHex (VerificationKey GenesisVestedKey)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    newtype SigningKey GenesisVestedKey =
        GenesisVestedSigningKey (Sophie.SignKeyDSIGN StandardCrypto)
      deriving (Show, IsString) via UsingRawBytesHex (SigningKey GenesisVestedKey)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    deterministicSigningKey :: AsType GenesisVestedKey -> Crypto.Seed -> SigningKey GenesisVestedKey
    deterministicSigningKey AsGenesisVestedKey seed =
        GenesisVestedSigningKey (Crypto.genKeyDSIGN seed)

    deterministicSigningKeySeedSize :: AsType GenesisVestedKey -> Word
    deterministicSigningKeySeedSize AsGenesisVestedKey =
        Crypto.seedSizeDSIGN proxy
      where
        proxy :: Proxy (Sophie.DSIGN StandardCrypto)
        proxy = Proxy

    getVerificationKey :: SigningKey GenesisVestedKey -> VerificationKey GenesisVestedKey
    getVerificationKey (GenesisVestedSigningKey sk) =
        GenesisVestedVerificationKey (Sophie.VKey (Crypto.deriveVerKeyDSIGN sk))

    verificationKeyHash :: VerificationKey GenesisVestedKey -> Hash GenesisVestedKey
    verificationKeyHash (GenesisVestedVerificationKey vkey) =
        GenesisVestedKeyHash (Sophie.hashKey vkey)


instance SerialiseAsRawBytes (VerificationKey GenesisVestedKey) where
    serialiseToRawBytes (GenesisVestedVerificationKey (Sophie.VKey vk)) =
      Crypto.rawSerialiseVerKeyDSIGN vk

    deserialiseFromRawBytes (AsVerificationKey AsGenesisVestedKey) bs =
      GenesisVestedVerificationKey . Sophie.VKey <$>
        Crypto.rawDeserialiseVerKeyDSIGN bs

instance SerialiseAsRawBytes (SigningKey GenesisVestedKey) where
    serialiseToRawBytes (GenesisVestedSigningKey sk) =
      Crypto.rawSerialiseSignKeyDSIGN sk

    deserialiseFromRawBytes (AsSigningKey AsGenesisVestedKey) bs =
      GenesisVestedSigningKey <$> Crypto.rawDeserialiseSignKeyDSIGN bs


newtype instance Hash GenesisVestedKey =
    GenesisVestedKeyHash (Sophie.KeyHash Sophie.Genesis StandardCrypto)
  deriving stock (Eq, Ord)
  deriving (Show, IsString) via UsingRawBytesHex (Hash GenesisVestedKey)
  deriving (ToCBOR, FromCBOR) via UsingRawBytes (Hash GenesisVestedKey)
  deriving anyclass SerialiseAsCBOR

instance SerialiseAsRawBytes (Hash GenesisVestedKey) where
    serialiseToRawBytes (GenesisVestedKeyHash (Sophie.KeyHash vkh)) =
      Crypto.hashToBytes vkh

    deserialiseFromRawBytes (AsHash AsGenesisVestedKey) bs =
      GenesisVestedKeyHash . Sophie.KeyHash <$> Crypto.hashFromBytes bs

instance HasTextEnvelope (VerificationKey GenesisVestedKey) where
    textEnvelopeType _ = "GenesisVestedVerificationKey_"
                      <> fromString (Crypto.algorithmNameDSIGN proxy)
      where
        proxy :: Proxy (Sophie.DSIGN StandardCrypto)
        proxy = Proxy

instance HasTextEnvelope (SigningKey GenesisVestedKey) where
    textEnvelopeType _ = "GenesisVestedSigningKey_"
                      <> fromString (Crypto.algorithmNameDSIGN proxy)
      where
        proxy :: Proxy (Sophie.DSIGN StandardCrypto)
        proxy = Proxy


--
-- Sophie vested extended ed25519 keys
--

-- | Sophie-era vested keys using extended ed25519 cryptographic keys.
--
-- These serve the same role as normal vested keys, but are here to support
-- legacy Cole vested keys which used extended keys.
--
-- The extended verification keys can be converted (via 'castVerificationKey')
-- to ordinary keys (i.e. 'VerificationKey' 'GenesisKey') but this is /not/ the
-- case for the signing keys. The signing keys can be used to witness
-- transactions directly, with verification via their non-extended verification
-- key ('VerificationKey' 'GenesisKey').
--
-- This is a type level tag, used with other interfaces like 'Key'.
--
data GenesisVestedExtendedKey

instance HasTypeProxy GenesisVestedExtendedKey where
    data AsType GenesisVestedExtendedKey = AsGenesisVestedExtendedKey
    proxyToAsType _ = AsGenesisVestedExtendedKey

instance Key GenesisVestedExtendedKey where

    newtype VerificationKey GenesisVestedExtendedKey =
        GenesisVestedExtendedVerificationKey Crypto.HD.XPub
      deriving stock (Eq)
      deriving anyclass SerialiseAsCBOR
      deriving (Show, IsString) via UsingRawBytesHex (VerificationKey GenesisVestedExtendedKey)

    newtype SigningKey GenesisVestedExtendedKey =
        GenesisVestedExtendedSigningKey Crypto.HD.XPrv
      deriving anyclass SerialiseAsCBOR
      deriving (Show, IsString) via UsingRawBytesHex (SigningKey GenesisVestedExtendedKey)

    deterministicSigningKey :: AsType GenesisVestedExtendedKey
                            -> Crypto.Seed
                            -> SigningKey GenesisVestedExtendedKey
    deterministicSigningKey AsGenesisVestedExtendedKey seed =
        GenesisVestedExtendedSigningKey
          (Crypto.HD.generate seedbs BS.empty)
      where
       (seedbs, _) = Crypto.getBytesFromSeedT 32 seed

    deterministicSigningKeySeedSize :: AsType GenesisVestedExtendedKey -> Word
    deterministicSigningKeySeedSize AsGenesisVestedExtendedKey = 32

    getVerificationKey :: SigningKey GenesisVestedExtendedKey
                       -> VerificationKey GenesisVestedExtendedKey
    getVerificationKey (GenesisVestedExtendedSigningKey sk) =
        GenesisVestedExtendedVerificationKey (Crypto.HD.toXPub sk)

    -- | We use the hash of the normal non-extended pub key so that it is
    -- consistent with the one used in addresses and signatures.
    --
    verificationKeyHash :: VerificationKey GenesisVestedExtendedKey
                        -> Hash GenesisVestedExtendedKey
    verificationKeyHash (GenesisVestedExtendedVerificationKey vk) =
        GenesisVestedExtendedKeyHash
      . Sophie.KeyHash
      . Crypto.castHash
      $ Crypto.hashWith Crypto.HD.xpubPublicKey vk


instance ToCBOR (VerificationKey GenesisVestedExtendedKey) where
    toCBOR (GenesisVestedExtendedVerificationKey xpub) =
      toCBOR (Crypto.HD.unXPub xpub)

instance FromCBOR (VerificationKey GenesisVestedExtendedKey) where
    fromCBOR = do
      bs <- fromCBOR
      either fail (return . GenesisVestedExtendedVerificationKey)
             (Crypto.HD.xpub (bs :: ByteString))

instance ToCBOR (SigningKey GenesisVestedExtendedKey) where
    toCBOR (GenesisVestedExtendedSigningKey xprv) =
      toCBOR (Crypto.HD.unXPrv xprv)

instance FromCBOR (SigningKey GenesisVestedExtendedKey) where
    fromCBOR = do
      bs <- fromCBOR
      either fail (return . GenesisVestedExtendedSigningKey)
             (Crypto.HD.xprv (bs :: ByteString))

instance SerialiseAsRawBytes (VerificationKey GenesisVestedExtendedKey) where
    serialiseToRawBytes (GenesisVestedExtendedVerificationKey xpub) =
      Crypto.HD.unXPub xpub

    deserialiseFromRawBytes (AsVerificationKey AsGenesisVestedExtendedKey) bs =
      either (const Nothing) (Just . GenesisVestedExtendedVerificationKey)
             (Crypto.HD.xpub bs)

instance SerialiseAsRawBytes (SigningKey GenesisVestedExtendedKey) where
    serialiseToRawBytes (GenesisVestedExtendedSigningKey xprv) =
      Crypto.HD.unXPrv xprv

    deserialiseFromRawBytes (AsSigningKey AsGenesisVestedExtendedKey) bs =
      either (const Nothing) (Just . GenesisVestedExtendedSigningKey)
             (Crypto.HD.xprv bs)


newtype instance Hash GenesisVestedExtendedKey =
    GenesisVestedExtendedKeyHash (Sophie.KeyHash Sophie.Staking StandardCrypto)
  deriving stock (Eq, Ord)
  deriving (Show, IsString) via UsingRawBytesHex (Hash GenesisVestedExtendedKey)
  deriving (ToCBOR, FromCBOR) via UsingRawBytes (Hash GenesisVestedExtendedKey)
  deriving anyclass SerialiseAsCBOR

instance SerialiseAsRawBytes (Hash GenesisVestedExtendedKey) where
    serialiseToRawBytes (GenesisVestedExtendedKeyHash (Sophie.KeyHash vkh)) =
      Crypto.hashToBytes vkh

    deserialiseFromRawBytes (AsHash AsGenesisVestedExtendedKey) bs =
      GenesisVestedExtendedKeyHash . Sophie.KeyHash <$> Crypto.hashFromBytes bs

instance HasTextEnvelope (VerificationKey GenesisVestedExtendedKey) where
    textEnvelopeType _ = "GenesisVestedExtendedVerificationKey_ed25519_bip32"

instance HasTextEnvelope (SigningKey GenesisVestedExtendedKey) where
    textEnvelopeType _ = "GenesisVestedExtendedSigningKey_ed25519_bip32"

instance CastVerificationKeyRole GenesisVestedExtendedKey GenesisVestedKey where
    castVerificationKey (GenesisVestedExtendedVerificationKey vk) =
        GenesisVestedVerificationKey
      . Sophie.VKey
      . fromMaybe impossible
      . Crypto.rawDeserialiseVerKeyDSIGN
      . Crypto.HD.xpubPublicKey
      $ vk
      where
        impossible =
          error "castVerificationKey: cole and sophie key sizes do not match!"


--
-- Genesis vested delegate keys
--

data GenesisVestedDelegateKey

instance HasTypeProxy GenesisVestedDelegateKey where
    data AsType GenesisVestedDelegateKey = AsGenesisVestedDelegateKey
    proxyToAsType _ = AsGenesisVestedDelegateKey


instance Key GenesisVestedDelegateKey where

    newtype VerificationKey GenesisVestedDelegateKey =
        GenesisVestedDelegateVerificationKey (Sophie.VKey Sophie.VestedDelegate StandardCrypto)
      deriving stock (Eq)
      deriving (Show, IsString) via UsingRawBytesHex (VerificationKey GenesisVestedDelegateKey)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    newtype SigningKey GenesisVestedDelegateKey =
        GenesisVestedDelegateSigningKey (Sophie.SignKeyDSIGN StandardCrypto)
      deriving (Show, IsString) via UsingRawBytesHex (SigningKey GenesisVestedDelegateKey)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    deterministicSigningKey :: AsType GenesisVestedDelegateKey -> Crypto.Seed -> SigningKey GenesisVestedDelegateKey
    deterministicSigningKey AsGenesisVestedDelegateKey seed =
        GenesisVestedDelegateSigningKey (Crypto.genKeyDSIGN seed)

    deterministicSigningKeySeedSize :: AsType GenesisVestedDelegateKey -> Word
    deterministicSigningKeySeedSize AsGenesisVestedDelegateKey =
        Crypto.seedSizeDSIGN proxy
      where
        proxy :: Proxy (Sophie.DSIGN StandardCrypto)
        proxy = Proxy

    getVerificationKey :: SigningKey GenesisVestedDelegateKey -> VerificationKey GenesisVestedDelegateKey
    getVerificationKey (GenesisVestedDelegateSigningKey sk) =
        GenesisVestedDelegateVerificationKey (Sophie.VKey (Crypto.deriveVerKeyDSIGN sk))

    verificationKeyHash :: VerificationKey GenesisVestedDelegateKey -> Hash GenesisVestedDelegateKey
    verificationKeyHash (GenesisVestedDelegateVerificationKey vkey) =
        GenesisVestedDelegateKeyHash (Sophie.hashKey vkey)


instance SerialiseAsRawBytes (VerificationKey GenesisVestedDelegateKey) where
    serialiseToRawBytes (GenesisVestedDelegateVerificationKey (Sophie.VKey vk)) =
      Crypto.rawSerialiseVerKeyDSIGN vk

    deserialiseFromRawBytes (AsVerificationKey AsGenesisVestedDelegateKey) bs =
      GenesisVestedDelegateVerificationKey . Sophie.VKey <$>
        Crypto.rawDeserialiseVerKeyDSIGN bs

instance SerialiseAsRawBytes (SigningKey GenesisVestedDelegateKey) where
    serialiseToRawBytes (GenesisVestedDelegateSigningKey sk) =
      Crypto.rawSerialiseSignKeyDSIGN sk

    deserialiseFromRawBytes (AsSigningKey AsGenesisVestedDelegateKey) bs =
      GenesisVestedDelegateSigningKey <$> Crypto.rawDeserialiseSignKeyDSIGN bs


newtype instance Hash GenesisVestedDelegateKey =
    GenesisVestedDelegateKeyHash (Sophie.KeyHash Sophie.VestedDelegate StandardCrypto)
  deriving stock (Eq, Ord)
  deriving (Show, IsString) via UsingRawBytesHex (Hash GenesisVestedDelegateKey)
  deriving (ToCBOR, FromCBOR) via UsingRawBytes (Hash GenesisVestedDelegateKey)
  deriving anyclass SerialiseAsCBOR

instance SerialiseAsRawBytes (Hash GenesisVestedDelegateKey) where
    serialiseToRawBytes (GenesisVestedDelegateKeyHash (Sophie.KeyHash vkh)) =
      Crypto.hashToBytes vkh

    deserialiseFromRawBytes (AsHash AsGenesisVestedDelegateKey) bs =
      GenesisVestedDelegateKeyHash . Sophie.KeyHash <$> Crypto.hashFromBytes bs

instance HasTextEnvelope (VerificationKey GenesisVestedDelegateKey) where
    textEnvelopeType _ = "GenesisVestedDelegateVerificationKey_"
                      <> fromString (Crypto.algorithmNameDSIGN proxy)
      where
        proxy :: Proxy (Sophie.DSIGN StandardCrypto)
        proxy = Proxy

instance HasTextEnvelope (SigningKey GenesisVestedDelegateKey) where
    textEnvelopeType _ = "GenesisVestedDelegateSigningKey_"
                      <> fromString (Crypto.algorithmNameDSIGN proxy)
      where
        proxy :: Proxy (Sophie.DSIGN StandardCrypto)
        proxy = Proxy

instance CastVerificationKeyRole GenesisVestedDelegateKey StakePoolKey where
    castVerificationKey (GenesisVestedDelegateVerificationKey (Sophie.VKey vkey)) =
      StakePoolVerificationKey (Sophie.VKey vkey)

instance CastSigningKeyRole GenesisVestedDelegateKey StakePoolKey where
    castSigningKey (GenesisVestedDelegateSigningKey skey) =
      StakePoolSigningKey skey


--
-- Sophie vested delegate extended ed25519 keys
--

-- | Sophie-era vested keys using extended ed25519 cryptographic keys.
--
-- These serve the same role as normal vested keys, but are here to support
-- legacy Cole vested keys which used extended keys.
--
-- The extended verification keys can be converted (via 'castVerificationKey')
-- to ordinary keys (i.e. 'VerificationKey' 'GenesisKey') but this is /not/ the
-- case for the signing keys. The signing keys can be used to witness
-- transactions directly, with verification via their non-extended verification
-- key ('VerificationKey' 'GenesisKey').
--
-- This is a type level tag, used with other interfaces like 'Key'.
--
data GenesisVestedDelegateExtendedKey

instance HasTypeProxy GenesisVestedDelegateExtendedKey where
    data AsType GenesisVestedDelegateExtendedKey = AsGenesisVestedDelegateExtendedKey
    proxyToAsType _ = AsGenesisVestedDelegateExtendedKey

instance Key GenesisVestedDelegateExtendedKey where

    newtype VerificationKey GenesisVestedDelegateExtendedKey =
        GenesisVestedDelegateExtendedVerificationKey Crypto.HD.XPub
      deriving stock (Eq)
      deriving anyclass SerialiseAsCBOR
      deriving (Show, IsString) via UsingRawBytesHex (VerificationKey GenesisVestedDelegateExtendedKey)

    newtype SigningKey GenesisVestedDelegateExtendedKey =
        GenesisVestedDelegateExtendedSigningKey Crypto.HD.XPrv
      deriving anyclass SerialiseAsCBOR
      deriving (Show, IsString) via UsingRawBytesHex (SigningKey GenesisVestedDelegateExtendedKey)

    deterministicSigningKey :: AsType GenesisVestedDelegateExtendedKey
                            -> Crypto.Seed
                            -> SigningKey GenesisVestedDelegateExtendedKey
    deterministicSigningKey AsGenesisVestedDelegateExtendedKey seed =
        GenesisVestedDelegateExtendedSigningKey
          (Crypto.HD.generate seedbs BS.empty)
      where
       (seedbs, _) = Crypto.getBytesFromSeedT 32 seed

    deterministicSigningKeySeedSize :: AsType GenesisVestedDelegateExtendedKey -> Word
    deterministicSigningKeySeedSize AsGenesisVestedDelegateExtendedKey = 32

    getVerificationKey :: SigningKey GenesisVestedDelegateExtendedKey
                       -> VerificationKey GenesisVestedDelegateExtendedKey
    getVerificationKey (GenesisVestedDelegateExtendedSigningKey sk) =
        GenesisVestedDelegateExtendedVerificationKey (Crypto.HD.toXPub sk)

    -- | We use the hash of the normal non-extended pub key so that it is
    -- consistent with the one used in addresses and signatures.
    --
    verificationKeyHash :: VerificationKey GenesisVestedDelegateExtendedKey
                        -> Hash GenesisVestedDelegateExtendedKey
    verificationKeyHash (GenesisVestedDelegateExtendedVerificationKey vk) =
        GenesisVestedDelegateExtendedKeyHash
      . Sophie.KeyHash
      . Crypto.castHash
      $ Crypto.hashWith Crypto.HD.xpubPublicKey vk


instance ToCBOR (VerificationKey GenesisVestedDelegateExtendedKey) where
    toCBOR (GenesisVestedDelegateExtendedVerificationKey xpub) =
      toCBOR (Crypto.HD.unXPub xpub)

instance FromCBOR (VerificationKey GenesisVestedDelegateExtendedKey) where
    fromCBOR = do
      bs <- fromCBOR
      either fail (return . GenesisVestedDelegateExtendedVerificationKey)
             (Crypto.HD.xpub (bs :: ByteString))

instance ToCBOR (SigningKey GenesisVestedDelegateExtendedKey) where
    toCBOR (GenesisVestedDelegateExtendedSigningKey xprv) =
      toCBOR (Crypto.HD.unXPrv xprv)

instance FromCBOR (SigningKey GenesisVestedDelegateExtendedKey) where
    fromCBOR = do
      bs <- fromCBOR
      either fail (return . GenesisVestedDelegateExtendedSigningKey)
             (Crypto.HD.xprv (bs :: ByteString))

instance SerialiseAsRawBytes (VerificationKey GenesisVestedDelegateExtendedKey) where
    serialiseToRawBytes (GenesisVestedDelegateExtendedVerificationKey xpub) =
      Crypto.HD.unXPub xpub

    deserialiseFromRawBytes (AsVerificationKey AsGenesisVestedDelegateExtendedKey) bs =
      either (const Nothing) (Just . GenesisVestedDelegateExtendedVerificationKey)
             (Crypto.HD.xpub bs)

instance SerialiseAsRawBytes (SigningKey GenesisVestedDelegateExtendedKey) where
    serialiseToRawBytes (GenesisVestedDelegateExtendedSigningKey xprv) =
      Crypto.HD.unXPrv xprv

    deserialiseFromRawBytes (AsSigningKey AsGenesisVestedDelegateExtendedKey) bs =
      either (const Nothing) (Just . GenesisVestedDelegateExtendedSigningKey)
             (Crypto.HD.xprv bs)


newtype instance Hash GenesisVestedDelegateExtendedKey =
    GenesisVestedDelegateExtendedKeyHash (Sophie.KeyHash Sophie.Staking StandardCrypto)
  deriving stock (Eq, Ord)
  deriving (Show, IsString) via UsingRawBytesHex (Hash GenesisVestedDelegateExtendedKey)
  deriving (ToCBOR, FromCBOR) via UsingRawBytes (Hash GenesisVestedDelegateExtendedKey)
  deriving anyclass SerialiseAsCBOR

instance SerialiseAsRawBytes (Hash GenesisVestedDelegateExtendedKey) where
    serialiseToRawBytes (GenesisVestedDelegateExtendedKeyHash (Sophie.KeyHash vkh)) =
      Crypto.hashToBytes vkh

    deserialiseFromRawBytes (AsHash AsGenesisVestedDelegateExtendedKey) bs =
      GenesisVestedDelegateExtendedKeyHash . Sophie.KeyHash <$> Crypto.hashFromBytes bs

instance HasTextEnvelope (VerificationKey GenesisVestedDelegateExtendedKey) where
    textEnvelopeType _ = "GenesisVestedDelegateExtendedVerificationKey_ed25519_bip32"

instance HasTextEnvelope (SigningKey GenesisVestedDelegateExtendedKey) where
    textEnvelopeType _ = "GenesisVestedDelegateExtendedSigningKey_ed25519_bip32"

instance CastVerificationKeyRole GenesisVestedDelegateExtendedKey GenesisVestedDelegateKey where
    castVerificationKey (GenesisVestedDelegateExtendedVerificationKey vk) =
        GenesisVestedDelegateVerificationKey
      . Sophie.VKey
      . fromMaybe impossible
      . Crypto.rawDeserialiseVerKeyDSIGN
      . Crypto.HD.xpubPublicKey
      $ vk
      where
        impossible =
          error "castVerificationKey: cole and sophie key sizes do not match!"

-- Vested keys
--

data VestedKey

instance HasTypeProxy VestedKey where
    data AsType VestedKey = AsVestedKey
    proxyToAsType _ = AsVestedKey

instance Key VestedKey where

    newtype VerificationKey VestedKey =
        VestedVerificationKey (Sophie.VKey Sophie.Vested StandardCrypto)
      deriving stock (Eq)
      deriving (Show, IsString) via UsingRawBytesHex (VerificationKey VestedKey)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    newtype SigningKey VestedKey =
        VestedSigningKey (Sophie.SignKeyDSIGN StandardCrypto)
      deriving (Show, IsString) via UsingRawBytesHex (SigningKey VestedKey)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    deterministicSigningKey :: AsType VestedKey -> Crypto.Seed -> SigningKey VestedKey
    deterministicSigningKey AsVestedKey seed =
        VestedSigningKey (Crypto.genKeyDSIGN seed)

    deterministicSigningKeySeedSize :: AsType VestedKey -> Word
    deterministicSigningKeySeedSize AsVestedKey =
        Crypto.seedSizeDSIGN proxy
      where
        proxy :: Proxy (Sophie.DSIGN StandardCrypto)
        proxy = Proxy

    getVerificationKey :: SigningKey VestedKey -> VerificationKey VestedKey
    getVerificationKey (VestedSigningKey sk) =
        VestedVerificationKey (Sophie.VKey (Crypto.deriveVerKeyDSIGN sk))

    verificationKeyHash :: VerificationKey VestedKey -> Hash VestedKey
    verificationKeyHash (VestedVerificationKey vkey) =
        VestedKeyHash (Sophie.hashKey vkey)


instance SerialiseAsRawBytes (VerificationKey VestedKey) where
    serialiseToRawBytes (VestedVerificationKey (Sophie.VKey vk)) =
      Crypto.rawSerialiseVerKeyDSIGN vk

    deserialiseFromRawBytes (AsVerificationKey AsVestedKey) bs =
      VestedVerificationKey . Sophie.VKey <$>
        Crypto.rawDeserialiseVerKeyDSIGN bs

instance SerialiseAsRawBytes (SigningKey VestedKey) where
    serialiseToRawBytes (VestedSigningKey sk) =
      Crypto.rawSerialiseSignKeyDSIGN sk

    deserialiseFromRawBytes (AsSigningKey AsVestedKey) bs =
      VestedSigningKey <$> Crypto.rawDeserialiseSignKeyDSIGN bs


newtype instance Hash VestedKey =
    VestedKeyHash (Sophie.KeyHash Sophie.Vested StandardCrypto)
  deriving stock (Eq, Ord)
  deriving (Show, IsString) via UsingRawBytesHex (Hash VestedKey)
  deriving (ToCBOR, FromCBOR) via UsingRawBytes (Hash VestedKey)
  deriving anyclass SerialiseAsCBOR

instance SerialiseAsRawBytes (Hash VestedKey) where
    serialiseToRawBytes (VestedKeyHash (Sophie.KeyHash vkh)) =
      Crypto.hashToBytes vkh

    deserialiseFromRawBytes (AsHash AsVestedKey) bs =
      VestedKeyHash . Sophie.KeyHash <$> Crypto.hashFromBytes bs

instance HasTextEnvelope (VerificationKey VestedKey) where
    textEnvelopeType _ = "VestedVerificationKey_"
                      <> fromString (Crypto.algorithmNameDSIGN proxy)
      where
        proxy :: Proxy (Sophie.DSIGN StandardCrypto)
        proxy = Proxy

instance HasTextEnvelope (SigningKey VestedKey) where
    textEnvelopeType _ = "VestedSigningKey_"
                      <> fromString (Crypto.algorithmNameDSIGN proxy)
      where
        proxy :: Proxy (Sophie.DSIGN StandardCrypto)
        proxy = Proxy


--
-- Sophie vested extended ed25519 keys
--

-- | Sophie-era vested keys using extended ed25519 cryptographic keys.
--
-- These serve the same role as normal vested keys, but are here to support
-- legacy Cole vested keys which used extended keys.
--
-- The extended verification keys can be converted (via 'castVerificationKey')
-- to ordinary keys (i.e. 'VerificationKey' 'VestedKey') but this is /not/ the
-- case for the signing keys. The signing keys can be used to witness
-- transactions directly, with verification via their non-extended verification
-- key ('VerificationKey' 'VestedKey').
--
-- This is a type level tag, used with other interfaces like 'Key'.
--
data VestedExtendedKey

instance HasTypeProxy VestedExtendedKey where
    data AsType VestedExtendedKey = AsVestedExtendedKey
    proxyToAsType _ = AsVestedExtendedKey

instance Key VestedExtendedKey where

    newtype VerificationKey VestedExtendedKey =
        VestedExtendedVerificationKey Crypto.HD.XPub
      deriving stock (Eq)
      deriving anyclass SerialiseAsCBOR
      deriving (Show, IsString) via UsingRawBytesHex (VerificationKey VestedExtendedKey)

    newtype SigningKey VestedExtendedKey =
        VestedExtendedSigningKey Crypto.HD.XPrv
      deriving anyclass SerialiseAsCBOR
      deriving (Show, IsString) via UsingRawBytesHex (SigningKey VestedExtendedKey)

    deterministicSigningKey :: AsType VestedExtendedKey
                            -> Crypto.Seed
                            -> SigningKey VestedExtendedKey
    deterministicSigningKey AsVestedExtendedKey seed =
        VestedExtendedSigningKey
          (Crypto.HD.generate seedbs BS.empty)
      where
       (seedbs, _) = Crypto.getBytesFromSeedT 32 seed

    deterministicSigningKeySeedSize :: AsType VestedExtendedKey -> Word
    deterministicSigningKeySeedSize AsVestedExtendedKey = 32

    getVerificationKey :: SigningKey VestedExtendedKey
                       -> VerificationKey VestedExtendedKey
    getVerificationKey (VestedExtendedSigningKey sk) =
        VestedExtendedVerificationKey (Crypto.HD.toXPub sk)

    -- | We use the hash of the normal non-extended pub key so that it is
    -- consistent with the one used in addresses and signatures.
    --
    verificationKeyHash :: VerificationKey VestedExtendedKey
                        -> Hash VestedExtendedKey
    verificationKeyHash (VestedExtendedVerificationKey vk) =
        VestedExtendedKeyHash
      . Sophie.KeyHash
      . Crypto.castHash
      $ Crypto.hashWith Crypto.HD.xpubPublicKey vk


instance ToCBOR (VerificationKey VestedExtendedKey) where
    toCBOR (VestedExtendedVerificationKey xpub) =
      toCBOR (Crypto.HD.unXPub xpub)

instance FromCBOR (VerificationKey VestedExtendedKey) where
    fromCBOR = do
      bs <- fromCBOR
      either fail (return . VestedExtendedVerificationKey)
             (Crypto.HD.xpub (bs :: ByteString))

instance ToCBOR (SigningKey VestedExtendedKey) where
    toCBOR (VestedExtendedSigningKey xprv) =
      toCBOR (Crypto.HD.unXPrv xprv)

instance FromCBOR (SigningKey VestedExtendedKey) where
    fromCBOR = do
      bs <- fromCBOR
      either fail (return . VestedExtendedSigningKey)
             (Crypto.HD.xprv (bs :: ByteString))

instance SerialiseAsRawBytes (VerificationKey VestedExtendedKey) where
    serialiseToRawBytes (VestedExtendedVerificationKey xpub) =
      Crypto.HD.unXPub xpub

    deserialiseFromRawBytes (AsVerificationKey AsVestedExtendedKey) bs =
      either (const Nothing) (Just . VestedExtendedVerificationKey)
             (Crypto.HD.xpub bs)

instance SerialiseAsRawBytes (SigningKey VestedExtendedKey) where
    serialiseToRawBytes (VestedExtendedSigningKey xprv) =
      Crypto.HD.unXPrv xprv

    deserialiseFromRawBytes (AsSigningKey AsVestedExtendedKey) bs =
      either (const Nothing) (Just . VestedExtendedSigningKey)
             (Crypto.HD.xprv bs)


newtype instance Hash VestedExtendedKey =
    VestedExtendedKeyHash (Sophie.KeyHash Sophie.Staking StandardCrypto)
  deriving stock (Eq, Ord)
  deriving (Show, IsString) via UsingRawBytesHex (Hash VestedExtendedKey)
  deriving (ToCBOR, FromCBOR) via UsingRawBytes (Hash VestedExtendedKey)
  deriving anyclass SerialiseAsCBOR

instance SerialiseAsRawBytes (Hash VestedExtendedKey) where
    serialiseToRawBytes (VestedExtendedKeyHash (Sophie.KeyHash vkh)) =
      Crypto.hashToBytes vkh

    deserialiseFromRawBytes (AsHash AsVestedExtendedKey) bs =
      VestedExtendedKeyHash . Sophie.KeyHash <$> Crypto.hashFromBytes bs

instance HasTextEnvelope (VerificationKey VestedExtendedKey) where
    textEnvelopeType _ = "VestedExtendedVerificationKey_ed25519_bip32"

instance HasTextEnvelope (SigningKey VestedExtendedKey) where
    textEnvelopeType _ = "VestedExtendedSigningKey_ed25519_bip32"

instance CastVerificationKeyRole VestedExtendedKey VestedKey where
    castVerificationKey (VestedExtendedVerificationKey vk) =
        VestedVerificationKey
      . Sophie.VKey
      . fromMaybe impossible
      . Crypto.rawDeserialiseVerKeyDSIGN
      . Crypto.HD.xpubPublicKey
      $ vk
      where
        impossible =
          error "castVerificationKey: cole and sophie key sizes do not match!"


--
-- Vested delegate keys
--

data VestedDelegateKey

instance HasTypeProxy VestedDelegateKey where
    data AsType VestedDelegateKey = AsVestedDelegateKey
    proxyToAsType _ = AsVestedDelegateKey


instance Key VestedDelegateKey where

    newtype VerificationKey VestedDelegateKey =
        VestedDelegateVerificationKey (Sophie.VKey Sophie.VestedDelegate StandardCrypto)
      deriving stock (Eq)
      deriving (Show, IsString) via UsingRawBytesHex (VerificationKey VestedDelegateKey)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    newtype SigningKey VestedDelegateKey =
        VestedDelegateSigningKey (Sophie.SignKeyDSIGN StandardCrypto)
      deriving (Show, IsString) via UsingRawBytesHex (SigningKey VestedDelegateKey)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    deterministicSigningKey :: AsType VestedDelegateKey -> Crypto.Seed -> SigningKey VestedDelegateKey
    deterministicSigningKey AsVestedDelegateKey seed =
        VestedDelegateSigningKey (Crypto.genKeyDSIGN seed)

    deterministicSigningKeySeedSize :: AsType VestedDelegateKey -> Word
    deterministicSigningKeySeedSize AsVestedDelegateKey =
        Crypto.seedSizeDSIGN proxy
      where
        proxy :: Proxy (Sophie.DSIGN StandardCrypto)
        proxy = Proxy

    getVerificationKey :: SigningKey VestedDelegateKey -> VerificationKey VestedDelegateKey
    getVerificationKey (VestedDelegateSigningKey sk) =
        VestedDelegateVerificationKey (Sophie.VKey (Crypto.deriveVerKeyDSIGN sk))

    verificationKeyHash :: VerificationKey VestedDelegateKey -> Hash VestedDelegateKey
    verificationKeyHash (VestedDelegateVerificationKey vkey) =
        VestedDelegateKeyHash (Sophie.hashKey vkey)


instance SerialiseAsRawBytes (VerificationKey VestedDelegateKey) where
    serialiseToRawBytes (VestedDelegateVerificationKey (Sophie.VKey vk)) =
      Crypto.rawSerialiseVerKeyDSIGN vk

    deserialiseFromRawBytes (AsVerificationKey AsVestedDelegateKey) bs =
      VestedDelegateVerificationKey . Sophie.VKey <$>
        Crypto.rawDeserialiseVerKeyDSIGN bs

instance SerialiseAsRawBytes (SigningKey VestedDelegateKey) where
    serialiseToRawBytes (VestedDelegateSigningKey sk) =
      Crypto.rawSerialiseSignKeyDSIGN sk

    deserialiseFromRawBytes (AsSigningKey AsVestedDelegateKey) bs =
      VestedDelegateSigningKey <$> Crypto.rawDeserialiseSignKeyDSIGN bs


newtype instance Hash VestedDelegateKey =
    VestedDelegateKeyHash (Sophie.KeyHash Sophie.VestedDelegate StandardCrypto)
  deriving stock (Eq, Ord)
  deriving (Show, IsString) via UsingRawBytesHex (Hash VestedDelegateKey)
  deriving (ToCBOR, FromCBOR) via UsingRawBytes (Hash VestedDelegateKey)
  deriving anyclass SerialiseAsCBOR

instance SerialiseAsRawBytes (Hash VestedDelegateKey) where
    serialiseToRawBytes (VestedDelegateKeyHash (Sophie.KeyHash vkh)) =
      Crypto.hashToBytes vkh

    deserialiseFromRawBytes (AsHash AsVestedDelegateKey) bs =
      VestedDelegateKeyHash . Sophie.KeyHash <$> Crypto.hashFromBytes bs

instance HasTextEnvelope (VerificationKey VestedDelegateKey) where
    textEnvelopeType _ = "VestedDelegateVerificationKey_"
                      <> fromString (Crypto.algorithmNameDSIGN proxy)
      where
        proxy :: Proxy (Sophie.DSIGN StandardCrypto)
        proxy = Proxy

instance HasTextEnvelope (SigningKey VestedDelegateKey) where
    textEnvelopeType _ = "VestedDelegateSigningKey_"
                      <> fromString (Crypto.algorithmNameDSIGN proxy)
      where
        proxy :: Proxy (Sophie.DSIGN StandardCrypto)
        proxy = Proxy

instance CastVerificationKeyRole VestedDelegateKey StakePoolKey where
    castVerificationKey (VestedDelegateVerificationKey (Sophie.VKey vkey)) =
      StakePoolVerificationKey (Sophie.VKey vkey)

instance CastSigningKeyRole VestedDelegateKey StakePoolKey where
    castSigningKey (VestedDelegateSigningKey skey) =
      StakePoolSigningKey skey


--
-- Sophie vested delegate extended ed25519 keys
--

-- | Sophie-era vested keys using extended ed25519 cryptographic keys.
--
-- These serve the same role as normal vested keys, but are here to support
-- legacy Cole vested keys which used extended keys.
--
-- The extended verification keys can be converted (via 'castVerificationKey')
-- to ordinary keys (i.e. 'VerificationKey' 'VestedKey') but this is /not/ the
-- case for the signing keys. The signing keys can be used to witness
-- transactions directly, with verification via their non-extended verification
-- key ('VerificationKey' 'VestedKey').
--
-- This is a type level tag, used with other interfaces like 'Key'.
--
data VestedDelegateExtendedKey

instance HasTypeProxy VestedDelegateExtendedKey where
    data AsType VestedDelegateExtendedKey = AsVestedDelegateExtendedKey
    proxyToAsType _ = AsVestedDelegateExtendedKey

instance Key VestedDelegateExtendedKey where

    newtype VerificationKey VestedDelegateExtendedKey =
        VestedDelegateExtendedVerificationKey Crypto.HD.XPub
      deriving stock (Eq)
      deriving anyclass SerialiseAsCBOR
      deriving (Show, IsString) via UsingRawBytesHex (VerificationKey VestedDelegateExtendedKey)

    newtype SigningKey VestedDelegateExtendedKey =
        VestedDelegateExtendedSigningKey Crypto.HD.XPrv
      deriving anyclass SerialiseAsCBOR
      deriving (Show, IsString) via UsingRawBytesHex (SigningKey VestedDelegateExtendedKey)

    deterministicSigningKey :: AsType VestedDelegateExtendedKey
                            -> Crypto.Seed
                            -> SigningKey VestedDelegateExtendedKey
    deterministicSigningKey AsVestedDelegateExtendedKey seed =
        VestedDelegateExtendedSigningKey
          (Crypto.HD.generate seedbs BS.empty)
      where
       (seedbs, _) = Crypto.getBytesFromSeedT 32 seed

    deterministicSigningKeySeedSize :: AsType VestedDelegateExtendedKey -> Word
    deterministicSigningKeySeedSize AsVestedDelegateExtendedKey = 32

    getVerificationKey :: SigningKey VestedDelegateExtendedKey
                       -> VerificationKey VestedDelegateExtendedKey
    getVerificationKey (VestedDelegateExtendedSigningKey sk) =
        VestedDelegateExtendedVerificationKey (Crypto.HD.toXPub sk)

    -- | We use the hash of the normal non-extended pub key so that it is
    -- consistent with the one used in addresses and signatures.
    --
    verificationKeyHash :: VerificationKey VestedDelegateExtendedKey
                        -> Hash VestedDelegateExtendedKey
    verificationKeyHash (VestedDelegateExtendedVerificationKey vk) =
        VestedDelegateExtendedKeyHash
      . Sophie.KeyHash
      . Crypto.castHash
      $ Crypto.hashWith Crypto.HD.xpubPublicKey vk


instance ToCBOR (VerificationKey VestedDelegateExtendedKey) where
    toCBOR (VestedDelegateExtendedVerificationKey xpub) =
      toCBOR (Crypto.HD.unXPub xpub)

instance FromCBOR (VerificationKey VestedDelegateExtendedKey) where
    fromCBOR = do
      bs <- fromCBOR
      either fail (return . VestedDelegateExtendedVerificationKey)
             (Crypto.HD.xpub (bs :: ByteString))

instance ToCBOR (SigningKey VestedDelegateExtendedKey) where
    toCBOR (VestedDelegateExtendedSigningKey xprv) =
      toCBOR (Crypto.HD.unXPrv xprv)

instance FromCBOR (SigningKey VestedDelegateExtendedKey) where
    fromCBOR = do
      bs <- fromCBOR
      either fail (return . VestedDelegateExtendedSigningKey)
             (Crypto.HD.xprv (bs :: ByteString))

instance SerialiseAsRawBytes (VerificationKey VestedDelegateExtendedKey) where
    serialiseToRawBytes (VestedDelegateExtendedVerificationKey xpub) =
      Crypto.HD.unXPub xpub

    deserialiseFromRawBytes (AsVerificationKey AsVestedDelegateExtendedKey) bs =
      either (const Nothing) (Just . VestedDelegateExtendedVerificationKey)
             (Crypto.HD.xpub bs)

instance SerialiseAsRawBytes (SigningKey VestedDelegateExtendedKey) where
    serialiseToRawBytes (VestedDelegateExtendedSigningKey xprv) =
      Crypto.HD.unXPrv xprv

    deserialiseFromRawBytes (AsSigningKey AsVestedDelegateExtendedKey) bs =
      either (const Nothing) (Just . VestedDelegateExtendedSigningKey)
             (Crypto.HD.xprv bs)


newtype instance Hash VestedDelegateExtendedKey =
    VestedDelegateExtendedKeyHash (Sophie.KeyHash Sophie.Staking StandardCrypto)
  deriving stock (Eq, Ord)
  deriving (Show, IsString) via UsingRawBytesHex (Hash VestedDelegateExtendedKey)
  deriving (ToCBOR, FromCBOR) via UsingRawBytes (Hash VestedDelegateExtendedKey)
  deriving anyclass SerialiseAsCBOR

instance SerialiseAsRawBytes (Hash VestedDelegateExtendedKey) where
    serialiseToRawBytes (VestedDelegateExtendedKeyHash (Sophie.KeyHash vkh)) =
      Crypto.hashToBytes vkh

    deserialiseFromRawBytes (AsHash AsVestedDelegateExtendedKey) bs =
      VestedDelegateExtendedKeyHash . Sophie.KeyHash <$> Crypto.hashFromBytes bs

instance HasTextEnvelope (VerificationKey VestedDelegateExtendedKey) where
    textEnvelopeType _ = "VestedDelegateExtendedVerificationKey_ed25519_bip32"

instance HasTextEnvelope (SigningKey VestedDelegateExtendedKey) where
    textEnvelopeType _ = "VestedDelegateExtendedSigningKey_ed25519_bip32"

instance CastVerificationKeyRole VestedDelegateExtendedKey VestedDelegateKey where
    castVerificationKey (VestedDelegateExtendedVerificationKey vk) =
        VestedDelegateVerificationKey
      . Sophie.VKey
      . fromMaybe impossible
      . Crypto.rawDeserialiseVerKeyDSIGN
      . Crypto.HD.xpubPublicKey
      $ vk
      where
        impossible =
          error "castVerificationKey: cole and sophie key sizes do not match!"

--
-- stake pool keys
--

data StakePoolKey

instance HasTypeProxy StakePoolKey where
    data AsType StakePoolKey = AsStakePoolKey
    proxyToAsType _ = AsStakePoolKey

instance Key StakePoolKey where

    newtype VerificationKey StakePoolKey =
        StakePoolVerificationKey (Sophie.VKey Sophie.StakePool StandardCrypto)
      deriving stock (Eq)
      deriving (Show, IsString) via UsingRawBytesHex (VerificationKey StakePoolKey)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    newtype SigningKey StakePoolKey =
        StakePoolSigningKey (Sophie.SignKeyDSIGN StandardCrypto)
      deriving (Show, IsString) via UsingRawBytesHex (SigningKey StakePoolKey)
      deriving newtype (ToCBOR, FromCBOR)
      deriving anyclass SerialiseAsCBOR

    deterministicSigningKey :: AsType StakePoolKey -> Crypto.Seed -> SigningKey StakePoolKey
    deterministicSigningKey AsStakePoolKey seed =
        StakePoolSigningKey (Crypto.genKeyDSIGN seed)

    deterministicSigningKeySeedSize :: AsType StakePoolKey -> Word
    deterministicSigningKeySeedSize AsStakePoolKey =
        Crypto.seedSizeDSIGN proxy
      where
        proxy :: Proxy (Sophie.DSIGN StandardCrypto)
        proxy = Proxy

    getVerificationKey :: SigningKey StakePoolKey -> VerificationKey StakePoolKey
    getVerificationKey (StakePoolSigningKey sk) =
        StakePoolVerificationKey (Sophie.VKey (Crypto.deriveVerKeyDSIGN sk))

    verificationKeyHash :: VerificationKey StakePoolKey -> Hash StakePoolKey
    verificationKeyHash (StakePoolVerificationKey vkey) =
        StakePoolKeyHash (Sophie.hashKey vkey)

instance SerialiseAsRawBytes (VerificationKey StakePoolKey) where
    serialiseToRawBytes (StakePoolVerificationKey (Sophie.VKey vk)) =
      Crypto.rawSerialiseVerKeyDSIGN vk

    deserialiseFromRawBytes (AsVerificationKey AsStakePoolKey) bs =
      StakePoolVerificationKey . Sophie.VKey <$>
        Crypto.rawDeserialiseVerKeyDSIGN bs

instance SerialiseAsRawBytes (SigningKey StakePoolKey) where
    serialiseToRawBytes (StakePoolSigningKey sk) =
      Crypto.rawSerialiseSignKeyDSIGN sk

    deserialiseFromRawBytes (AsSigningKey AsStakePoolKey) bs =
      StakePoolSigningKey <$> Crypto.rawDeserialiseSignKeyDSIGN bs

instance SerialiseAsBech32 (VerificationKey StakePoolKey) where
    bech32PrefixFor         _ =  "pool_vk"
    bech32PrefixesPermitted _ = ["pool_vk"]

instance SerialiseAsBech32 (SigningKey StakePoolKey) where
    bech32PrefixFor         _ =  "pool_sk"
    bech32PrefixesPermitted _ = ["pool_sk"]

newtype instance Hash StakePoolKey =
    StakePoolKeyHash (Sophie.KeyHash Sophie.StakePool StandardCrypto)
  deriving stock (Eq, Ord)
  deriving (Show, IsString) via UsingRawBytesHex (Hash StakePoolKey)
  deriving (ToCBOR, FromCBOR) via UsingRawBytes (Hash StakePoolKey)
  deriving anyclass SerialiseAsCBOR

instance SerialiseAsRawBytes (Hash StakePoolKey) where
    serialiseToRawBytes (StakePoolKeyHash (Sophie.KeyHash vkh)) =
      Crypto.hashToBytes vkh

    deserialiseFromRawBytes (AsHash AsStakePoolKey) bs =
      StakePoolKeyHash . Sophie.KeyHash <$> Crypto.hashFromBytes bs

instance SerialiseAsBech32 (Hash StakePoolKey) where
    bech32PrefixFor         _ =  "pool"
    bech32PrefixesPermitted _ = ["pool"]

instance ToJSON (Hash StakePoolKey) where
    toJSON = toJSON . serialiseToBech32

instance ToJSONKey (Hash StakePoolKey) where
  toJSONKey = toJSONKeyText serialiseToBech32

instance HasTextEnvelope (VerificationKey StakePoolKey) where
    textEnvelopeType _ = "StakePoolVerificationKey_"
                      <> fromString (Crypto.algorithmNameDSIGN proxy)
      where
        proxy :: Proxy (Sophie.DSIGN StandardCrypto)
        proxy = Proxy

instance HasTextEnvelope (SigningKey StakePoolKey) where
    textEnvelopeType _ = "StakePoolSigningKey_"
                      <> fromString (Crypto.algorithmNameDSIGN proxy)
      where
        proxy :: Proxy (Sophie.DSIGN StandardCrypto)
        proxy = Proxy

