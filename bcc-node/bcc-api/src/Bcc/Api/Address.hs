{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | Bcc addresses: payment and stake addresses.
--
module Bcc.Api.Address (
    -- * Payment addresses
    -- | Constructing and inspecting normal payment addresses
    Address(..),

    -- ** Cole addresses
    ColeAddr,
    makeColeAddress,

    -- ** Sophie addresses
    SophieAddr,
    makeSophieAddress,
    PaymentCredential(..),
    StakeAddressReference(..),
    StakeAddressPointer(..),

    -- ** Addresses in any era
    AddressAny(..),

    -- ** Addresses in specific eras
    AddressInEra(..),
    AddressTypeInEra(..),
    coleAddressInEra,
    sophieAddressInEra,
    anyAddressInSophieBasedEra,
    anyAddressInEra,
    toAddressAny,
    makeColeAddressInEra,
    makeSophieAddressInEra,

    -- * Stake addresses
    -- | Constructing and inspecting stake addresses
    StakeAddress(..),
    StakeCredential(..),
    makeStakeAddress,
    StakeKey,
    StakeExtendedKey,

    -- * Internal conversion functions
    toSophieAddr,
    toSophieStakeAddr,
    toSophieStakeCredential,
    fromSophieAddr,
    fromSophiePaymentCredential,
    fromSophieStakeAddr,
    fromSophieStakeCredential,
    fromSophieStakeReference,

    -- * Serialising addresses
    SerialiseAddress(..),

    -- * Data family instances
    AsType(AsColeAddr, AsSophieAddr, AsColeAddress, AsSophieAddress,
           AsAddress, AsAddressAny, AsAddressInEra, AsStakeAddress),

    -- * Helpers
    isKeyAddress
  ) where

import           Prelude

import           Data.Aeson (ToJSON (..))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base58 as Base58
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text

import           Control.Applicative

import           Bcc.Api.Eras
import           Bcc.Api.HasTypeProxy
import           Bcc.Api.Hash
import           Bcc.Api.Key
import           Bcc.Api.KeysCole
import           Bcc.Api.KeysSophie
import           Bcc.Api.NetworkId
import           Bcc.Api.Script
import           Bcc.Api.SerialiseBech32
import           Bcc.Api.SerialiseRaw
import qualified Bcc.Chain.Common as Cole
import qualified Bcc.Ledger.Address as Sophie
import qualified Bcc.Ledger.BaseTypes as Sophie
import qualified Bcc.Ledger.Credential as Sophie
import           Bcc.Ledger.Crypto (StandardCrypto)

-- ----------------------------------------------------------------------------
-- Address Serialisation
--

-- | Address serialisation uses different serialisation formats for different
-- kinds of addresses, so it needs its own class.
--
-- In particular, Cole addresses are typically formatted in base 58, while
-- Sophie addresses (payment and stake) are formatted using Bech32.
--
class HasTypeProxy addr => SerialiseAddress addr where

    serialiseAddress :: addr -> Text

    deserialiseAddress :: AsType addr -> Text -> Maybe addr
    -- TODO: consider adding data AddressDecodeError


-- ----------------------------------------------------------------------------
-- Payment address types
--

-- | A type used as a tag to distinguish Cole addresses.
data ColeAddr

-- | A type used as a tag to distinguish Sophie addresses.
data SophieAddr

instance HasTypeProxy ColeAddr where
    data AsType ColeAddr = AsColeAddr
    proxyToAsType _ = AsColeAddr

instance HasTypeProxy SophieAddr where
    data AsType SophieAddr = AsSophieAddr
    proxyToAsType _ = AsSophieAddr


-- ----------------------------------------------------------------------------
-- Payment addresses
--

-- | Addresses are used as locations where assets live. The address determines
-- the rights needed to spend assets at the address: in particular holding some
-- signing key or being able to satisfy the conditions of a script.
--
-- There are currently two types of address:
--
-- * Cole addresses, which use the type tag 'ColeAddr'; and
-- * Sophie addresses, which use the type tag 'SophieAddr'. Notably, Sophie
--   addresses support scripts and stake delegation.
--
-- The /address type/ is subtly from the /ledger era/ in which each
-- address type is valid: while Cole addresses are the only choice in the
-- Cole era, the Sophie era and all subsequent eras support both Cole and
-- Sophie addresses. The 'Address' type param only says the type of the address
-- (either Cole or Sophie). The 'AddressInEra' type connects the address type
-- with the era in which it is supported.
--
data Address addrtype where

     -- | Cole addresses were the only supported address type in the original
     -- Cole era.
     --
     ColeAddress
       :: Cole.Address
       -> Address ColeAddr

     -- | Sophie addresses allow delegation. Sophie addresses were introduced
     -- in Sophie era and are thus supported from the Sophie era onwards
     --
     SophieAddress
       :: Sophie.Network
       -> Sophie.PaymentCredential StandardCrypto
       -> Sophie.StakeReference    StandardCrypto
       -> Address SophieAddr
       -- Note that the two ledger credential types here are parametrised by
       -- the era, but in fact this is a phantom type parameter and they are
       -- the same for all eras. See 'toSophieAddr' below.

deriving instance Eq   (Address addrtype)
deriving instance Ord  (Address addrtype)
deriving instance Show (Address addrtype)


instance HasTypeProxy addrtype => HasTypeProxy (Address addrtype) where
    data AsType (Address addrtype) = AsAddress (AsType addrtype)
    proxyToAsType _ = AsAddress (proxyToAsType (Proxy :: Proxy addrtype))

pattern AsColeAddress :: AsType (Address ColeAddr)
pattern AsColeAddress   = AsAddress AsColeAddr
{-# COMPLETE AsColeAddress #-}

pattern AsSophieAddress :: AsType (Address SophieAddr)
pattern AsSophieAddress = AsAddress AsSophieAddr
{-# COMPLETE AsSophieAddress #-}

instance SerialiseAsRawBytes (Address ColeAddr) where
    serialiseToRawBytes (ColeAddress addr) =
        Sophie.serialiseAddr
      . Sophie.AddrBootstrap
      . Sophie.BootstrapAddress
      $ addr

    deserialiseFromRawBytes (AsAddress AsColeAddr) bs =
        case Sophie.deserialiseAddr bs :: Maybe (Sophie.Addr StandardCrypto) of
          Nothing             -> Nothing
          Just Sophie.Addr{} -> Nothing
          Just (Sophie.AddrBootstrap (Sophie.BootstrapAddress addr)) ->
            Just (ColeAddress addr)

instance SerialiseAsRawBytes (Address SophieAddr) where
    serialiseToRawBytes (SophieAddress nw pc scr) =
        Sophie.serialiseAddr (Sophie.Addr nw pc scr)

    deserialiseFromRawBytes (AsAddress AsSophieAddr) bs =
        case Sophie.deserialiseAddr bs of
          Nothing                       -> Nothing
          Just Sophie.AddrBootstrap{}  -> Nothing
          Just (Sophie.Addr nw pc scr) -> Just (SophieAddress nw pc scr)

instance SerialiseAsBech32 (Address SophieAddr) where
    bech32PrefixFor (SophieAddress Sophie.Mainnet _ _) = "addr"
    bech32PrefixFor (SophieAddress Sophie.Testnet _ _) = "addr_test"

    bech32PrefixesPermitted (AsAddress AsSophieAddr) = ["addr", "addr_test"]


instance SerialiseAddress (Address ColeAddr) where
    serialiseAddress addr@ColeAddress{} =
         Text.decodeLatin1
       . Base58.encodeBase58 Base58.bitcoinAlphabet
       . serialiseToRawBytes
       $ addr

    deserialiseAddress (AsAddress AsColeAddr) txt = do
      bs <- Base58.decodeBase58 Base58.bitcoinAlphabet (Text.encodeUtf8 txt)
      deserialiseFromRawBytes (AsAddress AsColeAddr) bs

instance SerialiseAddress (Address SophieAddr) where
    serialiseAddress addr@SophieAddress{} =
      serialiseToBech32 addr

    deserialiseAddress (AsAddress AsSophieAddr) t =
      either (const Nothing) Just $
      deserialiseFromBech32 (AsAddress AsSophieAddr) t


makeColeAddress :: NetworkId
                 -> VerificationKey ColeKey
                 -> Address ColeAddr
makeColeAddress nw (ColeVerificationKey vk) =
    ColeAddress $
      Cole.makeVerKeyAddress
        (toColeNetworkMagic nw)
        vk


makeSophieAddress :: NetworkId
                   -> PaymentCredential
                   -> StakeAddressReference
                   -> Address SophieAddr
makeSophieAddress nw pc scr =
    SophieAddress
      (toSophieNetwork nw)
      (toSophiePaymentCredential pc)
      (toSophieStakeReference scr)


-- ----------------------------------------------------------------------------
-- Either type of address
--

-- | Either a Cole address or a Sophie address.
--
-- Sometimes we need to be able to work with either of the two types of
-- address (Cole or Sophie addresses), but without reference to an era in
-- which the address will be used. This type serves that purpose.
--
data AddressAny = AddressCole   !(Address ColeAddr)
                | AddressSophie !(Address SophieAddr)
  deriving (Eq, Ord, Show)

instance HasTypeProxy AddressAny where
    data AsType AddressAny = AsAddressAny
    proxyToAsType _ = AsAddressAny

instance SerialiseAsRawBytes AddressAny where
    serialiseToRawBytes (AddressCole   addr) = serialiseToRawBytes addr
    serialiseToRawBytes (AddressSophie addr) = serialiseToRawBytes addr

    deserialiseFromRawBytes AsAddressAny bs =
      case Sophie.deserialiseAddr bs of
        Nothing -> Nothing
        Just (Sophie.AddrBootstrap (Sophie.BootstrapAddress addr)) ->
          Just (AddressCole (ColeAddress addr))

        Just (Sophie.Addr nw pc scr) ->
          Just (AddressSophie (SophieAddress nw pc scr))

instance SerialiseAddress AddressAny where
    serialiseAddress (AddressCole   addr) = serialiseAddress addr
    serialiseAddress (AddressSophie addr) = serialiseAddress addr

    deserialiseAddress AsAddressAny t =
          (AddressCole   <$> deserialiseAddress (AsAddress AsColeAddr)   t)
      <|> (AddressSophie <$> deserialiseAddress (AsAddress AsSophieAddr) t)


-- ----------------------------------------------------------------------------
-- Addresses in the context of a ledger era
--

-- | An 'Address' that can be used in a particular ledger era.
--
-- All current ledger eras support Cole addresses. Sophie addresses are
-- supported in the 'SophieEra' and later eras.
--
data AddressInEra era where
     AddressInEra :: AddressTypeInEra addrtype era
                  -> Address addrtype
                  -> AddressInEra era

instance IsBccEra era => ToJSON (AddressInEra era) where
  toJSON = Aeson.String . serialiseAddress

instance Eq (AddressInEra era) where
  (==) (AddressInEra ColeAddressInAnyEra addr1)
       (AddressInEra ColeAddressInAnyEra addr2) = addr1 == addr2

  (==) (AddressInEra SophieAddressInEra{} addr1)
       (AddressInEra SophieAddressInEra{} addr2) = addr1 == addr2

  (==) (AddressInEra ColeAddressInAnyEra _)
       (AddressInEra SophieAddressInEra{} _) = False

  (==) (AddressInEra SophieAddressInEra{} _)
       (AddressInEra ColeAddressInAnyEra _) = False

deriving instance Show (AddressInEra era)

data AddressTypeInEra addrtype era where

     ColeAddressInAnyEra :: AddressTypeInEra ColeAddr era

     SophieAddressInEra  :: SophieBasedEra era
                          -> AddressTypeInEra SophieAddr era

deriving instance Show (AddressTypeInEra addrtype era)


instance HasTypeProxy era => HasTypeProxy (AddressInEra era) where
    data AsType (AddressInEra era) = AsAddressInEra (AsType era)
    proxyToAsType _ = AsAddressInEra (proxyToAsType (Proxy :: Proxy era))

instance IsBccEra era => SerialiseAsRawBytes (AddressInEra era) where

    serialiseToRawBytes (AddressInEra ColeAddressInAnyEra addr) =
      serialiseToRawBytes addr

    serialiseToRawBytes (AddressInEra SophieAddressInEra{} addr) =
      serialiseToRawBytes addr

    deserialiseFromRawBytes _ bs =
      anyAddressInEra bccEra =<< deserialiseFromRawBytes AsAddressAny bs

instance IsBccEra era => SerialiseAddress (AddressInEra era) where
    serialiseAddress (AddressInEra ColeAddressInAnyEra addr) =
      serialiseAddress addr

    serialiseAddress (AddressInEra SophieAddressInEra{} addr) =
      serialiseAddress addr

    deserialiseAddress _ t =
      anyAddressInEra bccEra =<< deserialiseAddress AsAddressAny t


coleAddressInEra :: Address ColeAddr -> AddressInEra era
coleAddressInEra = AddressInEra ColeAddressInAnyEra


sophieAddressInEra :: IsSophieBasedEra era
                    => Address SophieAddr -> AddressInEra era
sophieAddressInEra = AddressInEra (SophieAddressInEra sophieBasedEra)


anyAddressInSophieBasedEra :: IsSophieBasedEra era
                            => AddressAny -> AddressInEra era
anyAddressInSophieBasedEra (AddressCole   addr) = coleAddressInEra addr
anyAddressInSophieBasedEra (AddressSophie addr) = sophieAddressInEra addr


anyAddressInEra :: BccEra era
                -> AddressAny
                -> Maybe (AddressInEra era)
anyAddressInEra _ (AddressCole addr) =
    Just (AddressInEra ColeAddressInAnyEra addr)

anyAddressInEra era (AddressSophie addr) =
    case bccEraStyle era of
      LegacyColeEra       -> Nothing
      SophieBasedEra era' -> Just (AddressInEra (SophieAddressInEra era') addr)

toAddressAny :: Address addr -> AddressAny
toAddressAny a@SophieAddress{} = AddressSophie a
toAddressAny a@ColeAddress{}   = AddressCole a

makeColeAddressInEra :: NetworkId
                      -> VerificationKey ColeKey
                      -> AddressInEra era
makeColeAddressInEra nw vk =
    coleAddressInEra (makeColeAddress nw vk)


makeSophieAddressInEra :: IsSophieBasedEra era
                        => NetworkId
                        -> PaymentCredential
                        -> StakeAddressReference
                        -> AddressInEra era
makeSophieAddressInEra nw pc scr =
    sophieAddressInEra (makeSophieAddress nw pc scr)


-- ----------------------------------------------------------------------------
-- Stake addresses
--

data StakeAddress where

     StakeAddress
       :: Sophie.Network
       -> Sophie.StakeCredential StandardCrypto
       -> StakeAddress
  deriving (Eq, Ord, Show)

data PaymentCredential
       = PaymentCredentialByKey    (Hash PaymentKey)
       | PaymentCredentialByScript  ScriptHash
  deriving (Eq, Ord, Show)

data StakeCredential
       = StakeCredentialByKey    (Hash StakeKey)
       | StakeCredentialByScript  ScriptHash
  deriving (Eq, Ord, Show)

data StakeAddressReference
       = StakeAddressByValue   StakeCredential
       | StakeAddressByPointer StakeAddressPointer
       | NoStakeAddress
  deriving (Eq, Show)

newtype StakeAddressPointer = StakeAddressPointer
  { unStakeAddressPointer :: Sophie.Ptr
  }
  deriving (Eq, Show)

instance HasTypeProxy StakeAddress where
    data AsType StakeAddress = AsStakeAddress
    proxyToAsType _ = AsStakeAddress


instance SerialiseAsRawBytes StakeAddress where
    serialiseToRawBytes (StakeAddress nw sc) =
        Sophie.serialiseRewardAcnt (Sophie.RewardAcnt nw sc)

    deserialiseFromRawBytes AsStakeAddress bs =
        case Sophie.deserialiseRewardAcnt bs of
          Nothing -> Nothing
          Just (Sophie.RewardAcnt nw sc) -> Just (StakeAddress nw sc)


instance SerialiseAsBech32 StakeAddress where
    bech32PrefixFor (StakeAddress Sophie.Mainnet _) = "stake"
    bech32PrefixFor (StakeAddress Sophie.Testnet _) = "stake_test"

    bech32PrefixesPermitted AsStakeAddress = ["stake", "stake_test"]


instance SerialiseAddress StakeAddress where
    serialiseAddress addr@StakeAddress{} =
      serialiseToBech32 addr

    deserialiseAddress AsStakeAddress t =
      either (const Nothing) Just $
      deserialiseFromBech32 AsStakeAddress t


makeStakeAddress :: NetworkId
                 -> StakeCredential
                 -> StakeAddress
makeStakeAddress nw sc =
    StakeAddress
      (toSophieNetwork nw)
      (toSophieStakeCredential sc)

-- ----------------------------------------------------------------------------
-- Helpers
--

-- | Is the UTxO at the address only spendable via a key witness.
isKeyAddress :: AddressInEra era -> Bool
isKeyAddress (AddressInEra ColeAddressInAnyEra _) = True
isKeyAddress (AddressInEra (SophieAddressInEra _) (SophieAddress _ pCred _)) =
  case fromSophiePaymentCredential pCred of
    PaymentCredentialByKey _ -> True
    PaymentCredentialByScript _ -> False


-- ----------------------------------------------------------------------------
-- Internal conversion functions
--

toSophieAddr :: AddressInEra era -> Sophie.Addr StandardCrypto
toSophieAddr (AddressInEra ColeAddressInAnyEra (ColeAddress addr)) =
    Sophie.AddrBootstrap (Sophie.BootstrapAddress addr)
toSophieAddr (AddressInEra (SophieAddressInEra _)
                            (SophieAddress nw pc scr)) =
    Sophie.Addr nw pc scr

toSophieStakeAddr :: StakeAddress -> Sophie.RewardAcnt StandardCrypto
toSophieStakeAddr (StakeAddress nw sc) =
    Sophie.RewardAcnt {
      Sophie.getRwdNetwork = nw,
      Sophie.getRwdCred    = sc
    }

toSophiePaymentCredential :: PaymentCredential
                           -> Sophie.PaymentCredential StandardCrypto
toSophiePaymentCredential (PaymentCredentialByKey (PaymentKeyHash kh)) =
    Sophie.KeyHashObj kh
toSophiePaymentCredential (PaymentCredentialByScript sh) =
    Sophie.ScriptHashObj (toSophieScriptHash sh)

toSophieStakeCredential :: StakeCredential
                         -> Sophie.StakeCredential StandardCrypto
toSophieStakeCredential (StakeCredentialByKey (StakeKeyHash kh)) =
    Sophie.KeyHashObj kh
toSophieStakeCredential (StakeCredentialByScript sh) =
    Sophie.ScriptHashObj (toSophieScriptHash sh)

toSophieStakeReference :: StakeAddressReference
                        -> Sophie.StakeReference StandardCrypto
toSophieStakeReference (StakeAddressByValue stakecred) =
    Sophie.StakeRefBase (toSophieStakeCredential stakecred)
toSophieStakeReference (StakeAddressByPointer ptr) =
    Sophie.StakeRefPtr (unStakeAddressPointer ptr)
toSophieStakeReference  NoStakeAddress =
    Sophie.StakeRefNull


fromSophieAddr :: IsSophieBasedEra era
                => Sophie.Addr StandardCrypto -> AddressInEra era
fromSophieAddr (Sophie.AddrBootstrap (Sophie.BootstrapAddress addr)) =
    AddressInEra ColeAddressInAnyEra (ColeAddress addr)

fromSophieAddr (Sophie.Addr nw pc scr) =
    AddressInEra
      (SophieAddressInEra sophieBasedEra)
      (SophieAddress nw pc scr)

fromSophieStakeAddr :: Sophie.RewardAcnt StandardCrypto -> StakeAddress
fromSophieStakeAddr (Sophie.RewardAcnt nw sc) = StakeAddress nw sc

fromSophieStakeCredential :: Sophie.StakeCredential StandardCrypto
                           -> StakeCredential
fromSophieStakeCredential (Sophie.KeyHashObj kh) =
    StakeCredentialByKey (StakeKeyHash kh)
fromSophieStakeCredential (Sophie.ScriptHashObj sh) =
    StakeCredentialByScript (fromSophieScriptHash sh)

fromSophiePaymentCredential :: Sophie.PaymentCredential StandardCrypto
                             -> PaymentCredential
fromSophiePaymentCredential (Sophie.KeyHashObj kh) =
  PaymentCredentialByKey (PaymentKeyHash kh)
fromSophiePaymentCredential (Sophie.ScriptHashObj sh) =
  PaymentCredentialByScript (ScriptHash sh)

fromSophieStakeReference :: Sophie.StakeReference StandardCrypto
                          -> StakeAddressReference
fromSophieStakeReference (Sophie.StakeRefBase stakecred) =
  StakeAddressByValue (fromSophieStakeCredential stakecred)
fromSophieStakeReference (Sophie.StakeRefPtr ptr) =
  StakeAddressByPointer (StakeAddressPointer ptr)
fromSophieStakeReference Sophie.StakeRefNull =
  NoStakeAddress
