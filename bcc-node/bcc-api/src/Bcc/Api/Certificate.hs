{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

-- | Certificates embedded in transactions
--
module Bcc.Api.Certificate (
    Certificate(..),

    -- * Registering stake address and delegating
    makeStakeAddressRegistrationCertificate,
    makeStakeAddressDeregistrationCertificate,
    makeStakeAddressDelegationCertificate,
    PoolId,

    -- * Registering stake pools
    makeStakePoolRegistrationCertificate,
    makeStakePoolRetirementCertificate,
    StakePoolParameters(..),
    StakePoolRelay(..),
    StakePoolMetadataReference(..),

    -- * Special certificates
    makeMIRCertificate,
    makeGenesisKeyDelegationCertificate,
    makeVestedKeyDelegationCertificate,
    MIRTarget (..),

    -- * Internal conversion functions
    toSophieCertificate,
    fromSophieCertificate,
    toSophiePoolParams,
    fromSophiePoolParams,

    -- * Data family instances
    AsType(..)
  ) where

import           Prelude

import           Data.ByteString (ByteString)
import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified Data.Sequence.Strict as Seq
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text

import           Data.IP (IPv4, IPv6)
import           Network.Socket (PortNumber)

import qualified Bcc.Crypto.Hash.Class as Crypto
import           Bcc.Slotting.Slot (EpochNo (..))

import           Bcc.Ledger.Crypto (StandardCrypto)

import           Bcc.Ledger.BaseTypes (maybeToStrictMaybe, strictMaybeToMaybe)
import qualified Bcc.Ledger.BaseTypes as Sophie
import qualified Bcc.Ledger.Coin as Sophie (toDeltaCoin)
import           Sophie.Spec.Ledger.TxBody (MIRPot (..))
import qualified Sophie.Spec.Ledger.TxBody as Sophie

import           Bcc.Api.Address
import           Bcc.Api.HasTypeProxy
import           Bcc.Api.Hash
import           Bcc.Api.KeysCole
import           Bcc.Api.KeysOptimum
import           Bcc.Api.KeysSophie
import           Bcc.Api.SerialiseCBOR
import           Bcc.Api.SerialiseTextEnvelope
import           Bcc.Api.StakePoolMetadata
import           Bcc.Api.Value


-- ----------------------------------------------------------------------------
-- Certificates embedded in transactions
--

data Certificate =

     -- Stake address certificates
     StakeAddressRegistrationCertificate   StakeCredential
   | StakeAddressDeregistrationCertificate StakeCredential
   | StakeAddressDelegationCertificate     StakeCredential PoolId

     -- Stake pool certificates
   | StakePoolRegistrationCertificate StakePoolParameters
   | StakePoolRetirementCertificate   PoolId EpochNo

     -- Special certificates
   | GenesisKeyDelegationCertificate (Hash GenesisKey)
                                     (Hash GenesisDelegateKey)
                                     (Hash VrfKey)
     -- Vested certificate
   | VestedKeyDelegationCertificate (Hash VestedKey)
                                   (Hash VestedDelegateKey)
                                   (Hash VrfKey)
   | MIRCertificate MIRPot MIRTarget

  deriving stock (Eq, Show)
  deriving anyclass SerialiseAsCBOR

instance HasTypeProxy Certificate where
    data AsType Certificate = AsCertificate
    proxyToAsType _ = AsCertificate

instance ToCBOR Certificate where
    toCBOR = toCBOR . toSophieCertificate

instance FromCBOR Certificate where
    fromCBOR = fromSophieCertificate <$> fromCBOR

instance HasTextEnvelope Certificate where
    textEnvelopeType _ = "CertificateSophie"
    textEnvelopeDefaultDescr cert = case cert of
      StakeAddressRegistrationCertificate{}   -> "Stake address registration"
      StakeAddressDeregistrationCertificate{} -> "Stake address de-registration"
      StakeAddressDelegationCertificate{}     -> "Stake address delegation"
      StakePoolRegistrationCertificate{}      -> "Pool registration"
      StakePoolRetirementCertificate{}        -> "Pool retirement"
      GenesisKeyDelegationCertificate{}       -> "Genesis key delegation"
      VestedKeyDelegationCertificate{}         -> "Vested key delegation"
      MIRCertificate{}                        -> "MIR"

-- | The 'MIRTarget' determines the target of a 'MIRCertificate'.
-- A 'MIRCertificate' moves entropic from either the reserves or the treasury
-- to either a collection of stake credentials or to the other pot.
data MIRTarget =

     -- | Use 'StakeAddressesMIR' to make the target of a 'MIRCertificate'
     -- a mapping of stake credentials to entropic.
     StakeAddressesMIR [(StakeCredential, Entropic)]

     -- | Use 'SendToReservesMIR' to make the target of a 'MIRCertificate'
     -- the reserves pot.
   | SendToReservesMIR Entropic

     -- | Use 'SendToTreasuryMIR' to make the target of a 'MIRCertificate'
     -- the treasury pot.
   | SendToTreasuryMIR Entropic
  deriving stock (Eq, Show)

-- ----------------------------------------------------------------------------
-- Stake pool parameters
--

type PoolId = Hash StakePoolKey

data StakePoolParameters =
     StakePoolParameters {
       stakePoolId            :: PoolId,
       stakePoolVRF           :: Hash VrfKey,
       stakePoolCost          :: Entropic,
       stakePoolMargin        :: Rational,
       stakePoolRewardAccount :: StakeAddress,
       stakePoolPledge        :: Entropic,
       stakePoolOwners        :: [Hash StakeKey],
       stakePoolRelays        :: [StakePoolRelay],
       stakePoolMetadata      :: Maybe StakePoolMetadataReference
     }
  deriving (Eq, Show)

data StakePoolRelay =

       -- | One or both of IPv4 & IPv6
       StakePoolRelayIp
          (Maybe IPv4) (Maybe IPv6) (Maybe PortNumber)

       -- | An DNS name pointing to a @A@ or @AAAA@ record.
     | StakePoolRelayDnsARecord
          ByteString (Maybe PortNumber)

       -- | A DNS name pointing to a @SRV@ record.
     | StakePoolRelayDnsSrvRecord
          ByteString

  deriving (Eq, Show)

data StakePoolMetadataReference =
     StakePoolMetadataReference {
       stakePoolMetadataURL  :: Text,
       stakePoolMetadataHash :: Hash StakePoolMetadata
     }
  deriving (Eq, Show)


-- ----------------------------------------------------------------------------
-- Constructor functions
--

makeStakeAddressRegistrationCertificate :: StakeCredential -> Certificate
makeStakeAddressRegistrationCertificate = StakeAddressRegistrationCertificate

makeStakeAddressDeregistrationCertificate :: StakeCredential -> Certificate
makeStakeAddressDeregistrationCertificate = StakeAddressDeregistrationCertificate

makeStakeAddressDelegationCertificate :: StakeCredential -> PoolId -> Certificate
makeStakeAddressDelegationCertificate = StakeAddressDelegationCertificate

makeStakePoolRegistrationCertificate :: StakePoolParameters -> Certificate
makeStakePoolRegistrationCertificate = StakePoolRegistrationCertificate

makeStakePoolRetirementCertificate :: PoolId -> EpochNo -> Certificate
makeStakePoolRetirementCertificate = StakePoolRetirementCertificate

makeGenesisKeyDelegationCertificate :: Hash GenesisKey
                                    -> Hash GenesisDelegateKey
                                    -> Hash VrfKey
                                    -> Certificate
makeGenesisKeyDelegationCertificate = GenesisKeyDelegationCertificate

makeVestedKeyDelegationCertificate :: Hash VestedKey
                                  -> Hash VestedDelegateKey
                                  -> Hash VrfKey
                                  -> Certificate
makeVestedKeyDelegationCertificate = VestedKeyDelegationCertificate

makeMIRCertificate :: MIRPot -> MIRTarget -> Certificate
makeMIRCertificate = MIRCertificate


-- ----------------------------------------------------------------------------
-- Internal conversion functions
--

toSophieCertificate :: Certificate -> Sophie.DCert StandardCrypto
toSophieCertificate (StakeAddressRegistrationCertificate stakecred) =
    Sophie.DCertDeleg $
      Sophie.RegKey
        (toSophieStakeCredential stakecred)

toSophieCertificate (StakeAddressDeregistrationCertificate stakecred) =
    Sophie.DCertDeleg $
      Sophie.DeRegKey
        (toSophieStakeCredential stakecred)

toSophieCertificate (StakeAddressDelegationCertificate
                        stakecred (StakePoolKeyHash poolid)) =
    Sophie.DCertDeleg $
    Sophie.Delegate $
      Sophie.Delegation
        (toSophieStakeCredential stakecred)
        poolid

toSophieCertificate (StakePoolRegistrationCertificate poolparams) =
    Sophie.DCertPool $
      Sophie.RegPool
        (toSophiePoolParams poolparams)

toSophieCertificate (StakePoolRetirementCertificate
                       (StakePoolKeyHash poolid) epochno) =
    Sophie.DCertPool $
      Sophie.RetirePool
        poolid
        epochno

toSophieCertificate (GenesisKeyDelegationCertificate
                       (GenesisKeyHash         genesiskh)
                       (GenesisDelegateKeyHash delegatekh)
                       (VrfKeyHash             vrfkh)) =
    Sophie.DCertGenesis $
      Sophie.GenesisDelegCert
        genesiskh
        delegatekh
        vrfkh

toSophieCertificate (VestedKeyDelegationCertificate
                       (VestedKeyHash         vestedkh)
                       (VestedDelegateKeyHash vesteddelegatekh)
                       (VrfKeyHash             vrfkh)) =
    Sophie.DCertVested $
      Sophie.VestedDelegCert
        vestedkh
        vesteddelegatekh
        vrfkh

toSophieCertificate (MIRCertificate mirpot (StakeAddressesMIR amounts)) =
    Sophie.DCertMir $
      Sophie.MIRCert
        mirpot
        (Sophie.StakeAddressesMIR $ Map.fromListWith (<>)
           [ (toSophieStakeCredential sc, Sophie.toDeltaCoin . toSophieEntropic $ v)
           | (sc, v) <- amounts ])

toSophieCertificate (MIRCertificate mirPot (SendToReservesMIR amount)) =
    case mirPot of
      TreasuryMIR ->
        Sophie.DCertMir $
          Sophie.MIRCert
            TreasuryMIR
            (Sophie.SendToOppositePotMIR $ toSophieEntropic amount)
      ReservesMIR ->
        error "toSophieCertificate: Incorrect MIRPot specified. Expected TreasuryMIR but got ReservesMIR"

toSophieCertificate (MIRCertificate mirPot (SendToTreasuryMIR amount)) =
    case mirPot of
      ReservesMIR ->
        Sophie.DCertMir $
          Sophie.MIRCert
            ReservesMIR
            (Sophie.SendToOppositePotMIR $ toSophieEntropic amount)
      TreasuryMIR ->
        error "toSophieCertificate: Incorrect MIRPot specified. Expected ReservesMIR but got TreasuryMIR"


fromSophieCertificate :: Sophie.DCert StandardCrypto -> Certificate
fromSophieCertificate (Sophie.DCertDeleg (Sophie.RegKey stakecred)) =
    StakeAddressRegistrationCertificate
      (fromSophieStakeCredential stakecred)

fromSophieCertificate (Sophie.DCertDeleg (Sophie.DeRegKey stakecred)) =
    StakeAddressDeregistrationCertificate
      (fromSophieStakeCredential stakecred)

fromSophieCertificate (Sophie.DCertDeleg
                         (Sophie.Delegate (Sophie.Delegation stakecred poolid))) =
    StakeAddressDelegationCertificate
      (fromSophieStakeCredential stakecred)
      (StakePoolKeyHash poolid)

fromSophieCertificate (Sophie.DCertPool (Sophie.RegPool poolparams)) =
    StakePoolRegistrationCertificate
      (fromSophiePoolParams poolparams)

fromSophieCertificate (Sophie.DCertPool (Sophie.RetirePool poolid epochno)) =
    StakePoolRetirementCertificate
      (StakePoolKeyHash poolid)
      epochno

fromSophieCertificate (Sophie.DCertGenesis
                         (Sophie.GenesisDelegCert genesiskh delegatekh vrfkh)) =
    GenesisKeyDelegationCertificate
      (GenesisKeyHash         genesiskh)
      (GenesisDelegateKeyHash delegatekh)
      (VrfKeyHash             vrfkh)

fromSophieCertificate (Sophie.DCertVested
                         (Sophie.VestedDelegCert vestedkh vesteddelegatekh vrfkh)) =
    VestedKeyDelegationCertificate
      (VestedKeyHash         vestedkh)
      (VestedDelegateKeyHash vesteddelegatekh)
      (VrfKeyHash             vrfkh)

fromSophieCertificate (Sophie.DCertMir
                         (Sophie.MIRCert mirpot (Sophie.StakeAddressesMIR amounts))) =
    MIRCertificate
      mirpot
      (StakeAddressesMIR
        [ (fromSophieStakeCredential sc, fromSophieDeltaEntropic v)
        | (sc, v) <- Map.toList amounts ]
      )
fromSophieCertificate (Sophie.DCertMir
                         (Sophie.MIRCert ReservesMIR (Sophie.SendToOppositePotMIR amount))) =
    MIRCertificate ReservesMIR
      (SendToTreasuryMIR $ fromSophieEntropic amount)

fromSophieCertificate (Sophie.DCertMir
                         (Sophie.MIRCert TreasuryMIR (Sophie.SendToOppositePotMIR amount))) =
    MIRCertificate TreasuryMIR
      (SendToReservesMIR $ fromSophieEntropic amount)

toSophiePoolParams :: StakePoolParameters -> Sophie.PoolParams StandardCrypto
toSophiePoolParams StakePoolParameters {
                      stakePoolId            = StakePoolKeyHash poolkh
                    , stakePoolVRF           = VrfKeyHash vrfkh
                    , stakePoolCost
                    , stakePoolMargin
                    , stakePoolRewardAccount
                    , stakePoolPledge
                    , stakePoolOwners
                    , stakePoolRelays
                    , stakePoolMetadata
                    } =
    --TODO: validate pool parameters such as the PoolMargin below, but also
    -- do simple client-side sanity checks, e.g. on the pool metadata url
    Sophie.PoolParams {
      Sophie._poolId     = poolkh
    , Sophie._poolVrf    = vrfkh
    , Sophie._poolPledge = toSophieEntropic stakePoolPledge
    , Sophie._poolCost   = toSophieEntropic stakePoolCost
    , Sophie._poolMargin = fromMaybe
                              (error "toSophiePoolParams: invalid PoolMargin")
                              (Sophie.boundRational stakePoolMargin)
    , Sophie._poolRAcnt  = toSophieStakeAddr stakePoolRewardAccount
    , Sophie._poolOwners = Set.fromList
                              [ kh | StakeKeyHash kh <- stakePoolOwners ]
    , Sophie._poolRelays = Seq.fromList
                              (map toSophieStakePoolRelay stakePoolRelays)
    , Sophie._poolMD     = toSophiePoolMetadata <$>
                              maybeToStrictMaybe stakePoolMetadata
    }
  where
    toSophieStakePoolRelay :: StakePoolRelay -> Sophie.StakePoolRelay
    toSophieStakePoolRelay (StakePoolRelayIp mipv4 mipv6 mport) =
      Sophie.SingleHostAddr
        (fromIntegral <$> maybeToStrictMaybe mport)
        (maybeToStrictMaybe mipv4)
        (maybeToStrictMaybe mipv6)

    toSophieStakePoolRelay (StakePoolRelayDnsARecord dnsname mport) =
      Sophie.SingleHostName
        (fromIntegral <$> maybeToStrictMaybe mport)
        (toSophieDnsName dnsname)

    toSophieStakePoolRelay (StakePoolRelayDnsSrvRecord dnsname) =
      Sophie.MultiHostName
        (toSophieDnsName dnsname)

    toSophiePoolMetadata :: StakePoolMetadataReference -> Sophie.PoolMetadata
    toSophiePoolMetadata StakePoolMetadataReference {
                            stakePoolMetadataURL
                          , stakePoolMetadataHash = StakePoolMetadataHash mdh
                          } =
      Sophie.PoolMetadata {
        Sophie._poolMDUrl  = toSophieUrl stakePoolMetadataURL
      , Sophie._poolMDHash = Crypto.hashToBytes mdh
      }

    toSophieDnsName :: ByteString -> Sophie.DnsName
    toSophieDnsName = fromMaybe (error "toSophieDnsName: invalid dns name. TODO: proper validation")
                     . Sophie.textToDns
                     . Text.decodeLatin1

    toSophieUrl :: Text -> Sophie.Url
    toSophieUrl = fromMaybe (error "toSophieUrl: invalid url. TODO: proper validation")
                 . Sophie.textToUrl


fromSophiePoolParams :: Sophie.PoolParams StandardCrypto
                      -> StakePoolParameters
fromSophiePoolParams
    Sophie.PoolParams {
      Sophie._poolId
    , Sophie._poolVrf
    , Sophie._poolPledge
    , Sophie._poolCost
    , Sophie._poolMargin
    , Sophie._poolRAcnt
    , Sophie._poolOwners
    , Sophie._poolRelays
    , Sophie._poolMD
    } =
    StakePoolParameters {
      stakePoolId            = StakePoolKeyHash _poolId
    , stakePoolVRF           = VrfKeyHash _poolVrf
    , stakePoolCost          = fromSophieEntropic _poolCost
    , stakePoolMargin        = Sophie.unboundRational _poolMargin
    , stakePoolRewardAccount = fromSophieStakeAddr _poolRAcnt
    , stakePoolPledge        = fromSophieEntropic _poolPledge
    , stakePoolOwners        = map StakeKeyHash (Set.toList _poolOwners)
    , stakePoolRelays        = map fromSophieStakePoolRelay
                                   (Foldable.toList _poolRelays)
    , stakePoolMetadata      = fromSophiePoolMetadata <$>
                                 strictMaybeToMaybe _poolMD
    }
  where
    fromSophieStakePoolRelay :: Sophie.StakePoolRelay -> StakePoolRelay
    fromSophieStakePoolRelay (Sophie.SingleHostAddr mport mipv4 mipv6) =
      StakePoolRelayIp
        (strictMaybeToMaybe mipv4)
        (strictMaybeToMaybe mipv6)
        (fromIntegral . Sophie.portToWord16 <$> strictMaybeToMaybe mport)

    fromSophieStakePoolRelay (Sophie.SingleHostName mport dnsname) =
      StakePoolRelayDnsARecord
        (fromSophieDnsName dnsname)
        (fromIntegral . Sophie.portToWord16 <$> strictMaybeToMaybe mport)

    fromSophieStakePoolRelay (Sophie.MultiHostName dnsname) =
      StakePoolRelayDnsSrvRecord
        (fromSophieDnsName dnsname)

    fromSophiePoolMetadata :: Sophie.PoolMetadata -> StakePoolMetadataReference
    fromSophiePoolMetadata Sophie.PoolMetadata {
                              Sophie._poolMDUrl
                            , Sophie._poolMDHash
                            } =
      StakePoolMetadataReference {
        stakePoolMetadataURL  = Sophie.urlToText _poolMDUrl
      , stakePoolMetadataHash = StakePoolMetadataHash
                              . fromMaybe (error "fromSophiePoolMetadata: invalid hash. TODO: proper validation")
                              . Crypto.hashFromBytes
                              $ _poolMDHash
      }

    --TODO: change the ledger rep of the DNS name to use ShortByteString
    fromSophieDnsName :: Sophie.DnsName -> ByteString
    fromSophieDnsName = Text.encodeUtf8
                       . Sophie.dnsToText
