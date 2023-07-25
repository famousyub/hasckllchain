-- | This module provides a library interface that is intended to be
-- the complete API for Sophie covering everything, including
-- exposing constructors for the lower level types.
--

module Bcc.Api.Sophie
  ( module Bcc.Api,

    -- * Genesis
    SophieGenesis(..),
    sophieGenesisDefaults,

    -- * Cryptographic key interface
    -- $keys
    VerificationKey(..),
    SigningKey(..),

    -- * Hashes
    Hash(..),

    -- * Payment addresses
    -- | Constructing and inspecting Sophie payment addresses
    Address(SophieAddress),
    toSophieAddr,
    fromSophieAddr,
    toSophieStakeCredential,
    fromSophieStakeCredential,
    NetworkId(Mainnet, Testnet),

    -- * Stake addresses
    PaymentCredential(..),
    StakeAddress(..),
    StakeAddressReference(..),
    StakeCredential(..),
    toSophieStakeAddr,
    fromSophieStakeAddr,
    fromSophieStakeReference,
    fromSophiePaymentCredential,

    -- * Building transactions
    -- | Constructing and inspecting transactions
    TxBody(SophieTxBody),
    TxId(TxId),
    toSophieTxId,
    fromSophieTxId,
    getTxIdSophie,
    TxIn(TxIn),
    toSophieTxIn,
    fromSophieTxIn,
    TxOut(TxOut),
    toSophieTxOut,
    fromSophieTxOut,
    TxIx(TxIx),
    Entropic(Entropic),
    toSophieEntropic,
    fromSophieEntropic,
    toJenValue,
    fromJenValue,
    calcMinimumDeposit,

    -- * Signing transactions
    -- | Creating transaction witnesses one by one, or all in one go.
    Tx(SophieTx),

    -- ** Incremental signing and separate witnesses
    KeyWitness
      ( SophieBootstrapWitness
      , SophieKeyWitness
      ),
    SophieWitnessSigningKey
      ( WitnessPaymentKey
      , WitnessPaymentExtendedKey
      , WitnessStakeKey
      , WitnessStakeExtendedKey
      , WitnessStakePoolKey
      , WitnessGenesisKey
      , WitnessGenesisExtendedKey
      , WitnessGenesisDelegateKey
      , WitnessGenesisDelegateExtendedKey
      , WitnessVestedKey
      , WitnessVestedExtendedKey
      , WitnessVestedDelegateKey
      , WitnessVestedDelegateExtendedKey
      ),
    SophieSigningKey(..),
    getSophieKeyWitnessVerificationKey,
    makeSophieSignature,
    toSophieSigningKey,

    -- * Transaction metadata
    -- | Embedding additional structured data within transactions.
    toSophieMetadata,
    fromSophieMetadata,
    toSophieMetadatum,
    fromSophieMetadatum,

    -- * Protocol parameters
    ProtocolParameters(..),
    checkProtocolParameters,
    ProtocolParametersError(..),

    -- * Scripts
    toSophieScript,
    toSophieMultiSig,
    fromSophieMultiSig,
    toEvieTimelock,
    fromEvieTimelock,
    toSophieScriptHash,
    fromSophieScriptHash,
    ZerepochScript(..),
    toZerepochData,
    fromZerepochData,
    toAurumData,
    fromAurumData,
    toAurumPrices,
    fromAurumPrices,
    toAurumExUnits,
    fromAurumExUnits,
    toAurumRdmrPtr,
    fromAurumRdmrPtr,

    -- * Certificates
    Certificate (..),
    toSophieCertificate,
    fromSophieCertificate,

    -- ** Operational certificates
    OperationalCertificate(OperationalCertificate),
    OperationalCertificateIssueCounter(OperationalCertificateIssueCounter),
    OperationalCertIssueError(..),

    -- * Stake Pool
    StakePoolMetadata(StakePoolMetadata),
    stakePoolName,
    stakePoolDescription,
    stakePoolTicker,
    stakePoolHomepage,
    StakePoolMetadataReference(StakePoolMetadataReference),
    stakePoolMetadataURL,
    stakePoolMetadataHash,
    StakePoolParameters(StakePoolParameters),
    stakePoolId,
    stakePoolVRF,
    stakePoolCost,
    stakePoolMargin,
    stakePoolRewardAccount,
    stakePoolPledge,
    stakePoolOwners,
    stakePoolRelays,
    stakePoolMetadata,
    StakePoolRelay
      ( StakePoolRelayIp
      , StakePoolRelayDnsARecord
      , StakePoolRelayDnsSrvRecord
      ),
    EpochNo(..),

    -- ** Stake pool operator's keys
    StakePoolKey,
    PoolId,  

    -- ** KES keys
    KesKey,
    KESPeriod(..),

    -- ** VRF keys
    VrfKey,

    -- ** Low level protocol interaction with a Bcc node
    LocalNodeConnectInfo(LocalNodeConnectInfo),
    SophieMode,
    ConsensusMode
      ( SophieMode
      ),
    LocalNodeClientProtocols(LocalNodeClientProtocols),

    -- ** Sophie based eras
    SophieLedgerEra,


    -- ** Local State Query
    DebugLedgerState(..),
    ProtocolState(..),
    SerialisedDebugLedgerState(..),
    UTxO(..),

    -- ** Conversions
    --TODO: arrange not to export these
    toSophieNetwork,
    fromSophiePParams,

  ) where

import           Bcc.Api
import           Bcc.Api.Address
import           Bcc.Api.Certificate
import           Bcc.Api.Eras
import           Bcc.Api.IPC
import           Bcc.Api.KeysOptimum
import           Bcc.Api.KeysSophie
import           Bcc.Api.NetworkId
import           Bcc.Api.OperationalCertificate
import           Bcc.Api.ProtocolParameters
import           Bcc.Api.Query
import           Bcc.Api.Script
import           Bcc.Api.Sophie.Genesis
import           Bcc.Api.StakePoolMetadata
import           Bcc.Api.Tx
import           Bcc.Api.TxBody
import           Bcc.Api.TxMetadata
import           Bcc.Api.Value
