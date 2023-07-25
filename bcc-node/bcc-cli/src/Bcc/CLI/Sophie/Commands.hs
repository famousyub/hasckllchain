{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Sophie CLI command types
module Bcc.CLI.Sophie.Commands
  ( -- * CLI command types
    SophieCommand (..)
  , AddressCmd (..)
  , StakeAddressCmd (..)
  , KeyCmd (..)
  , TransactionCmd (..)
  , NodeCmd (..)
  , PoolCmd (..)
  , QueryCmd (..)
  , GovernanceCmd (..)
  , GenesisCmd (..)
  , TextViewCmd (..)
  , renderSophieCommand

    -- * CLI flag types
  , AddressKeyType (..)
  , ColeKeyType (..)
  , ColeKeyFormat (..)
  , BccAddressKeyType (..)
  , GenesisDir (..)
  , VestedDir (..)
  , TxInCount (..)
  , TxOutCount (..)
  , TxSophieWitnessCount (..)
  , TxColeWitnessCount (..)
  , SomeKeyFile (..)
  , OpCertCounterFile (..)
  , OutputFile (..)
  , ProtocolParamsFile (..)
  , ProtocolParamsSourceSpec (..)
  , WitnessFile (..)
  , TxBodyFile (..)
  , TxFile (..)
  , InputTxFile (..)
  , VerificationKeyBase64 (..)
  , GenesisKeyFile (..)
  , VestedKeyFile (..)
  , MetadataFile (..)
  , PoolId (..)
  , PoolMetadataFile (..)
  , PrivKeyFile (..)
  , BlockId (..)
  , WitnessSigningData (..)
  , ColdVerificationKeyOrFile (..)
  ) where

import           Data.Text (Text)
import           Prelude

import           Bcc.Api.Sophie hiding (PoolId)

import           Shardagnostic.Consensus.BlockchainTime (SystemStart (..))

import           Bcc.CLI.Sophie.Key (PaymentVerifier, StakeVerifier, VerificationKeyOrFile,
                   VerificationKeyOrHashOrFile, VerificationKeyTextOrFile)
import           Bcc.CLI.Types

import           Sophie.Spec.Ledger.TxBody (MIRPot)
--
-- Sophie CLI command data types
--

-- | All the CLI subcommands under \"sophie\".
--
data SophieCommand
  = AddressCmd      AddressCmd
  | StakeAddressCmd StakeAddressCmd
  | KeyCmd          KeyCmd
  | TransactionCmd  TransactionCmd
  | NodeCmd         NodeCmd
  | PoolCmd         PoolCmd
  | QueryCmd        QueryCmd
  | GovernanceCmd   GovernanceCmd
  | GenesisCmd      GenesisCmd
  | TextViewCmd     TextViewCmd
  deriving Show

renderSophieCommand :: SophieCommand -> Text
renderSophieCommand sc =
  case sc of
    AddressCmd cmd -> renderAddressCmd cmd
    StakeAddressCmd cmd -> renderStakeAddressCmd cmd
    KeyCmd cmd -> renderKeyCmd cmd
    TransactionCmd cmd -> renderTransactionCmd cmd
    NodeCmd cmd -> renderNodeCmd cmd
    PoolCmd cmd -> renderPoolCmd cmd
    QueryCmd cmd -> renderQueryCmd cmd
    GovernanceCmd cmd -> renderGovernanceCmd cmd
    GenesisCmd cmd -> renderGenesisCmd cmd
    TextViewCmd cmd -> renderTextViewCmd cmd

data AddressCmd
  = AddressKeyGen AddressKeyType VerificationKeyFile SigningKeyFile
  | AddressKeyHash VerificationKeyTextOrFile (Maybe OutputFile)
  | AddressBuild
      PaymentVerifier
      (Maybe StakeVerifier)
      NetworkId
      (Maybe OutputFile)
  | AddressBuildMultiSig ScriptFile NetworkId (Maybe OutputFile)
  | AddressInfo Text (Maybe OutputFile)
  deriving Show


renderAddressCmd :: AddressCmd -> Text
renderAddressCmd cmd =
  case cmd of
    AddressKeyGen {} -> "address key-gen"
    AddressKeyHash {} -> "address key-hash"
    AddressBuild {} -> "address build"
    AddressBuildMultiSig {} -> "address build-script"
    AddressInfo {} -> "address info"

data StakeAddressCmd
  = StakeAddressKeyGen VerificationKeyFile SigningKeyFile
  | StakeAddressKeyHash (VerificationKeyOrFile StakeKey) (Maybe OutputFile)
  | StakeAddressBuild (VerificationKeyOrFile StakeKey) NetworkId (Maybe OutputFile)
  | StakeRegistrationCert StakeVerifier OutputFile
  | StakeCredentialDelegationCert
      StakeVerifier
      (VerificationKeyOrHashOrFile StakePoolKey)
      OutputFile
  | StakeCredentialDeRegistrationCert StakeVerifier OutputFile
  deriving Show

renderStakeAddressCmd :: StakeAddressCmd -> Text
renderStakeAddressCmd cmd =
  case cmd of
    StakeAddressKeyGen {} -> "stake-address key-gen"
    StakeAddressKeyHash {} -> "stake-address key-hash"
    StakeAddressBuild {} -> "stake-address build"
    StakeRegistrationCert {} -> "stake-address registration-certificate"
    StakeCredentialDelegationCert {} -> "stake-address delegation-certificate"
    StakeCredentialDeRegistrationCert {} -> "stake-address deregistration-certificate"

data KeyCmd
  = KeyGetVerificationKey SigningKeyFile VerificationKeyFile
  | KeyNonExtendedKey  VerificationKeyFile VerificationKeyFile
  | KeyConvertColeKey (Maybe Text) ColeKeyType SomeKeyFile OutputFile
  | KeyConvertColeGenesisVKey VerificationKeyBase64 OutputFile
  | KeyConvertColeVestedVKey VerificationKeyBase64 OutputFile
  | KeyConvertITNStakeKey SomeKeyFile OutputFile
  | KeyConvertITNExtendedToStakeKey SomeKeyFile OutputFile
  | KeyConvertITNBip32ToStakeKey SomeKeyFile OutputFile
  | KeyConvertBccAddressSigningKey BccAddressKeyType SigningKeyFile OutputFile
  deriving Show

renderKeyCmd :: KeyCmd -> Text
renderKeyCmd cmd =
  case cmd of
    KeyGetVerificationKey {} -> "key verification-key"
    KeyNonExtendedKey {} -> "key non-extended-key"
    KeyConvertColeKey {} -> "key convert-cole-key"
    KeyConvertColeGenesisVKey {} -> "key convert-cole-genesis-key"
    KeyConvertColeVestedVKey {} -> "key convert-cole-vested-key"
    KeyConvertITNStakeKey {} -> "key convert-itn-key"
    KeyConvertITNExtendedToStakeKey {} -> "key convert-itn-extended-key"
    KeyConvertITNBip32ToStakeKey {} -> "key convert-itn-bip32-key"
    KeyConvertBccAddressSigningKey {} -> "key convert-bcc-address-signing-key"

data TransactionCmd
  = TxBuildRaw
      AnyBccEra
      (Maybe ScriptValidity) -- ^ Mark script as expected to pass or fail validation
      [(TxIn, Maybe (ScriptWitnessFiles WitCtxTxIn))]
      -- ^ Transaction inputs with optional spending scripts
      [TxIn]
      -- ^ Transaction inputs for collateral, only key witnesses, no scripts.
      [WitnessSigningData]
      -- ^ Required signers
      [TxOutAnyEra]
      (Maybe (Value, [ScriptWitnessFiles WitCtxMint]))
      -- ^ Multi-Asset value with script witness
      (Maybe SlotNo)
      -- ^ Transaction lower bound
      (Maybe SlotNo)
      -- ^ Transaction upper bound
      (Maybe Entropic)
      -- ^ Tx fee
      [(CertificateFile, Maybe (ScriptWitnessFiles WitCtxStake))]
      -- ^ Certificates with potential script witness
      [(StakeAddress, Entropic, Maybe (ScriptWitnessFiles WitCtxStake))]
      TxMetadataJsonSchema
      [ScriptFile]
      -- ^ Auxillary scripts
      [MetadataFile]
      (Maybe ProtocolParamsSourceSpec)
      (Maybe UpdateProposalFile)
      TxBodyFile

    -- | Like 'TxBuildRaw' but without the fee, and with a change output.
  | TxBuild
      AnyBccEra
      AnyConsensusModeParams
      NetworkId
      (Maybe ScriptValidity) -- ^ Mark script as expected to pass or fail validation
      (Maybe Word)
      -- ^ Override the required number of tx witnesses
      [(TxIn, Maybe (ScriptWitnessFiles WitCtxTxIn))]
      -- ^ Required signers
      [WitnessSigningData]
      -- ^ Transaction inputs with optional spending scripts
      [TxIn]
      -- ^ Transaction inputs for collateral, only key witnesses, no scripts.
      [TxOutAnyEra]
      -- ^ Normal outputs
      TxOutChangeAddress
      -- ^ A change output
      (Maybe (Value, [ScriptWitnessFiles WitCtxMint]))
      -- ^ Multi-Asset value with script witness
      (Maybe SlotNo)
      -- ^ Transaction lower bound
      (Maybe SlotNo)
      -- ^ Transaction upper bound
      [(CertificateFile, Maybe (ScriptWitnessFiles WitCtxStake))]
      -- ^ Certificates with potential script witness
      [(StakeAddress, Entropic, Maybe (ScriptWitnessFiles WitCtxStake))]
      -- ^ Withdrawals with potential script witness
      TxMetadataJsonSchema
      [ScriptFile]
      -- ^ Auxillary scripts
      [MetadataFile]
      (Maybe ProtocolParamsSourceSpec)
      (Maybe UpdateProposalFile)
      TxBodyFile
  | TxSign TxBodyFile [WitnessSigningData] (Maybe NetworkId) TxFile
  | TxCreateWitness TxBodyFile WitnessSigningData (Maybe NetworkId) OutputFile
  | TxAssembleTxBodyWitness TxBodyFile [WitnessFile] OutputFile
  | TxSubmit AnyConsensusModeParams NetworkId FilePath
  | TxMintedPolicyId ScriptFile
  | TxCalculateMinFee
      TxBodyFile
      (Maybe NetworkId)
      ProtocolParamsSourceSpec
      TxInCount
      TxOutCount
      TxSophieWitnessCount
      TxColeWitnessCount
  | TxCalculateMinRequiredUTxO
      AnyBccEra
      ProtocolParamsSourceSpec
      TxOutAnyEra
  | TxHashScriptData
      ScriptDataOrFile
  | TxGetTxId InputTxFile
  | TxView InputTxFile
  deriving Show

data InputTxFile = InputTxBodyFile TxBodyFile | InputTxFile TxFile
  deriving Show

data ProtocolParamsSourceSpec
  = ParamsFromGenesis !GenesisFile
    -- ^ We allow an appropriately forewarned user to obtain protocol params
    --   directly from the genesis file, which allows them to avoid running
    --   the node in case they would like to estimate the fee using the
    --   blockchain's initial protocol parameters.
  | ParamsFromFile !ProtocolParamsFile
    -- ^ Obtain protocol parameters from a file structured by the
    --   'bcc-api' 'ProtocolParameters' data type.
  deriving Show

renderTransactionCmd :: TransactionCmd -> Text
renderTransactionCmd cmd =
  case cmd of
    TxBuild {} -> "transaction build"
    TxBuildRaw {} -> "transaction build-raw"
    TxSign {} -> "transaction sign"
    TxCreateWitness {} -> "transaction witness"
    TxAssembleTxBodyWitness {} -> "transaction sign-witness"
    TxSubmit {} -> "transaction submit"
    TxMintedPolicyId {} -> "transaction policyid"
    TxCalculateMinFee {} -> "transaction calculate-min-fee"
    TxCalculateMinRequiredUTxO {} -> "transaction calculate-min-value"
    TxHashScriptData {} -> "transaction hash-script-data"
    TxGetTxId {} -> "transaction txid"
    TxView {} -> "transaction view"

data NodeCmd
  = NodeKeyGenCold VerificationKeyFile SigningKeyFile OpCertCounterFile
  | NodeKeyGenKES  VerificationKeyFile SigningKeyFile
  | NodeKeyGenVRF  VerificationKeyFile SigningKeyFile
  | NodeKeyHashVRF  (VerificationKeyOrFile VrfKey) (Maybe OutputFile)
  | NodeNewCounter ColdVerificationKeyOrFile Word OpCertCounterFile
  | NodeIssueOpCert (VerificationKeyOrFile KesKey) SigningKeyFile OpCertCounterFile
                    KESPeriod OutputFile
  deriving Show

renderNodeCmd :: NodeCmd -> Text
renderNodeCmd cmd = do
  case cmd of
    NodeKeyGenCold {} -> "node key-gen"
    NodeKeyGenKES {} -> "node key-gen-KES"
    NodeKeyGenVRF {} -> "node key-gen-VRF"
    NodeKeyHashVRF {} -> "node key-hash-VRF"
    NodeNewCounter {} -> "node new-counter"
    NodeIssueOpCert{} -> "node issue-op-cert"


data PoolCmd
  = PoolRegistrationCert
      (VerificationKeyOrFile StakePoolKey)
      -- ^ Stake pool verification key.
      (VerificationKeyOrFile VrfKey)
      -- ^ VRF Verification key.
      Entropic
      -- ^ Pool pledge.
      Entropic
      -- ^ Pool cost.
      Rational
      -- ^ Pool margin.
      (VerificationKeyOrFile StakeKey)
      -- ^ Reward account verification staking key.
      [VerificationKeyOrFile StakeKey]
      -- ^ Pool owner verification staking key(s).
      [StakePoolRelay]
      -- ^ Stake pool relays.
      (Maybe StakePoolMetadataReference)
      -- ^ Stake pool metadata.
      NetworkId
      OutputFile
  | PoolRetirementCert
      (VerificationKeyOrFile StakePoolKey)
      -- ^ Stake pool verification key.
      EpochNo
      -- ^ Epoch in which to retire the stake pool.
      OutputFile
  | PoolGetId (VerificationKeyOrFile StakePoolKey) OutputFormat
  | PoolMetadataHash PoolMetadataFile (Maybe OutputFile)
  deriving Show

renderPoolCmd :: PoolCmd -> Text
renderPoolCmd cmd =
  case cmd of
    PoolRegistrationCert {} -> "stake-pool registration-certificate"
    PoolRetirementCert {} -> "stake-pool deregistration-certificate"
    PoolGetId {} -> "stake-pool id"
    PoolMetadataHash {} -> "stake-pool metadata-hash"

data QueryCmd =
    QueryProtocolParameters' AnyConsensusModeParams NetworkId (Maybe OutputFile)
  | QueryTip AnyConsensusModeParams NetworkId (Maybe OutputFile)
  | QueryStakePools' AnyConsensusModeParams NetworkId (Maybe OutputFile)
  | QueryStakeDistribution' AnyConsensusModeParams NetworkId (Maybe OutputFile)
  | QueryStakeAddressInfo AnyConsensusModeParams StakeAddress NetworkId (Maybe OutputFile)
  | QueryUTxO' AnyConsensusModeParams QueryUTxOFilter NetworkId (Maybe OutputFile)
  | QueryDebugLedgerState' AnyConsensusModeParams NetworkId (Maybe OutputFile)
  | QueryProtocolState' AnyConsensusModeParams NetworkId (Maybe OutputFile)
  | QueryStakeSnapshot' AnyConsensusModeParams NetworkId (Hash StakePoolKey)
  | QueryPoolParams' AnyConsensusModeParams NetworkId (Hash StakePoolKey)
  deriving Show

renderQueryCmd :: QueryCmd -> Text
renderQueryCmd cmd =
  case cmd of
    QueryProtocolParameters' {} -> "query protocol-parameters "
    QueryTip {} -> "query tip"
    QueryStakePools' {} -> "query stake-pools"
    QueryStakeDistribution' {} -> "query stake-distribution"
    QueryStakeAddressInfo {} -> "query stake-address-info"
    QueryUTxO' {} -> "query utxo"
    QueryDebugLedgerState' {} -> "query ledger-state"
    QueryProtocolState' {} -> "query protocol-state"
    QueryStakeSnapshot' {} -> "query stake-snapshot"
    QueryPoolParams' {} -> "query pool-params"

data GovernanceCmd
  = GovernanceMIRPayStakeAddressesCertificate
      MIRPot
      [StakeAddress]
      [Entropic]
      OutputFile
  | GovernanceMIRTransfer Entropic OutputFile TransferDirection
  | GovernanceGenesisKeyDelegationCertificate
      (VerificationKeyOrHashOrFile GenesisKey)
      (VerificationKeyOrHashOrFile GenesisDelegateKey)
      (VerificationKeyOrHashOrFile VrfKey)
      OutputFile
  | GovernanceVestedKeyDelegationCertificate
      (VerificationKeyOrHashOrFile VestedKey)
      (VerificationKeyOrHashOrFile VestedDelegateKey)
      (VerificationKeyOrHashOrFile VrfKey)
      OutputFile
  | GovernanceUpdateProposal OutputFile EpochNo
                             [VerificationKeyFile]
                             ProtocolParametersUpdate
  deriving Show

renderGovernanceCmd :: GovernanceCmd -> Text
renderGovernanceCmd cmd =
  case cmd of
    GovernanceGenesisKeyDelegationCertificate {} -> "governance create-genesis-key-delegation-certificate"
    GovernanceVestedKeyDelegationCertificate   {} -> "governance create-vested-key-delegation-certificate"
    GovernanceMIRPayStakeAddressesCertificate {} -> "governance create-mir-certificate stake-addresses"
    GovernanceMIRTransfer _ _ TransferToTreasury -> "governance create-mir-certificate transfer-to-treasury"
    GovernanceMIRTransfer _ _ TransferToReserves -> "governance create-mir-certificate transfer-to-reserves"
    GovernanceUpdateProposal {} -> "governance create-update-proposal"

data TextViewCmd
  = TextViewInfo !FilePath (Maybe OutputFile)
  deriving Show


renderTextViewCmd :: TextViewCmd -> Text
renderTextViewCmd (TextViewInfo _ _) = "text-view decode-cbor"

data GenesisCmd
  = GenesisCreate GenesisDir Word Word Word (Maybe SystemStart) (Maybe Entropic) NetworkId
  | GenesisCreateStaked GenesisDir Word Word Word Word Word (Maybe SystemStart) (Maybe Entropic) Entropic NetworkId Word Word Word
  | GenesisKeyGenGenesis VerificationKeyFile SigningKeyFile
  | GenesisKeyGenDelegate VerificationKeyFile SigningKeyFile OpCertCounterFile
  | GenesisKeyGenVested VerificationKeyFile SigningKeyFile
  | GenesisKeyGenVestedDelegate VerificationKeyFile SigningKeyFile OpCertCounterFile
  | GenesisKeyGenUTxO VerificationKeyFile SigningKeyFile
  | GenesisCmdKeyHash VerificationKeyFile
  | GenesisVerKey VerificationKeyFile SigningKeyFile
  | GenesisTxIn VerificationKeyFile NetworkId (Maybe OutputFile)
  | GenesisAddr VerificationKeyFile NetworkId (Maybe OutputFile)
  | GenesisHashFile GenesisFile
  deriving Show

renderGenesisCmd :: GenesisCmd -> Text
renderGenesisCmd cmd =
  case cmd of
    GenesisCreate {} -> "genesis create"
    GenesisCreateStaked {} -> "genesis create-staked"
    GenesisKeyGenGenesis {} -> "genesis key-gen-genesis"
    GenesisKeyGenDelegate {} -> "genesis key-gen-delegate"
    GenesisKeyGenVested {} -> "genesis key-gen-vested"
    GenesisKeyGenVestedDelegate {} -> "genesis key-gen-vesteddelegate"
    GenesisKeyGenUTxO {} -> "genesis key-gen-utxo"
    GenesisCmdKeyHash {} -> "genesis key-hash"
    GenesisVerKey {} -> "genesis get-ver-key"
    GenesisTxIn {} -> "genesis initial-txin"
    GenesisAddr {} -> "genesis initial-addr"
    GenesisHashFile {} -> "genesis hash"

--
-- Sophie CLI flag/option data types
--

newtype ProtocolParamsFile
  = ProtocolParamsFile FilePath
  deriving (Show, Eq)

newtype TxInCount
  = TxInCount Int
  deriving Show

newtype TxOutCount
  = TxOutCount Int
  deriving Show

newtype TxSophieWitnessCount
  = TxSophieWitnessCount Int
  deriving Show

newtype TxColeWitnessCount
  = TxColeWitnessCount Int
  deriving Show

newtype BlockId
  = BlockId String -- Probably not a String
  deriving Show

newtype GenesisKeyFile
  = GenesisKeyFile FilePath
  deriving Show

newtype VestedKeyFile
  = VestedKeyFile FilePath
  deriving Show

data MetadataFile = MetadataFileJSON FilePath
                  | MetadataFileCBOR FilePath

  deriving Show

newtype OutputFile
  = OutputFile FilePath
  deriving Show

newtype PoolId
  = PoolId String -- Probably not a String
  deriving Show

newtype PoolMetadataFile = PoolMetadataFile
  { unPoolMetadataFile :: FilePath }
  deriving Show

newtype GenesisDir
  = GenesisDir FilePath
  deriving Show

newtype VestedDir
  = VestedDir FilePath
  deriving Show

-- | Either a verification or signing key, used for conversions and other
-- commands that make sense for both.
--
data SomeKeyFile
  = AVerificationKeyFile VerificationKeyFile
  | ASigningKeyFile SigningKeyFile
  deriving Show

data AddressKeyType
  = AddressKeySophie
  | AddressKeySophieExtended
  | AddressKeyCole
  deriving Show

data ColeKeyType
  = ColePaymentKey        ColeKeyFormat
  | ColeGenesisKey        ColeKeyFormat
  | ColeGenesisVestedKey  ColeKeyFormat
  | ColeVestedKey         ColeKeyFormat
  | ColeDelegateKey       ColeKeyFormat
  | ColeVestedDelegateKey ColeKeyFormat
  deriving Show

data ColeKeyFormat = NonLegacyColeKeyFormat
                    | LegacyColeKeyFormat
  deriving Show

-- | The type of @bcc-address@ key.
data BccAddressKeyType
  = BccAddressSophiePaymentKey
  | BccAddressSophieStakeKey
  | BccAddressIcarusPaymentKey
  | BccAddressColePaymentKey
  deriving Show

newtype OpCertCounterFile
  = OpCertCounterFile FilePath
  deriving Show

newtype PrivKeyFile
  = PrivKeyFile FilePath
  deriving Show

newtype WitnessFile
  = WitnessFile FilePath
  deriving Show

newtype TxBodyFile
  = TxBodyFile FilePath
  deriving Show

newtype TxFile
  = TxFile FilePath
  deriving Show

-- | A raw verification key given in Base64, and decoded into a ByteString.
newtype VerificationKeyBase64
  = VerificationKeyBase64 String
  deriving Show

-- | Data required to construct a witness.
data WitnessSigningData
  = KeyWitnessSigningData
      !SigningKeyFile
      -- ^ Path to a file that should contain a signing key.
      !(Maybe (Address ColeAddr))
      -- ^ An optionally specified Cole address.
      --
      -- If specified, both the network ID and derivation path are extracted
      -- from the address and used in the construction of the Cole witness.
  deriving Show

-- | Either a stake pool verification key, genesis delegate verification key,
-- or a path to a cold verification key file.
--
-- Note that a "cold verification key" refers to either a stake pool or
-- genesis delegate verification key.
--
-- TODO: A genesis delegate extended key should also be valid here.
data ColdVerificationKeyOrFile
  = ColdStakePoolVerificationKey !(VerificationKey StakePoolKey)
  | ColdGenesisDelegateVerificationKey !(VerificationKey GenesisDelegateKey)
  | ColdGenesisVestedDelegateVerificationKey !(VerificationKey GenesisVestedDelegateKey)
  | ColdVestedDelegateVerificationKey !(VerificationKey VestedDelegateKey)
  | ColdVerificationKeyFile !VerificationKeyFile
  deriving Show
