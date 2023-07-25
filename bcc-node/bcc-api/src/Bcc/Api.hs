-- | This module provides a library interface for interacting with Bcc as
-- a user of the system.
--
-- It is intended to be the complete API covering everything but without exposing
-- constructors that reveal any lower level types.
--
-- In the interest of simplicity it glosses over some details of the system.
-- Most simple tools should be able to work just using this interface,
-- however you can go deeper and expose the types from the underlying libraries
-- using "Bcc.Api.Cole" or "Bcc.Api.Sophie".
--

module Bcc.Api (
    -- * Eras
    ColeEra,
    SophieEra,
    EvieEra,
    JenEra,
    AurumEra,
    BccEra(..),
    IsBccEra(..),
    AnyBccEra(..),
    anyBccEra,
    InAnyBccEra(..),

    -- ** Sophie-based eras
    SophieBasedEra(..),
    IsSophieBasedEra(..),
    InAnySophieBasedEra(..),
    BccEraStyle(..),
    bccEraStyle,
    sophieBasedToBccEra,

    -- ** Deprecated
    Cole,
    Sophie,
    Evie,
    Jen,
    -- * Type tags
    HasTypeProxy(..),
    AsType(..),
    -- * Cryptographic key interface
    -- $keys
    Key,
    VerificationKey,
    SigningKey(..),
    getVerificationKey,
    verificationKeyHash,
    castVerificationKey,
    castSigningKey,

    -- ** Generating keys
    generateSigningKey,
    deterministicSigningKey,
    deterministicSigningKeySeedSize,

    -- ** Hashes
    -- | In Bcc most keys are identified by their hash, and hashes are
    -- used in many other places.
    Hash,
    castHash,

    -- * Payment addresses
    -- | Constructing and inspecting normal payment addresses
    Address,
    ColeAddr,
    SophieAddr,
    NetworkId(..),
    -- ** Cole addresses
    makeColeAddress,
    ColeKey,
    ColeKeyLegacy,

    -- ** Sophie addresses
    makeSophieAddress,
    PaymentCredential(..),
    StakeAddressPointer(..),
    StakeAddressReference(..),
    PaymentKey,
    PaymentExtendedKey,

    -- ** Addresses in any era
    AddressAny(..),

    -- ** Addresses in specific eras
    AddressInEra(..),
    isKeyAddress,
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
    StakeAddress,
    StakeCredential,
    makeStakeAddress,
    StakeKey,
    StakeExtendedKey,

    -- * Currency values
    -- ** Bcc \/ Entropic
    Entropic,

    -- ** Multi-asset values
    Quantity(..),
    PolicyId(..),
    scriptPolicyId,
    AssetName(..),
    AssetId(..),
    Value,
    selectAsset,
    valueFromList,
    valueToList,
    filterValue,
    negateValue,
    ValueNestedRep(..),
    ValueNestedBundle(..),
    valueToNestedRep,
    valueFromNestedRep,
    renderValue,
    renderValuePretty,

    -- ** Bcc \/ Entropic within multi-asset values
    quantityToEntropic,
    entropicToQuantity,
    selectEntropic,
    entropicToValue,
    valueToEntropic,

    -- * Blocks

    -- ** Blocks in the context of an era
    Block(Block),
    BlockHeader(..),

    -- ** Points on the chain
    ChainPoint(..),
    EpochNo(..),

    -- ** Tip of the chain
    ChainTip(..),
    BlockNo(..),
    chainTipToChainPoint,

    -- * Building transactions

    -- * Building transactions
    -- | Constructing and inspecting transactions

    -- ** Transaction bodies
    TxBody(TxBody),
    makeTransactionBody,
    TxBodyContent(..),
    TxBodyError(..),
    TxBodyScriptData(..),

    -- ** Transaction Ids
    TxId(..),
    getTxId,

    -- ** Transaction inputs
    TxIn(TxIn),
    TxIx(TxIx),

    -- ** Transaction outputs
    TxOut(TxOut),
    TxOutValue(..),
    serialiseAddressForTxOut,
    TxOutDatumHash(..),

    -- ** Other transaction body types
    TxInsCollateral(..),
    TxFee(..),
    TxValidityLowerBound(..),
    TxValidityUpperBound(..),
    SlotNo(..),
    EpochSlots(..),
    TxMetadataInEra(..),
    TxAuxScripts(..),
    TxExtraScriptData(..),
    TxExtraKeyWitnesses(..),
    TxWithdrawals(..),
    TxCertificates(..),
    TxUpdateProposal(..),
    TxMintValue(..),

    -- ** Building vs viewing transactions
    BuildTxWith(..),
    BuildTx,
    ViewTx,

    -- ** Era-dependent transaction body features
    CollateralSupportedInEra(..),
    MultiAssetSupportedInEra(..),
    OnlyBccSupportedInEra(..),
    TxFeesExplicitInEra(..),
    TxFeesImplicitInEra(..),
    ValidityUpperBoundSupportedInEra(..),
    ValidityNoUpperBoundSupportedInEra(..),
    ValidityLowerBoundSupportedInEra(..),
    TxMetadataSupportedInEra(..),
    AuxScriptsSupportedInEra(..),
    TxExtraKeyWitnessesSupportedInEra(..),
    ScriptDataSupportedInEra(..),
    WithdrawalsSupportedInEra(..),
    CertificatesSupportedInEra(..),
    UpdateProposalSupportedInEra(..),

    -- ** Feature availability functions
    collateralSupportedInEra,
    multiAssetSupportedInEra,
    txFeesExplicitInEra,
    validityUpperBoundSupportedInEra,
    validityNoUpperBoundSupportedInEra,
    validityLowerBoundSupportedInEra,
    txMetadataSupportedInEra,
    auxScriptsSupportedInEra,
    extraKeyWitnessesSupportedInEra,
    withdrawalsSupportedInEra,
    certificatesSupportedInEra,
    updateProposalSupportedInEra,
    scriptDataSupportedInEra,

    -- ** Fee calculation
    transactionFee,
    estimateTransactionFee,
    evaluateTransactionFee,
    estimateTransactionKeyWitnessCount,

    -- ** Minimum required UTxO calculation
    calculateMinimumUTxO,
    MinimumUTxOError,

    -- ** Script execution units
    evaluateTransactionExecutionUnits,
    ScriptExecutionError(..),
    TransactionValidityIntervalError(..),

    -- ** Transaction balance
    evaluateTransactionBalance,

    -- ** Building transactions with automated fees and balancing
    makeTransactionBodyAutoBalance,
    BalancedTxBody(..),
    TxBodyErrorAutoBalance(..),
    TxScriptValidity(..),
    ScriptValidity(..),
    TxScriptValiditySupportedInEra(..),
    scriptValidityToTxScriptValidity,
    txScriptValiditySupportedInSophieBasedEra,
    txScriptValiditySupportedInBccEra,

    -- * Signing transactions
    -- | Creating transaction witnesses one by one, or all in one go.
    Tx(Tx),
    getTxBody,
    getTxWitnesses,

    -- ** Signing in one go
    signColeTransaction,
    signSophieTransaction,

    -- ** Incremental signing and separate witnesses
    makeSignedTransaction,
    KeyWitness,
    makeColeKeyWitness,
    SophieWitnessSigningKey(..),
    makeSophieKeyWitness,
    makeSophieBootstrapWitness,

    -- * Transaction metadata
    -- | Embedding additional structured data within transactions.
    TxMetadata(..),

    -- ** Constructing metadata
    TxMetadataValue(..),
    makeTransactionMetadata,

    -- ** Validating metadata
    validateTxMetadata,
    TxMetadataRangeError (..),

    -- ** Converstion to\/from JSON
    TxMetadataJsonSchema (..),
    metadataFromJson,
    metadataToJson,
    metadataValueToJsonNoSchema,
    TxMetadataJsonError (..),
    TxMetadataJsonSchemaError (..),

    -- * Certificates
    Certificate(..),

    -- ** Registering stake address and delegating
    -- | Certificates that are embedded in transactions for registering and
    -- unregistering stake address, and for setting the stake pool delegation
    -- choice for a stake address.
    makeStakeAddressRegistrationCertificate,
    makeStakeAddressDeregistrationCertificate,
    makeStakeAddressDelegationCertificate,

    -- ** Registering stake pools
    -- | Certificates that are embedded in transactions for registering and
    -- retiring stake pools. This includes updating the stake pool parameters.
    makeStakePoolRegistrationCertificate,
    makeStakePoolRetirementCertificate,
    StakePoolParameters,
    StakePoolRelay,
    StakePoolMetadataReference,

    -- * Stake pool off-chain metadata
    StakePoolMetadata,
    validateAndHashStakePoolMetadata,
    StakePoolMetadataValidationError,

    -- * Scripts
    -- | Both 'PaymentCredential's and 'StakeCredential's can use scripts.

    -- ** Script languages
    SimpleScriptV1,
    SimpleScriptV2,
    ZerepochScriptV1,
    ScriptLanguage(..),
    SimpleScriptVersion(..),
    ZerepochScriptVersion(..),
    AnyScriptLanguage(..),
    AnyZerepochScriptVersion(..),
    IsScriptLanguage(..),
    IsSimpleScriptLanguage(..),

    -- ** Scripts in a specific language
    Script(..),

    -- ** Scripts in any language
    ScriptInAnyLang(..),
    toScriptInAnyLang,

    -- ** Scripts in a specific era
    ScriptInEra(..),
    toScriptInEra,
    eraOfScriptInEra,

    -- ** Use of a script in an era as a witness
    WitCtxTxIn, WitCtxMint, WitCtxStake,
    WitCtx(..),
    ScriptWitness(..),
    Witness(..),
    KeyWitnessInCtx(..),
    ScriptWitnessInCtx(..),
    ScriptDatum(..),
    ScriptRedeemer,
    scriptWitnessScript,

    -- ** Inspecting 'ScriptWitness'es
    AnyScriptWitness(..),
    ScriptWitnessIndex(..),
    renderScriptWitnessIndex,
    collectTxBodyScriptWitnesses,
    mapTxScriptWitnesses,

    -- ** Languages supported in each era
    ScriptLanguageInEra(..),
    scriptLanguageSupportedInEra,
    languageOfScriptLanguageInEra,
    eraOfScriptLanguageInEra,

    -- ** Simple scripts
    -- | Making multi-signature and time-lock scripts.
    SimpleScript(..),
    TimeLocksSupported(..),
    timeLocksSupported,
    adjustSimpleScriptVersion,

    -- ** Zerepoch scripts
    ZerepochScript,
    exampleZerepochScriptAlwaysSucceeds,
    exampleZerepochScriptAlwaysFails,

    -- ** Script data
    ScriptData(..),
    hashScriptData,

    -- ** Validation
    ScriptDataRangeError (..),
    validateScriptData,

    -- ** Conversion to\/from JSON
    ScriptDataJsonSchema (..),
    scriptDataFromJson,
    scriptDataToJson,
    ScriptDataJsonError (..),
    ScriptDataJsonSchemaError (..),

    -- ** Script execution units
    ExecutionUnits(..),
    ExecutionUnitPrices(..),
    CostModel(..),
    validateCostModel,

    -- ** Script addresses
    -- | Making addresses from scripts.
    ScriptHash,
    hashScript,

    -- * Serialisation
    -- | Support for serialising data in JSON, CBOR and text files.

    -- ** CBOR
    SerialiseAsCBOR,
    ToCBOR,
    FromCBOR,
    serialiseToCBOR,
    deserialiseFromCBOR,

    -- ** JSON
    ToJSON,
    FromJSON,
    serialiseToJSON,
    deserialiseFromJSON,
    JsonDecodeError(..),
    readFileJSON,
    writeFileJSON,
    prettyPrintJSON,

    -- ** Bech32
    SerialiseAsBech32,
    serialiseToBech32,
    deserialiseFromBech32,
    deserialiseAnyOfFromBech32,
    Bech32DecodeError(..),

    -- ** Addresses
    -- | Address serialisation is (sadly) special
    SerialiseAddress,
    serialiseAddress,
    deserialiseAddress,

    -- ** Raw binary
    -- | Some types have a natural raw binary format.
    SerialiseAsRawBytes,
    serialiseToRawBytes,
    deserialiseFromRawBytes,
    serialiseToRawBytesHex,
    deserialiseFromRawBytesHex,
    serialiseToRawBytesHexText,

    -- ** Text envelope
    -- | Support for a envelope file format with text headers and a hex-encoded
    -- binary payload.
    HasTextEnvelope(..),
    TextEnvelope(..),
    TextEnvelopeType(..),
    TextEnvelopeDescr,
    TextEnvelopeError(..),
    textEnvelopeRawCBOR,
    serialiseToTextEnvelope,
    deserialiseFromTextEnvelope,
    readFileTextEnvelope,
    writeFileTextEnvelope,
    writeFileTextEnvelopeWithOwnerPermissions,
    readTextEnvelopeFromFile,
    readTextEnvelopeOfTypeFromFile,
    -- *** Reading one of several key types
    FromSomeType(..),
    deserialiseFromTextEnvelopeAnyOf,
    readFileTextEnvelopeAnyOf,

    -- * Errors
    Error(..),
    throwErrorAsException,
    FileError(..),

    -- * Node interaction
    -- | Operations that involve talking to a local Bcc node.

    -- ** Queries
    -- ** Submitting transactions

    -- ** High level protocol interaction with a Bcc node
    -- *** Initialization / Accumulation
    Env(..),
    envSecurityParam,
    LedgerState(..),
    initialLedgerState,
    applyBlock,
    ValidationMode(..),

    -- *** Traversing the block chain
    foldBlocks,
    chainSyncClientWithLedgerState,
    chainSyncClientPipelinedWithLedgerState,

    -- *** Errors
    FoldBlocksError(..),
    GenesisConfigError(..),
    InitialLedgerStateError(..),
    renderFoldBlocksError,
    renderGenesisConfigError,
    renderInitialLedgerStateError,

    -- ** Low level protocol interaction with a Bcc node
    connectToLocalNode,
    connectToLocalNodeWithVersion,
    LocalNodeConnectInfo(..),
    AnyConsensusMode(..),
    renderMode,
    ConsensusMode(BccMode),
    consensusModeOnly,
    ConsensusModeIsMultiEra(..),
    AnyConsensusModeParams(..),
    ConsensusModeParams(..),
    EraInMode(..),
    toEraInMode,
    LocalNodeClientProtocols(..),
    LocalChainSyncClient(..),
    BccMode,
--  connectToRemoteNode,

    -- *** Chain sync protocol
    -- | To construct a @ChainSyncClient@ see @Bcc.Api.Client@ or
    -- @Bcc.Api.ClientPipelined@.
    ChainSyncClient(..),
    ChainSyncClientPipelined(..),
    BlockInMode(..),
    LocalNodeClientProtocolsInMode,

    -- *** Local tx submission
    LocalTxSubmissionClient,
    TxInMode(..),
    TxValidationErrorInMode(..),
    runLocalTxSubmissionClient,
    submitTxToNodeLocal,

    -- *** Local state query
    LocalStateQueryClient(..),
    QueryInMode(..),
    QueryInEra(..),
    QueryInSophieBasedEra(..),
    QueryUTxOFilter(..),
    UTxO(..),
    queryNodeLocalState,

    EraHistory(..),
    getProgress,

    -- *** Common queries
    getLocalChainTip,

    -- * Node operation
    -- | Support for the steps needed to operate a node

    -- ** Operational certificates
    OperationalCertificate,
    OperationalCertificateIssueCounter,
    OperationalCertIssueError,
    issueOperationalCertificate,

    -- * Genesis file
    -- | Types and functions needed to inspect or create a genesis file.
    GenesisKey,
    GenesisExtendedKey,
    GenesisDelegateKey,
    GenesisDelegateExtendedKey,
    GenesisUTxOKey,
    genesisUTxOPseudoTxIn,

    -- GenesisVestedKeys
    GenesisVestedKey,
    GenesisVestedExtendedKey,
    GenesisVestedDelegateKey,
    GenesisVestedDelegateExtendedKey,

    -- ** Genesis paramaters
    GenesisParameters(..),

-- | Types and functions needed to inspect or create a genesis file.
    VestedKey,
    VestedExtendedKey,
    VestedDelegateKey,
    VestedDelegateExtendedKey,
    VestedUTxOKey,
    -- * Special transactions
    -- | There are various additional things that can be embedded in a
    -- transaction for special operations.
    makeMIRCertificate,
    makeGenesisKeyDelegationCertificate,
    makeVestedKeyDelegationCertificate,
    MIRTarget (..),

    -- * Protocol parameter updates
    UpdateProposal(..),
    ProtocolParametersUpdate(..),
    makeSophieUpdateProposal,
    OptimumNonce,
    makeOptimumNonce,

    NetworkMagic(..),

    -- ** Conversions
    toLedgerPParams,
    fromLedgerPParams,
    --TODO: arrange not to export these
    toNetworkMagic,
    fromLedgerTxOuts,
    toLedgerUTxO,
    --TODO: Remove after updating bcc-node-chairman with new IPC
    SomeNodeClientProtocol(..),

    SlotsInEpoch(..),
    SlotsToEpochEnd(..),
    slotToEpoch,

    NodeToClientVersion(..),

    -- ** Monadic queries
    LocalStateQueryExpr,
    executeLocalStateQueryExpr,
    executeLocalStateQueryExprWithChainSync,
    queryExpr,
    determineEraExpr
  ) where

import           Bcc.Api.Address
import           Bcc.Api.Block
import           Bcc.Api.Certificate
import           Bcc.Api.Eras
import           Bcc.Api.Error
import           Bcc.Api.Fees
import           Bcc.Api.GenesisParameters
import           Bcc.Api.HasTypeProxy
import           Bcc.Api.Hash
import           Bcc.Api.IPC
import           Bcc.Api.IPC.Monad
import           Bcc.Api.Key
import           Bcc.Api.KeysCole
import           Bcc.Api.KeysOptimum
import           Bcc.Api.KeysSophie
import           Bcc.Api.LedgerState
import           Bcc.Api.Modes
import           Bcc.Api.NetworkId
import           Bcc.Api.OperationalCertificate
import           Bcc.Api.ProtocolParameters
import           Bcc.Api.Query hiding (LedgerState (..))
import           Bcc.Api.Script
import           Bcc.Api.ScriptData
import           Bcc.Api.SerialiseBech32
import           Bcc.Api.SerialiseCBOR
import           Bcc.Api.SerialiseJSON
import           Bcc.Api.SerialiseRaw
import           Bcc.Api.SerialiseTextEnvelope
import           Bcc.Api.StakePoolMetadata
import           Bcc.Api.Tx
import           Bcc.Api.TxBody
import           Bcc.Api.TxMetadata
import           Bcc.Api.Value
--TODO: Remove after updating bcc-node-chairman with new IPC
import           Bcc.Api.Protocol.Types
