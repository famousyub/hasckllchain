{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | The various Bcc protocol parameters, including:
--
-- * the current values of updateable protocol parameters: 'ProtocolParameters'
-- * updates to protocol parameters: 'ProtocolParametersUpdate'
-- * update proposals that can be embedded in transactions: 'UpdateProposal'
-- * parameters fixed in the genesis file: 'GenesisParameters'
--
module Bcc.Api.ProtocolParameters (
    -- * The updateable protocol paramaters
    ProtocolParameters(..),
    checkProtocolParameters,
    ProtocolParametersError(..),
    EpochNo,

    -- * Updates to the protocol paramaters
    ProtocolParametersUpdate(..),

    -- * OptimumNonce
    OptimumNonce,
    makeOptimumNonce,

    -- * Execution units, prices and cost models,
    ExecutionUnits(..),
    ExecutionUnitPrices(..),
    CostModel(..),
    validateCostModel,

    -- * Update proposals to change the protocol paramaters
    UpdateProposal(..),
    makeSophieUpdateProposal,

    -- * Internal conversion functions
    toLedgerUpdate,
    fromLedgerUpdate,
    toLedgerProposedPPUpdates,
    fromLedgerProposedPPUpdates,
    toLedgerPParams,
    fromLedgerPParams,
    fromSophiePParams,
    toAurumPrices,
    fromAurumPrices,
    toAurumScriptLanguage,
    fromAurumScriptLanguage,
    toAurumCostModel,
    fromAurumCostModel,

    -- * Data family instances
    AsType(..)
  ) where

import           Prelude

import           Control.Monad
import           Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.!=), (.:), (.:?),
                   (.=))
import           Data.Bifunctor (bimap)
import           Data.ByteString (ByteString)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe, isJust)
import           Data.String (IsString)
import           Data.Text (Text)
import           GHC.Generics
import           Numeric.Natural

import           Bcc.Api.Json
import qualified Bcc.Binary as CBOR
import qualified Bcc.Crypto.Hash.Class as Crypto
import           Bcc.Slotting.Slot (EpochNo)

import           Bcc.Ledger.BaseTypes (maybeToStrictMaybe, strictMaybeToMaybe)
import qualified Bcc.Ledger.BaseTypes as Ledger
import qualified Bcc.Ledger.Core as Ledger
import           Bcc.Ledger.Crypto (StandardCrypto)
import qualified Bcc.Ledger.Era as Ledger
import qualified Bcc.Ledger.Keys as Ledger
import qualified Sophie.Spec.Ledger.PParams as Ledger (ProposedPPUpdates (..), ProtVer (..),
                   Update (..))
-- Some of the things from Sophie.Spec.Ledger.PParams are generic across all
-- eras, and some are specific to the Sophie era (and other pre-Aurum eras).
-- So we import in twice under different names.
import qualified Sophie.Spec.Ledger.PParams as Sophie (PParams, PParams' (..), PParamsUpdate)

import qualified Bcc.Ledger.Aurum.Language as Aurum
import qualified Bcc.Ledger.Aurum.PParams as Aurum
import qualified Bcc.Ledger.Aurum.Scripts as Aurum

import           Bcc.Api.Address
import           Bcc.Api.Eras
import           Bcc.Api.Error
import           Bcc.Api.HasTypeProxy
import           Bcc.Api.Hash
import           Bcc.Api.KeysCole
import           Bcc.Api.KeysSophie
import           Bcc.Api.Script
import           Bcc.Api.SerialiseCBOR
import           Bcc.Api.SerialiseRaw
import           Bcc.Api.SerialiseTextEnvelope
import           Bcc.Api.SerialiseUsing
import           Bcc.Api.StakePoolMetadata
import           Bcc.Api.TxMetadata
import           Bcc.Api.Value


-- | The values of the set of /updateable/ protocol paramaters. At any
-- particular point on the chain there is a current set of paramaters in use.
--
-- These paramaters can be updated (at epoch boundaries) via an
-- 'UpdateProposal', which contains a 'ProtocolParametersUpdate'.
--
-- The 'ProtocolParametersUpdate' is essentially a diff for the
-- 'ProtocolParameters'.
--
-- There are also paramaters fixed in the Genesis file. See 'GenesisParameters'.
--
data ProtocolParameters =
     ProtocolParameters {

       -- | Protocol version, major and minor. Updating the major version is
       -- used to trigger hard forks.
       --
       protocolParamProtocolVersion :: (Natural, Natural),

       -- | The decentralization parameter. This is fraction of slots that
       -- belong to the BFT overlay schedule, rather than the Optimum schedule.
       -- So 1 means fully centralised, while 0 means fully decentralised.
       --
       -- This is the \"d\" parameter from the design document.
       --
       protocolParamDecentralization :: Rational,

       -- | Extra entropy for the Optimum per-epoch nonce.
       --
       -- This can be used to add extra entropy during the decentralisation
       -- process. If the extra entropy can be demonstrated to be generated
       -- randomly then this method can be used to show that the initial
       -- federated operators did not subtly bias the initial schedule so that
       -- they retain undue influence after decentralisation.
       --
       protocolParamExtraOptimumEntropy :: Maybe OptimumNonce,

       -- | The maximum permitted size of a block header.
       --
       -- This must be at least as big as the largest legitimate block headers
       -- but should not be too much larger, to help prevent DoS attacks.
       --
       -- Caution: setting this to be smaller than legitimate block headers is
       -- a sure way to brick the system!
       --
       protocolParamMaxBlockHeaderSize :: Natural,

       -- | The maximum permitted size of the block body (that is, the block
       -- payload, without the block header).
       --
       -- This should be picked with the Optimum network delta security parameter
       -- in mind. Making this too large can severely weaken the Optimum
       -- consensus properties.
       --
       -- Caution: setting this to be smaller than a transaction that can
       -- change the protocol parameters is a sure way to brick the system!
       --
       protocolParamMaxBlockBodySize :: Natural,

       -- | The maximum permitted size of a transaction.
       --
       -- Typically this should not be too high a fraction of the block size,
       -- otherwise wastage from block fragmentation becomes a problem, and
       -- the current implementation does not use any sophisticated box packing
       -- algorithm.
       --
       protocolParamMaxTxSize :: Natural,

       -- | The constant factor for the minimum fee calculation.
       --
       protocolParamTxFeeFixed :: Natural,

       -- | The linear factor for the minimum fee calculation.
       --
       protocolParamTxFeePerByte :: Natural,

       -- | The minimum permitted value for new UTxO entries, ie for
       -- transaction outputs.
       --
       protocolParamMinUTxOValue :: Maybe Entropic,

       -- | The deposit required to register a stake address.
       --
       protocolParamStakeAddressDeposit :: Entropic,

       -- | The deposit required to register a stake pool.
       --
       protocolParamStakePoolDeposit :: Entropic,

       -- | The minimum value that stake pools are permitted to declare for
       -- their cost parameter.
       --
       protocolParamMinPoolCost :: Entropic,

       -- | The maximum number of epochs into the future that stake pools
       -- are permitted to schedule a retirement.
       --
       protocolParamPoolRetireMaxEpoch :: EpochNo,

       -- | The equilibrium target number of stake pools.
       --
       -- This is the \"k\" incentives parameter from the design document.
       --
       protocolParamStakePoolTargetNum :: Natural,

       -- | The influence of the pledge in stake pool rewards.
       --
       -- This is the \"a_0\" incentives parameter from the design document.
       --
       protocolParamPoolPledgeInfluence :: Rational,

       -- | The monetary expansion rate. This determines the fraction of the
       -- reserves that are added to the fee pot each epoch.
       --
       -- This is the \"rho\" incentives parameter from the design document.
       --
       protocolParamMonetaryExpansion :: Rational,

       -- | The fraction of the fee pot each epoch that goes to the treasury.
       --
       -- This is the \"tau\" incentives parameter from the design document.
       --
       protocolParamTreasuryCut :: Rational,

       -- | Cost in bcc per word of UTxO storage.
       --
       -- /Introduced in Aurum/
       protocolParamUTxOCostPerWord :: Maybe Entropic,

       -- | Cost models for script languages that use them.
       --
       -- /Introduced in Aurum/
       protocolParamCostModels :: Map AnyZerepochScriptVersion CostModel,

       -- | Price of execution units for script languages that use them.
       --
       -- /Introduced in Aurum/
       protocolParamPrices :: Maybe ExecutionUnitPrices,

       -- | Max total script execution resources units allowed per tx
       --
       -- /Introduced in Aurum/
       protocolParamMaxTxExUnits :: Maybe ExecutionUnits,

       -- | Max total script execution resources units allowed per block
       --
       -- /Introduced in Aurum/
       protocolParamMaxBlockExUnits :: Maybe ExecutionUnits,

       -- | Max size of a Value in a tx ouput.
       --
       -- /Introduced in Aurum/
       protocolParamMaxValueSize :: Maybe Natural,

       -- | The percentage of the script contribution to the txfee that must be
       -- provided as collateral inputs when including Zerepoch scripts.
       --
       -- /Introduced in Aurum/
       protocolParamCollateralPercent :: Maybe Natural,

       -- | The maximum number of collateral inputs allowed in a transaction.
       --
       -- /Introduced in Aurum/
       protocolParamMaxCollateralInputs :: Maybe Natural
    }
  deriving (Eq, Generic, Show)

instance FromJSON ProtocolParameters where
  parseJSON =
    withObject "ProtocolParameters" $ \o -> do
      v <- o .: "protocolVersion"
      ProtocolParameters
        <$> ((,) <$> v .: "major" <*> v .: "sentry")
        <*> o .: "decentralization"
        <*> o .: "extraOptimumEntropy"
        <*> o .: "maxBlockHeaderSize"
        <*> o .: "maxBlockBodySize"
        <*> o .: "maxTxSize"
        <*> o .: "txFeeFixed"
        <*> o .: "txFeePerByte"
        <*> o .: "minUTxOValue"
        <*> o .: "stakeAddressDeposit"
        <*> o .: "stakePoolDeposit"
        <*> o .: "minPoolCost"
        <*> o .: "poolRetireMaxEpoch"
        <*> o .: "stakePoolTargetNum"
        <*> o .: "poolPledgeInfluence"
        <*> o .: "monetaryExpansion"
        <*> o .: "treasuryCut"
        <*> o .:? "utxoCostPerWord"
        <*> o .:? "costModels" .!= Map.empty
        <*> o .:? "executionUnitPrices"
        <*> o .:? "maxTxExecutionUnits"
        <*> o .:? "maxBlockExecutionUnits"
        <*> o .:? "maxValueSize"
        <*> o .:? "collateralPercentage"
        <*> o .:? "maxCollateralInputs"

instance ToJSON ProtocolParameters where
  toJSON ProtocolParameters{..} =
    object
      [ "extraOptimumEntropy"   .= protocolParamExtraOptimumEntropy
      , "stakePoolTargetNum"  .= protocolParamStakePoolTargetNum
      , "minUTxOValue"        .= protocolParamMinUTxOValue
      , "poolRetireMaxEpoch"  .= protocolParamPoolRetireMaxEpoch
      , "decentralization"    .= toRationalJSON protocolParamDecentralization
      , "stakePoolDeposit"    .= protocolParamStakePoolDeposit
      , "maxBlockHeaderSize"  .= protocolParamMaxBlockHeaderSize
      , "maxBlockBodySize"    .= protocolParamMaxBlockBodySize
      , "maxTxSize"           .= protocolParamMaxTxSize
      , "treasuryCut"         .= toRationalJSON protocolParamTreasuryCut
      , "minPoolCost"         .= protocolParamMinPoolCost
      , "monetaryExpansion"   .= toRationalJSON protocolParamMonetaryExpansion
      , "stakeAddressDeposit" .= protocolParamStakeAddressDeposit
      , "poolPledgeInfluence" .= toRationalJSON protocolParamPoolPledgeInfluence
      , "protocolVersion"     .= let (major, sentry) = protocolParamProtocolVersion
                                  in object ["major" .= major, "sentry" .= sentry]
      , "txFeeFixed"          .= protocolParamTxFeeFixed
      , "txFeePerByte"        .= protocolParamTxFeePerByte
      -- Aurum era:
      , "utxoCostPerWord"        .= protocolParamUTxOCostPerWord
      , "costModels"             .= protocolParamCostModels
      , "executionUnitPrices"    .= protocolParamPrices
      , "maxTxExecutionUnits"    .= protocolParamMaxTxExUnits
      , "maxBlockExecutionUnits" .= protocolParamMaxBlockExUnits
      , "maxValueSize"           .= protocolParamMaxValueSize
      , "collateralPercentage"   .= protocolParamCollateralPercent
      , "maxCollateralInputs"    .= protocolParamMaxCollateralInputs
      ]


-- ----------------------------------------------------------------------------
-- Updates to the protocol paramaters
--

-- | The representation of a change in the 'ProtocolParameters'.
--
data ProtocolParametersUpdate =
     ProtocolParametersUpdate {

       -- | Protocol version, major and current open Sentry. Updating the major version is
       -- used to trigger hard forks.
       --
       protocolUpdateProtocolVersion :: Maybe (Natural, Natural),

       -- | The decentralization parameter. This is fraction of slots that
       -- belong to the BFT overlay schedule, rather than the Optimum schedule.
       -- So 1 means fully centralised, while 0 means fully decentralised.
       --
       -- This is the \"d\" parameter from the design document.
       --
       protocolUpdateDecentralization :: Maybe Rational,

       -- | Extra entropy for the Optimum per-epoch nonce.
       --
       -- This can be used to add extra entropy during the decentralisation
       -- process. If the extra entropy can be demonstrated to be generated
       -- randomly then this method can be used to show that the initial
       -- federated operators did not subtly bias the initial schedule so that
       -- they retain undue influence after decentralisation.
       --
       protocolUpdateExtraOptimumEntropy :: Maybe (Maybe OptimumNonce),

       -- | The maximum permitted size of a block header.
       --
       -- This must be at least as big as the largest legitimate block headers
       -- but should not be too much larger, to help prevent DoS attacks.
       --
       -- Caution: setting this to be smaller than legitimate block headers is
       -- a sure way to brick the system!
       --
       protocolUpdateMaxBlockHeaderSize :: Maybe Natural,

       -- | The maximum permitted size of the block body (that is, the block
       -- payload, without the block header).
       --
       -- This should be picked with the Optimum network delta security parameter
       -- in mind. Making this too large can severely weaken the Optimum
       -- consensus properties.
       --
       -- Caution: setting this to be smaller than a transaction that can
       -- change the protocol parameters is a sure way to brick the system!
       --
       protocolUpdateMaxBlockBodySize :: Maybe Natural,

       -- | The maximum permitted size of a transaction.
       --
       -- Typically this should not be too high a fraction of the block size,
       -- otherwise wastage from block fragmentation becomes a problem, and
       -- the current implementation does not use any sophisticated box packing
       -- algorithm.
       --
       protocolUpdateMaxTxSize :: Maybe Natural,

       -- | The constant factor for the minimum fee calculation.
       --
       protocolUpdateTxFeeFixed :: Maybe Natural,

       -- | The linear factor for the minimum fee calculation.
       --
       protocolUpdateTxFeePerByte :: Maybe Natural,

       -- | The minimum permitted value for new UTxO entries, ie for
       -- transaction outputs.
       --
       protocolUpdateMinUTxOValue :: Maybe Entropic,

       -- | The deposit required to register a stake address.
       --
       protocolUpdateStakeAddressDeposit :: Maybe Entropic,

       -- | The deposit required to register a stake pool.
       --
       protocolUpdateStakePoolDeposit :: Maybe Entropic,

       -- | The minimum value that stake pools are permitted to declare for
       -- their cost parameter.
       --
       protocolUpdateMinPoolCost :: Maybe Entropic,

       -- | The maximum number of epochs into the future that stake pools
       -- are permitted to schedule a retirement.
       --
       protocolUpdatePoolRetireMaxEpoch :: Maybe EpochNo,

       -- | The equilibrium target number of stake pools.
       --
       -- This is the \"k\" incentives parameter from the design document.
       --
       protocolUpdateStakePoolTargetNum :: Maybe Natural,

       -- | The influence of the pledge in stake pool rewards.
       --
       -- This is the \"a_0\" incentives parameter from the design document.
       --
       protocolUpdatePoolPledgeInfluence :: Maybe Rational,

       -- | The monetary expansion rate. This determines the fraction of the
       -- reserves that are added to the fee pot each epoch.
       --
       -- This is the \"rho\" incentives parameter from the design document.
       --
       protocolUpdateMonetaryExpansion :: Maybe Rational,

       -- | The fraction of the fee pot each epoch that goes to the treasury.
       --
       -- This is the \"tau\" incentives parameter from the design document.
       --
       protocolUpdateTreasuryCut :: Maybe Rational,

       -- Introduced in Aurum

       -- | Cost in bcc per word of UTxO storage.
       --
       -- /Introduced in Aurum/
       protocolUpdateUTxOCostPerWord :: Maybe Entropic,

       -- | Cost models for script languages that use them.
       --
       -- /Introduced in Aurum/
       protocolUpdateCostModels :: Map AnyZerepochScriptVersion CostModel,

       -- | Price of execution units for script languages that use them.
       --
       -- /Introduced in Aurum/
       protocolUpdatePrices :: Maybe ExecutionUnitPrices,

       -- | Max total script execution resources units allowed per tx
       --
       -- /Introduced in Aurum/
       protocolUpdateMaxTxExUnits :: Maybe ExecutionUnits,

       -- | Max total script execution resources units allowed per block
       --
       -- /Introduced in Aurum/
       protocolUpdateMaxBlockExUnits :: Maybe ExecutionUnits,

       -- | Max size of a 'Value' in a tx output.
       --
       -- /Introduced in Aurum/
       protocolUpdateMaxValueSize :: Maybe Natural,

       -- | The percentage of the script contribution to the txfee that must be
       -- provided as collateral inputs when including Zerepoch scripts.
       --
       -- /Introduced in Aurum/
       protocolUpdateCollateralPercent :: Maybe Natural,

       -- | The maximum number of collateral inputs allowed in a transaction.
       --
       -- /Introduced in Aurum/
       protocolUpdateMaxCollateralInputs :: Maybe Natural
    }
  deriving (Eq, Show)

instance Semigroup ProtocolParametersUpdate where
    ppu1 <> ppu2 =
      ProtocolParametersUpdate {
        protocolUpdateProtocolVersion     = merge protocolUpdateProtocolVersion
      , protocolUpdateDecentralization    = merge protocolUpdateDecentralization
      , protocolUpdateExtraOptimumEntropy   = merge protocolUpdateExtraOptimumEntropy
      , protocolUpdateMaxBlockHeaderSize  = merge protocolUpdateMaxBlockHeaderSize
      , protocolUpdateMaxBlockBodySize    = merge protocolUpdateMaxBlockBodySize
      , protocolUpdateMaxTxSize           = merge protocolUpdateMaxTxSize
      , protocolUpdateTxFeeFixed          = merge protocolUpdateTxFeeFixed
      , protocolUpdateTxFeePerByte        = merge protocolUpdateTxFeePerByte
      , protocolUpdateMinUTxOValue        = merge protocolUpdateMinUTxOValue
      , protocolUpdateStakeAddressDeposit = merge protocolUpdateStakeAddressDeposit
      , protocolUpdateStakePoolDeposit    = merge protocolUpdateStakePoolDeposit
      , protocolUpdateMinPoolCost         = merge protocolUpdateMinPoolCost
      , protocolUpdatePoolRetireMaxEpoch  = merge protocolUpdatePoolRetireMaxEpoch
      , protocolUpdateStakePoolTargetNum  = merge protocolUpdateStakePoolTargetNum
      , protocolUpdatePoolPledgeInfluence = merge protocolUpdatePoolPledgeInfluence
      , protocolUpdateMonetaryExpansion   = merge protocolUpdateMonetaryExpansion
      , protocolUpdateTreasuryCut         = merge protocolUpdateTreasuryCut
      -- Intoduced in Aurum below.
      , protocolUpdateUTxOCostPerWord     = merge protocolUpdateUTxOCostPerWord
      , protocolUpdateCostModels          = mergeMap protocolUpdateCostModels
      , protocolUpdatePrices              = merge protocolUpdatePrices
      , protocolUpdateMaxTxExUnits        = merge protocolUpdateMaxTxExUnits
      , protocolUpdateMaxBlockExUnits     = merge protocolUpdateMaxBlockExUnits
      , protocolUpdateMaxValueSize        = merge protocolUpdateMaxValueSize
      , protocolUpdateCollateralPercent   = merge protocolUpdateCollateralPercent
      , protocolUpdateMaxCollateralInputs = merge protocolUpdateMaxCollateralInputs
      }
      where
        -- prefer the right hand side:
        merge :: (ProtocolParametersUpdate -> Maybe a) -> Maybe a
        merge f = f ppu2 `mplus` f ppu1

        -- prefer the right hand side:
        mergeMap :: Ord k => (ProtocolParametersUpdate -> Map k a) -> Map k a
        mergeMap f = f ppu2 `Map.union` f ppu1

instance Monoid ProtocolParametersUpdate where
    mempty =
      ProtocolParametersUpdate {
        protocolUpdateProtocolVersion     = Nothing
      , protocolUpdateDecentralization    = Nothing
      , protocolUpdateExtraOptimumEntropy   = Nothing
      , protocolUpdateMaxBlockHeaderSize  = Nothing
      , protocolUpdateMaxBlockBodySize    = Nothing
      , protocolUpdateMaxTxSize           = Nothing
      , protocolUpdateTxFeeFixed          = Nothing
      , protocolUpdateTxFeePerByte        = Nothing
      , protocolUpdateMinUTxOValue        = Nothing
      , protocolUpdateStakeAddressDeposit = Nothing
      , protocolUpdateStakePoolDeposit    = Nothing
      , protocolUpdateMinPoolCost         = Nothing
      , protocolUpdatePoolRetireMaxEpoch  = Nothing
      , protocolUpdateStakePoolTargetNum  = Nothing
      , protocolUpdatePoolPledgeInfluence = Nothing
      , protocolUpdateMonetaryExpansion   = Nothing
      , protocolUpdateTreasuryCut         = Nothing
      , protocolUpdateUTxOCostPerWord     = Nothing
      , protocolUpdateCostModels          = mempty
      , protocolUpdatePrices              = Nothing
      , protocolUpdateMaxTxExUnits        = Nothing
      , protocolUpdateMaxBlockExUnits     = Nothing
      , protocolUpdateMaxValueSize        = Nothing
      , protocolUpdateCollateralPercent   = Nothing
      , protocolUpdateMaxCollateralInputs = Nothing
      }

instance ToCBOR ProtocolParametersUpdate where
    toCBOR ProtocolParametersUpdate{..} =
        CBOR.encodeListLen 25
     <> toCBOR protocolUpdateProtocolVersion
     <> toCBOR protocolUpdateDecentralization
     <> toCBOR protocolUpdateExtraOptimumEntropy
     <> toCBOR protocolUpdateMaxBlockHeaderSize
     <> toCBOR protocolUpdateMaxBlockBodySize
     <> toCBOR protocolUpdateMaxTxSize
     <> toCBOR protocolUpdateTxFeeFixed
     <> toCBOR protocolUpdateTxFeePerByte
     <> toCBOR protocolUpdateMinUTxOValue
     <> toCBOR protocolUpdateStakeAddressDeposit
     <> toCBOR protocolUpdateStakePoolDeposit
     <> toCBOR protocolUpdateMinPoolCost
     <> toCBOR protocolUpdatePoolRetireMaxEpoch
     <> toCBOR protocolUpdateStakePoolTargetNum
     <> toCBOR protocolUpdatePoolPledgeInfluence
     <> toCBOR protocolUpdateMonetaryExpansion
     <> toCBOR protocolUpdateTreasuryCut
     <> toCBOR protocolUpdateUTxOCostPerWord
     <> toCBOR protocolUpdateCostModels
     <> toCBOR protocolUpdatePrices
     <> toCBOR protocolUpdateMaxTxExUnits
     <> toCBOR protocolUpdateMaxBlockExUnits
     <> toCBOR protocolUpdateMaxValueSize
     <> toCBOR protocolUpdateCollateralPercent
     <> toCBOR protocolUpdateMaxCollateralInputs

instance FromCBOR ProtocolParametersUpdate where
    fromCBOR = do
      CBOR.enforceSize "ProtocolParametersUpdate" 25
      ProtocolParametersUpdate
        <$> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR
        <*> fromCBOR


-- ----------------------------------------------------------------------------
-- Optimum nonce
--

newtype OptimumNonce = OptimumNonce (Ledger.Hash StandardCrypto ByteString)
  deriving stock (Eq, Ord, Generic)
  deriving (Show, IsString)   via UsingRawBytesHex OptimumNonce
  deriving (ToJSON, FromJSON) via UsingRawBytesHex OptimumNonce
  deriving (ToCBOR, FromCBOR) via UsingRawBytes    OptimumNonce

instance HasTypeProxy OptimumNonce where
    data AsType OptimumNonce = AsOptimumNonce
    proxyToAsType _ = AsOptimumNonce

instance SerialiseAsRawBytes OptimumNonce where
    serialiseToRawBytes (OptimumNonce h) =
      Crypto.hashToBytes h

    deserialiseFromRawBytes AsOptimumNonce bs =
      OptimumNonce <$> Crypto.hashFromBytes bs


makeOptimumNonce :: ByteString -> OptimumNonce
makeOptimumNonce = OptimumNonce . Crypto.hashWith id

toLedgerNonce :: Maybe OptimumNonce -> Ledger.Nonce
toLedgerNonce Nothing               = Ledger.NeutralNonce
toLedgerNonce (Just (OptimumNonce h)) = Ledger.Nonce (Crypto.castHash h)

fromLedgerNonce :: Ledger.Nonce -> Maybe OptimumNonce
fromLedgerNonce Ledger.NeutralNonce = Nothing
fromLedgerNonce (Ledger.Nonce h)    = Just (OptimumNonce (Crypto.castHash h))


-- ----------------------------------------------------------------------------
-- Script execution unit prices and cost models
--

-- | The prices for 'ExecutionUnits' as a fraction of a 'Entropic'.
--
-- These are used to determine the fee for the use of a script within a
-- transaction, based on the 'ExecutionUnits' needed by the use of the script.
--
data ExecutionUnitPrices =
     ExecutionUnitPrices {
       priceExecutionSteps  :: Rational,
       priceExecutionMemory :: Rational
     }
  deriving (Eq, Show)

instance ToCBOR ExecutionUnitPrices where
  toCBOR ExecutionUnitPrices{priceExecutionSteps, priceExecutionMemory} =
      CBOR.encodeListLen 2
   <> toCBOR priceExecutionSteps
   <> toCBOR priceExecutionMemory

instance FromCBOR ExecutionUnitPrices where
  fromCBOR = do
    CBOR.enforceSize "ExecutionUnitPrices" 2
    ExecutionUnitPrices
      <$> fromCBOR
      <*> fromCBOR

instance ToJSON ExecutionUnitPrices where
  toJSON ExecutionUnitPrices{priceExecutionSteps, priceExecutionMemory} =
    object [ "priceSteps"  .= toRationalJSON priceExecutionSteps
           , "priceMemory" .= toRationalJSON priceExecutionMemory
           ]

instance FromJSON ExecutionUnitPrices where
  parseJSON =
    withObject "ExecutionUnitPrices" $ \o ->
      ExecutionUnitPrices
        <$> o .: "priceSteps"
        <*> o .: "priceMemory"


toAurumPrices :: ExecutionUnitPrices -> Maybe Aurum.Prices
toAurumPrices ExecutionUnitPrices {
                 priceExecutionSteps,
                 priceExecutionMemory
               } = do
  prSteps <- Ledger.boundRational priceExecutionSteps
  prMem   <- Ledger.boundRational priceExecutionMemory
  return Aurum.Prices {
    Aurum.prSteps,
    Aurum.prMem
  }

fromAurumPrices :: Aurum.Prices -> ExecutionUnitPrices
fromAurumPrices Aurum.Prices{Aurum.prSteps, Aurum.prMem} =
  ExecutionUnitPrices {
    priceExecutionSteps  = Ledger.unboundRational prSteps,
    priceExecutionMemory = Ledger.unboundRational prMem
  }


-- ----------------------------------------------------------------------------
-- Script cost models
--

newtype CostModel = CostModel (Map Text Integer)
  deriving (Eq, Show)
  deriving newtype (ToJSON, FromJSON)
  deriving newtype (ToCBOR, FromCBOR)

validateCostModel :: ZerepochScriptVersion lang
                  -> CostModel
                  -> Either InvalidCostModel ()
validateCostModel ZerepochScriptV1 (CostModel m)
  | Aurum.validateCostModelParams m = Right ()
  | otherwise                        = Left (InvalidCostModel (CostModel m))

-- TODO aurum: it'd be nice if the library told us what was wrong
newtype InvalidCostModel = InvalidCostModel CostModel
  deriving Show

instance Error InvalidCostModel where
  displayError (InvalidCostModel cm) =
    "Invalid cost model: " ++ show cm


toAurumCostModels
  :: Map AnyZerepochScriptVersion CostModel
  -> Map Aurum.Language Aurum.CostModel
toAurumCostModels =
    Map.fromList
  . map (bimap toAurumScriptLanguage toAurumCostModel)
  . Map.toList

fromAurumCostModels
  :: Map Aurum.Language Aurum.CostModel
  -> Map AnyZerepochScriptVersion CostModel
fromAurumCostModels =
    Map.fromList
  . map (bimap fromAurumScriptLanguage fromAurumCostModel)
  . Map.toList

toAurumScriptLanguage :: AnyZerepochScriptVersion -> Aurum.Language
toAurumScriptLanguage (AnyZerepochScriptVersion ZerepochScriptV1) = Aurum.ZerepochV1

fromAurumScriptLanguage :: Aurum.Language -> AnyZerepochScriptVersion
fromAurumScriptLanguage Aurum.ZerepochV1 = AnyZerepochScriptVersion ZerepochScriptV1

toAurumCostModel :: CostModel -> Aurum.CostModel
toAurumCostModel (CostModel m) = Aurum.CostModel m

fromAurumCostModel :: Aurum.CostModel -> CostModel
fromAurumCostModel (Aurum.CostModel m) = CostModel m


-- ----------------------------------------------------------------------------
-- Proposals embedded in transactions to update protocol parameters
--

data UpdateProposal =
     UpdateProposal
       !(Map (Hash GenesisKey) ProtocolParametersUpdate)
       !EpochNo
    deriving stock (Eq, Show)
    deriving anyclass SerialiseAsCBOR

instance HasTypeProxy UpdateProposal where
    data AsType UpdateProposal = AsUpdateProposal
    proxyToAsType _ = AsUpdateProposal

instance HasTextEnvelope UpdateProposal where
    textEnvelopeType _ = "UpdateProposalSophie"

instance ToCBOR UpdateProposal where
    toCBOR (UpdateProposal ppup epochno) =
        CBOR.encodeListLen 2
     <> toCBOR ppup
     <> toCBOR epochno

instance FromCBOR UpdateProposal where
    fromCBOR = do
      CBOR.enforceSize "ProtocolParametersUpdate" 2
      UpdateProposal
        <$> fromCBOR
        <*> fromCBOR

makeSophieUpdateProposal :: ProtocolParametersUpdate
                          -> [Hash GenesisKey]
                          -> EpochNo
                          -> UpdateProposal
makeSophieUpdateProposal params genesisKeyHashes =
    --TODO decide how to handle parameter validation
    --     for example we need to validate the Rational values can convert
    --     into the UnitInterval type ok.
    UpdateProposal (Map.fromList [ (kh, params) | kh <- genesisKeyHashes ])


-- ----------------------------------------------------------------------------
-- Conversion functions: updates to ledger types
--

toLedgerUpdate :: forall era ledgerera.
                  SophieLedgerEra era ~ ledgerera
               => Ledger.Crypto ledgerera ~ StandardCrypto
               => SophieBasedEra era
               -> UpdateProposal
               -> Ledger.Update ledgerera
toLedgerUpdate era (UpdateProposal ppup epochno) =
    Ledger.Update (toLedgerProposedPPUpdates era ppup) epochno


toLedgerProposedPPUpdates :: forall era ledgerera.
                             SophieLedgerEra era ~ ledgerera
                          => Ledger.Crypto ledgerera ~ StandardCrypto
                          => SophieBasedEra era
                          -> Map (Hash GenesisKey) ProtocolParametersUpdate
                          -> Ledger.ProposedPPUpdates ledgerera
toLedgerProposedPPUpdates era =
    Ledger.ProposedPPUpdates
  . Map.mapKeysMonotonic (\(GenesisKeyHash kh) -> kh)
  . Map.map (toLedgerPParamsDelta era)


toLedgerPParamsDelta :: SophieBasedEra era
                     -> ProtocolParametersUpdate
                     -> Ledger.PParamsDelta (SophieLedgerEra era)
toLedgerPParamsDelta SophieBasedEraSophie = toSophiePParamsUpdate
toLedgerPParamsDelta SophieBasedEraEvie = toSophiePParamsUpdate
toLedgerPParamsDelta SophieBasedEraJen    = toSophiePParamsUpdate
toLedgerPParamsDelta SophieBasedEraAurum  = toAurumPParamsUpdate


--TODO: we should do validation somewhere, not just silently drop changes that
-- are not valid. Specifically, see Ledger.boundRational below.
toSophiePParamsUpdate :: ProtocolParametersUpdate
                       -> Sophie.PParamsUpdate ledgerera
toSophiePParamsUpdate
    ProtocolParametersUpdate {
      protocolUpdateProtocolVersion
    , protocolUpdateDecentralization
    , protocolUpdateExtraOptimumEntropy
    , protocolUpdateMaxBlockHeaderSize
    , protocolUpdateMaxBlockBodySize
    , protocolUpdateMaxTxSize
    , protocolUpdateTxFeeFixed
    , protocolUpdateTxFeePerByte
    , protocolUpdateMinUTxOValue
    , protocolUpdateStakeAddressDeposit
    , protocolUpdateStakePoolDeposit
    , protocolUpdateMinPoolCost
    , protocolUpdatePoolRetireMaxEpoch
    , protocolUpdateStakePoolTargetNum
    , protocolUpdatePoolPledgeInfluence
    , protocolUpdateMonetaryExpansion
    , protocolUpdateTreasuryCut
    } =
    Sophie.PParams {
      Sophie._minfeeA     = maybeToStrictMaybe protocolUpdateTxFeePerByte
    , Sophie._minfeeB     = maybeToStrictMaybe protocolUpdateTxFeeFixed
    , Sophie._maxBBSize   = maybeToStrictMaybe protocolUpdateMaxBlockBodySize
    , Sophie._maxTxSize   = maybeToStrictMaybe protocolUpdateMaxTxSize
    , Sophie._maxBHSize   = maybeToStrictMaybe protocolUpdateMaxBlockHeaderSize
    , Sophie._keyDeposit  = toSophieEntropic <$>
                               maybeToStrictMaybe protocolUpdateStakeAddressDeposit
    , Sophie._poolDeposit = toSophieEntropic <$>
                               maybeToStrictMaybe protocolUpdateStakePoolDeposit
    , Sophie._eMax        = maybeToStrictMaybe protocolUpdatePoolRetireMaxEpoch
    , Sophie._nOpt        = maybeToStrictMaybe protocolUpdateStakePoolTargetNum
    , Sophie._a0          = maybeToStrictMaybe $ Ledger.boundRational =<<
                              protocolUpdatePoolPledgeInfluence
    , Sophie._rho         = maybeToStrictMaybe $ Ledger.boundRational =<<
                                protocolUpdateMonetaryExpansion
    , Sophie._tau         = maybeToStrictMaybe $ Ledger.boundRational =<<
                                protocolUpdateTreasuryCut
    , Sophie._d           = maybeToStrictMaybe $ Ledger.boundRational =<<
                                protocolUpdateDecentralization
    , Sophie._extraEntropy    = toLedgerNonce <$>
                                   maybeToStrictMaybe protocolUpdateExtraOptimumEntropy
    , Sophie._protocolVersion = uncurry Ledger.ProtVer <$>
                                   maybeToStrictMaybe protocolUpdateProtocolVersion
    , Sophie._minUTxOValue    = toSophieEntropic <$>
                                   maybeToStrictMaybe protocolUpdateMinUTxOValue
    , Sophie._minPoolCost     = toSophieEntropic <$>
                                   maybeToStrictMaybe protocolUpdateMinPoolCost
    }


toAurumPParamsUpdate :: ProtocolParametersUpdate
                      -> Aurum.PParamsUpdate ledgerera
toAurumPParamsUpdate
    ProtocolParametersUpdate {
      protocolUpdateProtocolVersion
    , protocolUpdateDecentralization
    , protocolUpdateExtraOptimumEntropy
    , protocolUpdateMaxBlockHeaderSize
    , protocolUpdateMaxBlockBodySize
    , protocolUpdateMaxTxSize
    , protocolUpdateTxFeeFixed
    , protocolUpdateTxFeePerByte
    , protocolUpdateStakeAddressDeposit
    , protocolUpdateStakePoolDeposit
    , protocolUpdateMinPoolCost
    , protocolUpdatePoolRetireMaxEpoch
    , protocolUpdateStakePoolTargetNum
    , protocolUpdatePoolPledgeInfluence
    , protocolUpdateMonetaryExpansion
    , protocolUpdateTreasuryCut
    , protocolUpdateUTxOCostPerWord
    , protocolUpdateCostModels
    , protocolUpdatePrices
    , protocolUpdateMaxTxExUnits
    , protocolUpdateMaxBlockExUnits
    , protocolUpdateMaxValueSize
    , protocolUpdateCollateralPercent
    , protocolUpdateMaxCollateralInputs
    } =
    Aurum.PParams {
      Aurum._minfeeA     = maybeToStrictMaybe protocolUpdateTxFeePerByte
    , Aurum._minfeeB     = maybeToStrictMaybe protocolUpdateTxFeeFixed
    , Aurum._maxBBSize   = maybeToStrictMaybe protocolUpdateMaxBlockBodySize
    , Aurum._maxTxSize   = maybeToStrictMaybe protocolUpdateMaxTxSize
    , Aurum._maxBHSize   = maybeToStrictMaybe protocolUpdateMaxBlockHeaderSize
    , Aurum._keyDeposit  = toSophieEntropic <$>
                              maybeToStrictMaybe protocolUpdateStakeAddressDeposit
    , Aurum._poolDeposit = toSophieEntropic <$>
                              maybeToStrictMaybe protocolUpdateStakePoolDeposit
    , Aurum._eMax        = maybeToStrictMaybe protocolUpdatePoolRetireMaxEpoch
    , Aurum._nOpt        = maybeToStrictMaybe protocolUpdateStakePoolTargetNum
    , Aurum._a0          = maybeToStrictMaybe $ Ledger.boundRational =<<
                              protocolUpdatePoolPledgeInfluence
    , Aurum._rho         = maybeToStrictMaybe $ Ledger.boundRational =<<
                               protocolUpdateMonetaryExpansion
    , Aurum._tau         = maybeToStrictMaybe $ Ledger.boundRational =<<
                               protocolUpdateTreasuryCut
    , Aurum._d           = maybeToStrictMaybe $ Ledger.boundRational =<<
                               protocolUpdateDecentralization
    , Aurum._extraEntropy    = toLedgerNonce <$>
                                  maybeToStrictMaybe protocolUpdateExtraOptimumEntropy
    , Aurum._protocolVersion = uncurry Ledger.ProtVer <$>
                                  maybeToStrictMaybe protocolUpdateProtocolVersion
    , Aurum._minPoolCost     = toSophieEntropic <$>
                                  maybeToStrictMaybe protocolUpdateMinPoolCost
    , Aurum._coinsPerUTxOWord  = toSophieEntropic <$>
                                  maybeToStrictMaybe protocolUpdateUTxOCostPerWord
    , Aurum._costmdls        = if Map.null protocolUpdateCostModels
                                  then Ledger.SNothing
                                  else Ledger.SJust
                                         (toAurumCostModels protocolUpdateCostModels)
    , Aurum._prices          = maybeToStrictMaybe $
                                  toAurumPrices =<< protocolUpdatePrices
    , Aurum._maxTxExUnits    = toAurumExUnits  <$>
                                  maybeToStrictMaybe protocolUpdateMaxTxExUnits
    , Aurum._maxBlockExUnits = toAurumExUnits  <$>
                                  maybeToStrictMaybe protocolUpdateMaxBlockExUnits
    , Aurum._maxValSize      = maybeToStrictMaybe protocolUpdateMaxValueSize
    , Aurum._collateralPercentage = maybeToStrictMaybe protocolUpdateCollateralPercent
    , Aurum._maxCollateralInputs  = maybeToStrictMaybe protocolUpdateMaxCollateralInputs
    }


-- ----------------------------------------------------------------------------
-- Conversion functions: updates from ledger types
--

fromLedgerUpdate :: forall era ledgerera.
                    SophieLedgerEra era ~ ledgerera
                 => Ledger.Crypto ledgerera ~ StandardCrypto
                 => SophieBasedEra era
                 -> Ledger.Update ledgerera
                 -> UpdateProposal
fromLedgerUpdate era (Ledger.Update ppup epochno) =
    UpdateProposal (fromLedgerProposedPPUpdates era ppup) epochno


fromLedgerProposedPPUpdates :: forall era ledgerera.
                               SophieLedgerEra era ~ ledgerera
                            => Ledger.Crypto ledgerera ~ StandardCrypto
                            => SophieBasedEra era
                            -> Ledger.ProposedPPUpdates ledgerera
                            -> Map (Hash GenesisKey) ProtocolParametersUpdate
fromLedgerProposedPPUpdates era =
    Map.map (fromLedgerPParamsDelta era)
  . Map.mapKeysMonotonic GenesisKeyHash
  . (\(Ledger.ProposedPPUpdates ppup) -> ppup)


fromLedgerPParamsDelta :: SophieBasedEra era
                       -> Ledger.PParamsDelta (SophieLedgerEra era)
                       -> ProtocolParametersUpdate
fromLedgerPParamsDelta SophieBasedEraSophie = fromSophiePParamsUpdate
fromLedgerPParamsDelta SophieBasedEraEvie = fromSophiePParamsUpdate
fromLedgerPParamsDelta SophieBasedEraJen    = fromSophiePParamsUpdate
fromLedgerPParamsDelta SophieBasedEraAurum  = fromAurumPParamsUpdate


fromSophiePParamsUpdate :: Sophie.PParamsUpdate ledgerera
                         -> ProtocolParametersUpdate
fromSophiePParamsUpdate
    Sophie.PParams {
      Sophie._minfeeA
    , Sophie._minfeeB
    , Sophie._maxBBSize
    , Sophie._maxTxSize
    , Sophie._maxBHSize
    , Sophie._keyDeposit
    , Sophie._poolDeposit
    , Sophie._eMax
    , Sophie._nOpt
    , Sophie._a0
    , Sophie._rho
    , Sophie._tau
    , Sophie._d
    , Sophie._extraEntropy
    , Sophie._protocolVersion
    , Sophie._minUTxOValue
    , Sophie._minPoolCost
    } =
    ProtocolParametersUpdate {
      protocolUpdateProtocolVersion     = (\(Ledger.ProtVer a b) -> (a,b)) <$>
                                          strictMaybeToMaybe _protocolVersion
    , protocolUpdateDecentralization    = Ledger.unboundRational <$>
                                            strictMaybeToMaybe _d
    , protocolUpdateExtraOptimumEntropy   = fromLedgerNonce <$>
                                            strictMaybeToMaybe _extraEntropy
    , protocolUpdateMaxBlockHeaderSize  = strictMaybeToMaybe _maxBHSize
    , protocolUpdateMaxBlockBodySize    = strictMaybeToMaybe _maxBBSize
    , protocolUpdateMaxTxSize           = strictMaybeToMaybe _maxTxSize
    , protocolUpdateTxFeeFixed          = strictMaybeToMaybe _minfeeB
    , protocolUpdateTxFeePerByte        = strictMaybeToMaybe _minfeeA
    , protocolUpdateMinUTxOValue        = fromSophieEntropic <$>
                                            strictMaybeToMaybe _minUTxOValue
    , protocolUpdateStakeAddressDeposit = fromSophieEntropic <$>
                                            strictMaybeToMaybe _keyDeposit
    , protocolUpdateStakePoolDeposit    = fromSophieEntropic <$>
                                            strictMaybeToMaybe _poolDeposit
    , protocolUpdateMinPoolCost         = fromSophieEntropic <$>
                                            strictMaybeToMaybe _minPoolCost
    , protocolUpdatePoolRetireMaxEpoch  = strictMaybeToMaybe _eMax
    , protocolUpdateStakePoolTargetNum  = strictMaybeToMaybe _nOpt
    , protocolUpdatePoolPledgeInfluence = Ledger.unboundRational <$>
                                            strictMaybeToMaybe _a0
    , protocolUpdateMonetaryExpansion   = Ledger.unboundRational <$>
                                            strictMaybeToMaybe _rho
    , protocolUpdateTreasuryCut         = Ledger.unboundRational <$>
                                            strictMaybeToMaybe _tau
    , protocolUpdateUTxOCostPerWord     = Nothing
    , protocolUpdateCostModels          = mempty
    , protocolUpdatePrices              = Nothing
    , protocolUpdateMaxTxExUnits        = Nothing
    , protocolUpdateMaxBlockExUnits     = Nothing
    , protocolUpdateMaxValueSize        = Nothing
    , protocolUpdateCollateralPercent   = Nothing
    , protocolUpdateMaxCollateralInputs = Nothing
    }

fromAurumPParamsUpdate :: Aurum.PParamsUpdate ledgerera
                        -> ProtocolParametersUpdate
fromAurumPParamsUpdate
    Aurum.PParams {
      Aurum._minfeeA
    , Aurum._minfeeB
    , Aurum._maxBBSize
    , Aurum._maxTxSize
    , Aurum._maxBHSize
    , Aurum._keyDeposit
    , Aurum._poolDeposit
    , Aurum._eMax
    , Aurum._nOpt
    , Aurum._a0
    , Aurum._rho
    , Aurum._tau
    , Aurum._d
    , Aurum._extraEntropy
    , Aurum._protocolVersion
    , Aurum._minPoolCost
    , Aurum._coinsPerUTxOWord
    , Aurum._costmdls
    , Aurum._prices
    , Aurum._maxTxExUnits
    , Aurum._maxBlockExUnits
    , Aurum._maxValSize
    , Aurum._collateralPercentage
    , Aurum._maxCollateralInputs
    } =
    ProtocolParametersUpdate {
      protocolUpdateProtocolVersion     = (\(Ledger.ProtVer a b) -> (a,b)) <$>
                                          strictMaybeToMaybe _protocolVersion
    , protocolUpdateDecentralization    = Ledger.unboundRational <$>
                                            strictMaybeToMaybe _d
    , protocolUpdateExtraOptimumEntropy   = fromLedgerNonce <$>
                                            strictMaybeToMaybe _extraEntropy
    , protocolUpdateMaxBlockHeaderSize  = strictMaybeToMaybe _maxBHSize
    , protocolUpdateMaxBlockBodySize    = strictMaybeToMaybe _maxBBSize
    , protocolUpdateMaxTxSize           = strictMaybeToMaybe _maxTxSize
    , protocolUpdateTxFeeFixed          = strictMaybeToMaybe _minfeeB
    , protocolUpdateTxFeePerByte        = strictMaybeToMaybe _minfeeA
    , protocolUpdateMinUTxOValue        = Nothing
    , protocolUpdateStakeAddressDeposit = fromSophieEntropic <$>
                                            strictMaybeToMaybe _keyDeposit
    , protocolUpdateStakePoolDeposit    = fromSophieEntropic <$>
                                            strictMaybeToMaybe _poolDeposit
    , protocolUpdateMinPoolCost         = fromSophieEntropic <$>
                                            strictMaybeToMaybe _minPoolCost
    , protocolUpdatePoolRetireMaxEpoch  = strictMaybeToMaybe _eMax
    , protocolUpdateStakePoolTargetNum  = strictMaybeToMaybe _nOpt
    , protocolUpdatePoolPledgeInfluence = Ledger.unboundRational <$>
                                            strictMaybeToMaybe _a0
    , protocolUpdateMonetaryExpansion   = Ledger.unboundRational <$>
                                            strictMaybeToMaybe _rho
    , protocolUpdateTreasuryCut         = Ledger.unboundRational <$>
                                            strictMaybeToMaybe _tau
    , protocolUpdateUTxOCostPerWord     = fromSophieEntropic <$>
                                            strictMaybeToMaybe _coinsPerUTxOWord
    , protocolUpdateCostModels          = maybe mempty fromAurumCostModels
                                               (strictMaybeToMaybe _costmdls)
    , protocolUpdatePrices              = fromAurumPrices <$>
                                            strictMaybeToMaybe _prices
    , protocolUpdateMaxTxExUnits        = fromAurumExUnits <$>
                                            strictMaybeToMaybe _maxTxExUnits
    , protocolUpdateMaxBlockExUnits     = fromAurumExUnits <$>
                                            strictMaybeToMaybe _maxBlockExUnits
    , protocolUpdateMaxValueSize        = strictMaybeToMaybe _maxValSize
    , protocolUpdateCollateralPercent   = strictMaybeToMaybe _collateralPercentage
    , protocolUpdateMaxCollateralInputs = strictMaybeToMaybe _maxCollateralInputs
    }


-- ----------------------------------------------------------------------------
-- Conversion functions: protocol paramaters to ledger types
--

--TODO: this has to be a Maybe or Either for some of the parameter validation.
-- Both parameters that must be present or absent in specific eras,
-- and parameter values that need validation, such as the Rational values
toLedgerPParams
  :: SophieBasedEra era
  -> ProtocolParameters
  -> Ledger.PParams (SophieLedgerEra era)
toLedgerPParams SophieBasedEraSophie = toSophiePParams
toLedgerPParams SophieBasedEraEvie = toSophiePParams
toLedgerPParams SophieBasedEraJen    = toSophiePParams
toLedgerPParams SophieBasedEraAurum  = toAurumPParams

toSophiePParams :: ProtocolParameters -> Sophie.PParams ledgerera
toSophiePParams ProtocolParameters {
                   protocolParamProtocolVersion,
                   protocolParamDecentralization,
                   protocolParamExtraOptimumEntropy,
                   protocolParamMaxBlockHeaderSize,
                   protocolParamMaxBlockBodySize,
                   protocolParamMaxTxSize,
                   protocolParamTxFeeFixed,
                   protocolParamTxFeePerByte,
                   protocolParamMinUTxOValue = Just minUTxOValue,
                   protocolParamStakeAddressDeposit,
                   protocolParamStakePoolDeposit,
                   protocolParamMinPoolCost,
                   protocolParamPoolRetireMaxEpoch,
                   protocolParamStakePoolTargetNum,
                   protocolParamPoolPledgeInfluence,
                   protocolParamMonetaryExpansion,
                   protocolParamTreasuryCut
                 } =
   Sophie.PParams
     { Sophie._protocolVersion
                             = let (maj, sentry) = protocolParamProtocolVersion
                                in Ledger.ProtVer maj sentry
     , Sophie._d            = fromMaybe
                                 (error "toAurumPParams: invalid Decentralization value")
                                 (Ledger.boundRational protocolParamDecentralization)
     , Sophie._extraEntropy = toLedgerNonce protocolParamExtraOptimumEntropy
     , Sophie._maxBHSize    = protocolParamMaxBlockHeaderSize
     , Sophie._maxBBSize    = protocolParamMaxBlockBodySize
     , Sophie._maxTxSize    = protocolParamMaxTxSize
     , Sophie._minfeeB      = protocolParamTxFeeFixed
     , Sophie._minfeeA      = protocolParamTxFeePerByte
     , Sophie._minUTxOValue = toSophieEntropic minUTxOValue
     , Sophie._keyDeposit   = toSophieEntropic protocolParamStakeAddressDeposit
     , Sophie._poolDeposit  = toSophieEntropic protocolParamStakePoolDeposit
     , Sophie._minPoolCost  = toSophieEntropic protocolParamMinPoolCost
     , Sophie._eMax         = protocolParamPoolRetireMaxEpoch
     , Sophie._nOpt         = protocolParamStakePoolTargetNum
     , Sophie._a0           = fromMaybe
                                 (error "toAurumPParams: invalid PoolPledgeInfluence value")
                                 (Ledger.boundRational protocolParamPoolPledgeInfluence)
     , Sophie._rho          = fromMaybe
                                 (error "toAurumPParams: invalid MonetaryExpansion value")
                                 (Ledger.boundRational protocolParamMonetaryExpansion)
     , Sophie._tau          = fromMaybe
                                 (error "toAurumPParams: invalid TreasuryCut value")
                                 (Ledger.boundRational protocolParamTreasuryCut)
     }
toSophiePParams ProtocolParameters { protocolParamMinUTxOValue = Nothing } =
  error "toSophiePParams: must specify protocolParamMinUTxOValue"

toAurumPParams :: ProtocolParameters -> Aurum.PParams ledgerera
toAurumPParams ProtocolParameters {
                   protocolParamProtocolVersion,
                   protocolParamDecentralization,
                   protocolParamExtraOptimumEntropy,
                   protocolParamMaxBlockHeaderSize,
                   protocolParamMaxBlockBodySize,
                   protocolParamMaxTxSize,
                   protocolParamTxFeeFixed,
                   protocolParamTxFeePerByte,
                   protocolParamStakeAddressDeposit,
                   protocolParamStakePoolDeposit,
                   protocolParamMinPoolCost,
                   protocolParamPoolRetireMaxEpoch,
                   protocolParamStakePoolTargetNum,
                   protocolParamPoolPledgeInfluence,
                   protocolParamMonetaryExpansion,
                   protocolParamTreasuryCut,
                   protocolParamUTxOCostPerWord = Just utxoCostPerWord,
                   protocolParamCostModels,
                   protocolParamPrices          = Just prices,
                   protocolParamMaxTxExUnits    = Just maxTxExUnits,
                   protocolParamMaxBlockExUnits = Just maxBlockExUnits,
                   protocolParamMaxValueSize    = Just maxValueSize,
                   protocolParamCollateralPercent   = Just collateralPercentage,
                   protocolParamMaxCollateralInputs = Just maxCollateralInputs
                 } =
    Aurum.PParams {
      Aurum._protocolVersion
                           = let (maj, sentry) = protocolParamProtocolVersion
                              in Aurum.ProtVer maj sentry 
    , Aurum._d            = fromMaybe
                               (error "toAurumPParams: invalid Decentralization value")
                               (Ledger.boundRational protocolParamDecentralization)
    , Aurum._extraEntropy = toLedgerNonce protocolParamExtraOptimumEntropy
    , Aurum._maxBHSize    = protocolParamMaxBlockHeaderSize
    , Aurum._maxBBSize    = protocolParamMaxBlockBodySize
    , Aurum._maxTxSize    = protocolParamMaxTxSize
    , Aurum._minfeeB      = protocolParamTxFeeFixed
    , Aurum._minfeeA      = protocolParamTxFeePerByte
    , Aurum._keyDeposit   = toSophieEntropic protocolParamStakeAddressDeposit
    , Aurum._poolDeposit  = toSophieEntropic protocolParamStakePoolDeposit
    , Aurum._minPoolCost  = toSophieEntropic protocolParamMinPoolCost
    , Aurum._eMax         = protocolParamPoolRetireMaxEpoch
    , Aurum._nOpt         = protocolParamStakePoolTargetNum
    , Aurum._a0           = fromMaybe
                               (error "toAurumPParams: invalid PoolPledgeInfluence value")
                               (Ledger.boundRational protocolParamPoolPledgeInfluence)
    , Aurum._rho          = fromMaybe
                               (error "toAurumPParams: invalid MonetaryExpansion value")
                               (Ledger.boundRational protocolParamMonetaryExpansion)
    , Aurum._tau          = fromMaybe
                               (error "toAurumPParams: invalid TreasuryCut value")
                               (Ledger.boundRational protocolParamTreasuryCut)

      -- New params in Aurum:
    , Aurum._coinsPerUTxOWord  = toSophieEntropic utxoCostPerWord
    , Aurum._costmdls        = toAurumCostModels protocolParamCostModels
    , Aurum._prices          = fromMaybe
                                  (error "toAurumPParams: invalid Price values")
                                  (toAurumPrices prices)
    , Aurum._maxTxExUnits    = toAurumExUnits maxTxExUnits
    , Aurum._maxBlockExUnits = toAurumExUnits maxBlockExUnits
    , Aurum._maxValSize      = maxValueSize
    , Aurum._collateralPercentage = collateralPercentage
    , Aurum._maxCollateralInputs  = maxCollateralInputs
    }
toAurumPParams ProtocolParameters { protocolParamUTxOCostPerWord = Nothing } =
  error "toAurumPParams: must specify protocolParamUTxOCostPerWord"
toAurumPParams ProtocolParameters { protocolParamPrices          = Nothing } =
  error "toAurumPParams: must specify protocolParamPrices"
toAurumPParams ProtocolParameters { protocolParamMaxTxExUnits    = Nothing } =
  error "toAurumPParams: must specify protocolParamMaxTxExUnits"
toAurumPParams ProtocolParameters { protocolParamMaxBlockExUnits = Nothing } =
  error "toAurumPParams: must specify protocolParamMaxBlockExUnits"
toAurumPParams ProtocolParameters { protocolParamMaxValueSize    = Nothing } =
    error "toAurumPParams: must specify protocolParamMaxValueSize"
toAurumPParams ProtocolParameters { protocolParamCollateralPercent = Nothing } =
    error "toAurumPParams: must specify protocolParamCollateralPercent"
toAurumPParams ProtocolParameters { protocolParamMaxCollateralInputs = Nothing } =
    error "toAurumPParams: must specify protocolParamMaxCollateralInputs"


-- ----------------------------------------------------------------------------
-- Conversion functions: protocol paramaters from ledger types
--

fromLedgerPParams
  :: SophieBasedEra era
  -> Ledger.PParams (SophieLedgerEra era)
  -> ProtocolParameters
fromLedgerPParams SophieBasedEraSophie = fromSophiePParams
fromLedgerPParams SophieBasedEraEvie = fromSophiePParams
fromLedgerPParams SophieBasedEraJen    = fromSophiePParams
fromLedgerPParams SophieBasedEraAurum  = fromAurumPParams


fromSophiePParams :: Sophie.PParams ledgerera
                   -> ProtocolParameters
fromSophiePParams
    Sophie.PParams {
      Sophie._minfeeA
    , Sophie._minfeeB
    , Sophie._maxBBSize
    , Sophie._maxTxSize
    , Sophie._maxBHSize
    , Sophie._keyDeposit
    , Sophie._poolDeposit
    , Sophie._eMax
    , Sophie._nOpt
    , Sophie._a0
    , Sophie._rho
    , Sophie._tau
    , Sophie._d
    , Sophie._extraEntropy
    , Sophie._protocolVersion
    , Sophie._minUTxOValue
    , Sophie._minPoolCost
    } =
    ProtocolParameters {
      protocolParamProtocolVersion     = (\(Ledger.ProtVer a b) -> (a,b))
                                           _protocolVersion
    , protocolParamDecentralization    = Ledger.unboundRational _d
    , protocolParamExtraOptimumEntropy   = fromLedgerNonce _extraEntropy
    , protocolParamMaxBlockHeaderSize  = _maxBHSize
    , protocolParamMaxBlockBodySize    = _maxBBSize
    , protocolParamMaxTxSize           = _maxTxSize
    , protocolParamTxFeeFixed          = _minfeeB
    , protocolParamTxFeePerByte        = _minfeeA
    , protocolParamMinUTxOValue        = Just (fromSophieEntropic _minUTxOValue)
    , protocolParamStakeAddressDeposit = fromSophieEntropic _keyDeposit
    , protocolParamStakePoolDeposit    = fromSophieEntropic _poolDeposit
    , protocolParamMinPoolCost         = fromSophieEntropic _minPoolCost
    , protocolParamPoolRetireMaxEpoch  = _eMax
    , protocolParamStakePoolTargetNum  = _nOpt
    , protocolParamPoolPledgeInfluence = Ledger.unboundRational _a0
    , protocolParamMonetaryExpansion   = Ledger.unboundRational _rho
    , protocolParamTreasuryCut         = Ledger.unboundRational _tau
    , protocolParamUTxOCostPerWord     = Nothing
    , protocolParamCostModels          = Map.empty
    , protocolParamPrices              = Nothing
    , protocolParamMaxTxExUnits        = Nothing
    , protocolParamMaxBlockExUnits     = Nothing
    , protocolParamMaxValueSize        = Nothing
    , protocolParamCollateralPercent   = Nothing
    , protocolParamMaxCollateralInputs = Nothing
    }


fromAurumPParams :: Aurum.PParams ledgerera -> ProtocolParameters
fromAurumPParams
    Aurum.PParams {
      Aurum._minfeeA
    , Aurum._minfeeB
    , Aurum._maxBBSize
    , Aurum._maxTxSize
    , Aurum._maxBHSize
    , Aurum._keyDeposit
    , Aurum._poolDeposit
    , Aurum._eMax
    , Aurum._nOpt
    , Aurum._a0
    , Aurum._rho
    , Aurum._tau
    , Aurum._d
    , Aurum._extraEntropy
    , Aurum._protocolVersion
    , Aurum._minPoolCost
    , Aurum._coinsPerUTxOWord
    , Aurum._costmdls
    , Aurum._prices
    , Aurum._maxTxExUnits
    , Aurum._maxBlockExUnits
    , Aurum._maxValSize
    , Aurum._collateralPercentage
    , Aurum._maxCollateralInputs
    } =
    ProtocolParameters {
      protocolParamProtocolVersion     = (\(Ledger.ProtVer a b) -> (a,b))
                                           _protocolVersion
    , protocolParamDecentralization    = Ledger.unboundRational _d
    , protocolParamExtraOptimumEntropy   = fromLedgerNonce _extraEntropy
    , protocolParamMaxBlockHeaderSize  = _maxBHSize
    , protocolParamMaxBlockBodySize    = _maxBBSize
    , protocolParamMaxTxSize           = _maxTxSize
    , protocolParamTxFeeFixed          = _minfeeB
    , protocolParamTxFeePerByte        = _minfeeA
    , protocolParamMinUTxOValue        = Nothing
    , protocolParamStakeAddressDeposit = fromSophieEntropic _keyDeposit
    , protocolParamStakePoolDeposit    = fromSophieEntropic _poolDeposit
    , protocolParamMinPoolCost         = fromSophieEntropic _minPoolCost
    , protocolParamPoolRetireMaxEpoch  = _eMax
    , protocolParamStakePoolTargetNum  = _nOpt
    , protocolParamPoolPledgeInfluence = Ledger.unboundRational _a0
    , protocolParamMonetaryExpansion   = Ledger.unboundRational _rho
    , protocolParamTreasuryCut         = Ledger.unboundRational _tau
    , protocolParamUTxOCostPerWord     = Just (fromSophieEntropic _coinsPerUTxOWord)
    , protocolParamCostModels          = fromAurumCostModels _costmdls
    , protocolParamPrices              = Just (fromAurumPrices _prices)
    , protocolParamMaxTxExUnits        = Just (fromAurumExUnits _maxTxExUnits)
    , protocolParamMaxBlockExUnits     = Just (fromAurumExUnits _maxBlockExUnits)
    , protocolParamMaxValueSize        = Just _maxValSize
    , protocolParamCollateralPercent   = Just _collateralPercentage
    , protocolParamMaxCollateralInputs = Just _maxCollateralInputs
    }

data ProtocolParametersError =
    PParamsErrorMissingMinUTxoValue AnyBccEra
  | PParamsErrorMissingAurumProtocolParameter
  deriving Show

instance Error ProtocolParametersError where
  displayError (PParamsErrorMissingMinUTxoValue (AnyBccEra era)) =
   "The " <> show era <> " protocol parameters value is missing the following \
       \field: MinUTxoValue. Did you intend to use a " <> show era <> " protocol \
       \ parameters value?"
  displayError PParamsErrorMissingAurumProtocolParameter =
    "The Aurum era protocol parameters in use is missing one or more of the \
    \following fields: UTxOCostPerWord, CostModels, Prices, MaxTxExUnits, \
    \MaxBlockExUnits, MaxValueSize, CollateralPercent, MaxCollateralInputs. Did \
    \you intend to use an Aurum era protocol parameters value?"

checkProtocolParameters
  :: forall era. IsBccEra era
  => SophieBasedEra era
  -> ProtocolParameters
  -> Either ProtocolParametersError ()
checkProtocolParameters sbe ProtocolParameters{..} =
  case sbe of
    SophieBasedEraSophie -> checkMinUTxOVal
    SophieBasedEraEvie -> checkMinUTxOVal
    SophieBasedEraJen -> checkMinUTxOVal
    SophieBasedEraAurum -> checkAurumParams
 where
   era :: BccEra era
   era = sophieBasedToBccEra sbe

   costPerWord = isJust protocolParamUTxOCostPerWord
   cModel = not $ Map.null protocolParamCostModels
   prices = isJust protocolParamPrices
   maxTxUnits = isJust protocolParamMaxTxExUnits
   maxBlockExUnits = isJust protocolParamMaxBlockExUnits
   maxValueSize = isJust protocolParamMaxValueSize
   collateralPercent = isJust protocolParamCollateralPercent
   maxCollateralInputs = isJust protocolParamMaxCollateralInputs

   checkAurumParams :: Either ProtocolParametersError ()
   checkAurumParams =
     if all (== True) [ costPerWord
                      , cModel
                      , prices
                      , maxTxUnits
                      , maxBlockExUnits
                      , maxValueSize
                      , collateralPercent
                      , maxCollateralInputs
                      ]
     then return ()
     else Left PParamsErrorMissingAurumProtocolParameter

   checkMinUTxOVal :: Either ProtocolParametersError ()
   checkMinUTxOVal =
     if isJust protocolParamMinUTxOValue
     then return ()
     else Left . PParamsErrorMissingMinUTxoValue
               $ AnyBccEra era
