{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Bcc.Api.Orphans () where

import           Prelude

import           Data.Aeson (FromJSON (..), ToJSON (..), object, (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Types (FromJSONKey (..), ToJSONKey (..), toJSONKeyText)
import qualified Data.ByteString.Base16 as B16
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Control.Applicative
import           Control.Iterate.SetAlgebra (BiMap (..), Bimap)

import           Bcc.Api.Json
import           Bcc.Ledger.BaseTypes (StrictMaybe (..), strictMaybeToMaybe)
import qualified Bcc.Ledger.BaseTypes as Ledger
import           Bcc.Ledger.Crypto (StandardCrypto)
import           Bcc.Slotting.Slot (SlotNo (..))
import           Bcc.Slotting.Time (SystemStart (..))

import qualified Bcc.Crypto.Hash.Class as Crypto
import qualified Bcc.Ledger.Aurum as Aurum
import qualified Bcc.Ledger.Aurum.Genesis as Aurum
import qualified Bcc.Ledger.Aurum.Language as Aurum
import qualified Bcc.Ledger.Aurum.PParams as Aurum
import qualified Bcc.Ledger.Aurum.Scripts as Aurum
import qualified Bcc.Ledger.Aurum.TxBody as Aurum
import qualified Bcc.Ledger.Coin as Sophie
import qualified Bcc.Ledger.Core as Core
import qualified Bcc.Ledger.Core as Ledger
import qualified Bcc.Ledger.Crypto as Crypto
import qualified Bcc.Ledger.Era as Ledger
import qualified Bcc.Ledger.Jen.Value as Jen
import qualified Bcc.Ledger.SafeHash as SafeHash
import qualified Bcc.Ledger.Sophie.Constraints as Sophie
import qualified Bcc.Protocol.TOptimum as Optimum
import qualified Shardagnostic.Consensus.Sophie.Eras as Consensus
import qualified Sophie.Spec.Ledger.API as Sophie
import qualified Sophie.Spec.Ledger.EpochBoundary as SophieEpoch
import qualified Sophie.Spec.Ledger.LedgerState as SophieLedger
import           Sophie.Spec.Ledger.PParams (PParamsUpdate)
import qualified Sophie.Spec.Ledger.RewardUpdate as Sophie
import qualified Sophie.Spec.Ledger.Rewards as Sophie

import           Zerepoch.V1.Ledger.Api (defaultCostModelParams)

-- Orphan instances involved in the JSON output of the API queries.
-- We will remove/replace these as we provide more API wrapper types

instance ToJSON (Jen.Value era) where
  toJSON (Jen.Value l ps) =
    object
      [ "entropic" .= toJSON l
      , "policies" .= toJSON ps
      ]

instance ToJSONKey Jen.AssetName where
  toJSONKey = toJSONKeyText render
    where
      render = Text.decodeLatin1 . B16.encode . Jen.assetName

instance ToJSON (Jen.PolicyID era) where
  toJSON (Jen.PolicyID (Sophie.ScriptHash h)) = Aeson.String (hashToText h)

instance ToJSONKey (Jen.PolicyID era) where
  toJSONKey = toJSONKeyText render
    where
      render (Jen.PolicyID (Sophie.ScriptHash h)) = hashToText h

instance ToJSON Jen.AssetName where
  toJSON = Aeson.String . Text.decodeLatin1 . B16.encode . Jen.assetName

instance ToJSON Sophie.AccountState where
  toJSON (Sophie.AccountState tr rs) = object [ "treasury" .= tr
                                               , "reserves" .= rs
                                               ]

instance ( Consensus.SophieBasedEra era
         , ToJSON (Core.TxOut era)
         , ToJSON (Core.PParams era)
         , ToJSON (Core.PParamsDelta era)
         ) => ToJSON (Sophie.EpochState era) where
  toJSON eState = object [ "esAccountState" .= Sophie.esAccountState eState
                         , "esSnapshots" .= Sophie.esSnapshots eState
                         , "esLState" .= Sophie.esLState eState
                         , "esPrevPp" .= Sophie.esPrevPp eState
                         , "esPp" .= Sophie.esPp eState
                         , "esNonMyopic" .= Sophie.esNonMyopic eState
                         ]

instance ( Consensus.SophieBasedEra era
         , ToJSON (Core.TxOut era)
         , ToJSON (Core.PParamsDelta era)
         ) => ToJSON (Sophie.LedgerState era) where
  toJSON lState = object [ "utxoState" .= Sophie._utxoState lState
                         , "delegationState" .= Sophie._delegationState lState
                         ]

instance ( Consensus.SophieBasedEra era
         , ToJSON (Core.TxOut era)
         , ToJSON (Core.PParamsDelta era)
         ) => ToJSON (Sophie.UTxOState era) where
  toJSON utxoState = object [ "utxo" .= Sophie._utxo utxoState
                            , "deposited" .= Sophie._deposited utxoState
                            , "fees" .= Sophie._fees utxoState
                            , "ppups" .= Sophie._ppups utxoState
                            ]

instance ( ToJSON (Core.PParamsDelta era)
         , Sophie.UsesPParams era
         ) => ToJSON (Sophie.PPUPState era) where
  toJSON ppUpState = object [ "proposals" .= Sophie.proposals ppUpState
                            , "futureProposals" .= Sophie.futureProposals ppUpState
                            ]

instance ( ToJSON (Core.PParamsDelta era)
         , Sophie.UsesPParams era
         ) => ToJSON (Sophie.ProposedPPUpdates era) where
  toJSON (Sophie.ProposedPPUpdates ppUpdates) = toJSON $ Map.toList ppUpdates

instance ToJSON (PParamsUpdate era) where
  toJSON pp =
    Aeson.object $
        [ "minFeeA"               .= x | x <- mbfield (Sophie._minfeeA pp) ]
     ++ [ "minFeeB"               .= x | x <- mbfield (Sophie._minfeeB pp) ]
     ++ [ "maxBlockBodySize"      .= x | x <- mbfield (Sophie._maxBBSize pp) ]
     ++ [ "maxTxSize"             .= x | x <- mbfield (Sophie._maxTxSize pp) ]
     ++ [ "maxBlockHeaderSize"    .= x | x <- mbfield (Sophie._maxBHSize pp) ]
     ++ [ "keyDeposit"            .= x | x <- mbfield (Sophie._keyDeposit pp) ]
     ++ [ "poolDeposit"           .= x | x <- mbfield (Sophie._poolDeposit pp) ]
     ++ [ "eMax"                  .= x | x <- mbfield (Sophie._eMax pp) ]
     ++ [ "nOpt"                  .= x | x <- mbfield (Sophie._nOpt pp) ]
     ++ [ "a0"                    .= x | x <- mbfield (Sophie._a0 pp) ]
     ++ [ "rho"                   .= x | x <- mbfield (Sophie._rho pp) ]
     ++ [ "tau"                   .= x | x <- mbfield (Sophie._tau pp) ]
     ++ [ "decentralisationParam" .= x | x <- mbfield (Sophie._d pp) ]
     ++ [ "extraEntropy"          .= x | x <- mbfield (Sophie._extraEntropy pp) ]
     ++ [ "protocolVersion"       .= x | x <- mbfield (Sophie._protocolVersion pp) ]
     ++ [ "minUTxOValue"          .= x | x <- mbfield (Sophie._minUTxOValue pp) ]
     ++ [ "minPoolCost"           .= x | x <- mbfield (Sophie._minPoolCost pp) ]
    where
      mbfield SNothing  = []
      mbfield (SJust x) = [x]

instance Crypto.Crypto crypto => ToJSON (Sophie.DPState crypto) where
  toJSON dpState = object [ "dstate" .= Sophie._dstate dpState
                          , "pstate" .= Sophie._pstate dpState
                          ]

instance Crypto.Crypto crypto => ToJSON (Sophie.DState crypto) where
  toJSON dState = object [ "rewards" .= Sophie._rewards dState
                         , "delegations" .= SophieLedger._delegations dState
                         , "ptrs" .= Sophie._ptrs dState
                         , "fGenDelegs" .= Map.toList (Sophie._fGenDelegs dState)
                         , "genDelegs" .= Sophie._genDelegs dState
                         , "irwd" .= Sophie._irwd dState
                         ]

instance Crypto.Crypto crypto => ToJSON (SophieLedger.FutureGenDeleg crypto) where
  toJSON fGenDeleg =
    object [ "fGenDelegSlot" .= SophieLedger.fGenDelegSlot fGenDeleg
           , "fGenDelegGenKeyHash" .= SophieLedger.fGenDelegGenKeyHash fGenDeleg
           ]

instance Crypto.Crypto crypto => ToJSON (Sophie.GenDelegs crypto) where
  toJSON (Sophie.GenDelegs delegs) = toJSON delegs

instance Crypto.Crypto crypto => ToJSON (Sophie.InstantaneousRewards crypto) where
  toJSON iRwds = object [ "iRReserves" .= Sophie.iRReserves iRwds
                        , "iRTreasury" .= Sophie.iRTreasury iRwds
                        ]

instance
  Crypto.Crypto crypto =>
  ToJSON (Bimap Sophie.Ptr (Sophie.Credential Sophie.Staking crypto))
  where
  toJSON (MkBiMap ptsStakeM stakePtrSetM) =
    object [ "stakedCreds" .= Map.toList ptsStakeM
           , "credPtrR" .= toJSON stakePtrSetM
           ]

instance ToJSON Sophie.Ptr where
  toJSON (Sophie.Ptr slotNo txIndex certIndex) =
    object [ "slot" .= unSlotNo slotNo
           , "txIndex" .= txIndex
           , "certIndex" .= certIndex
           ]


instance Crypto.Crypto crypto => ToJSON (Sophie.PState crypto) where
  toJSON pState = object [ "pParams pState" .= Sophie._pParams pState
                         , "fPParams pState" .= Sophie._fPParams pState
                         , "retiring pState" .= Sophie._retiring pState
                         ]

instance ( Consensus.SophieBasedEra era
         , ToJSON (Core.TxOut era)
         ) => ToJSON (Sophie.UTxO era) where
  toJSON (Sophie.UTxO utxo) = toJSON utxo

instance ( Consensus.SophieBasedEra era
         , ToJSON (Core.Value era)
         ) => ToJSON (Sophie.TxOut era) where
  toJSON (Sophie.TxOut addr amount) =
    object
      [ "address" .= addr
      , "amount" .= amount
      ]

instance Crypto.Crypto crypto => ToJSON (Sophie.TxIn crypto) where
  toJSON = toJSON . txInToText

instance Crypto.Crypto crypto => ToJSONKey (Sophie.TxIn crypto) where
  toJSONKey = toJSONKeyText txInToText

txInToText :: Crypto.Crypto crypto => Sophie.TxIn crypto -> Text
txInToText (Sophie.TxIn (Sophie.TxId txidHash) ix) =
  hashToText (SafeHash.extractHash txidHash)
    <> Text.pack "#"
    <> Text.pack (show ix)

hashToText :: Crypto.Hash crypto a -> Text
hashToText = Text.decodeLatin1 . Crypto.hashToBytesAsHex

instance Crypto.Crypto crypto => ToJSON (Sophie.NonMyopic crypto) where
  toJSON nonMy = object [ "likelihoodsNM" .= Sophie.likelihoodsNM nonMy
                        , "rewardPotNM" .= Sophie.rewardPotNM nonMy
                        ]

instance ToJSON Sophie.Likelihood where
  toJSON (Sophie.Likelihood llhd) =
    toJSON $ fmap (\(Sophie.LogWeight f) -> exp $ realToFrac f :: Double) llhd

instance Crypto.Crypto crypto => ToJSON (Sophie.SnapShots crypto) where
  toJSON ss = object [ "pstakeMark" .= Sophie._pstakeMark ss
                     , "pstakeSet" .= Sophie._pstakeSet ss
                     , "pstakeGo" .= Sophie._pstakeGo ss
                     , "feeSS" .= Sophie._feeSS ss
                     ]

instance Crypto.Crypto crypto => ToJSON (Sophie.SnapShot crypto) where
  toJSON ss = object [ "stake" .= Sophie._stake ss
                     , "delegations" .= SophieEpoch._delegations ss
                     , "poolParams" .= Sophie._poolParams ss
                     ]

instance Crypto.Crypto crypto => ToJSON (Sophie.Stake crypto) where
  toJSON (Sophie.Stake s) = toJSON s

instance Crypto.Crypto crypto => ToJSON (Sophie.RewardUpdate crypto) where
  toJSON rUpdate = object [ "deltaT" .= Sophie.deltaT rUpdate
                          , "deltaR" .= Sophie.deltaR rUpdate
                          , "rs" .= Sophie.rs rUpdate
                          , "deltaF" .= Sophie.deltaF rUpdate
                          , "nonMyopic" .= Sophie.nonMyopic rUpdate
                          ]

instance Crypto.Crypto crypto => ToJSON (Sophie.PulsingRewUpdate crypto) where
  toJSON (Sophie.Pulsing _ _) = Aeson.Null
  toJSON (Sophie.Complete ru) = toJSON ru

instance ToJSON Sophie.DeltaCoin where
  toJSON (Sophie.DeltaCoin i) = toJSON i

instance Crypto.Crypto crypto => ToJSON (Optimum.PoolDistr crypto) where
  toJSON (Optimum.PoolDistr m) = toJSON m

instance Crypto.Crypto crypto => ToJSON (Optimum.IndividualPoolStake crypto) where
  toJSON indivPoolStake =
    object [ "individualPoolStake" .= Optimum.individualPoolStake indivPoolStake
           , "individualPoolStakeVrf" .= Optimum.individualPoolStakeVrf indivPoolStake
           ]

instance Crypto.Crypto crypto => ToJSON (Sophie.Reward crypto) where
  toJSON reward =
     object [ "rewardType" .= Sophie.rewardType reward
            , "rewardPool" .= Sophie.rewardPool reward
            , "rewardAmount" .= Sophie.rewardAmount reward
            ]

instance ToJSON Sophie.RewardType where
  toJSON Sophie.MemberReward = "MemberReward"
  toJSON Sophie.LeaderReward = "LeaderReward"

instance Crypto.Crypto c => ToJSON (SafeHash.SafeHash c a) where
  toJSON = toJSON . SafeHash.extractHash

-----

instance ToJSON Aurum.ExUnits
deriving instance FromJSON Aurum.ExUnits

instance ToJSON Aurum.Prices where
  toJSON Aurum.Prices { Aurum.prSteps, Aurum.prMem } =
    -- We cannot round-trip via NonNegativeInterval, so we go via Rational
    object [ "prSteps" .= toRationalJSON (Ledger.unboundRational prSteps)
           , "prMem"   .= toRationalJSON (Ledger.unboundRational prMem)
           ]

instance FromJSON Aurum.Prices where
  parseJSON =
    Aeson.withObject "prices" $ \o -> do
      steps <- o .: "prSteps"
      mem   <- o .: "prMem"
      prSteps <- checkBoundedRational steps
      prMem   <- checkBoundedRational mem
      return Aurum.Prices { Aurum.prSteps, Aurum.prMem }
    where
      -- We cannot round-trip via NonNegativeInterval, so we go via Rational
      checkBoundedRational r =
        case Ledger.boundRational r of
          Nothing -> fail ("too much precision for bounded rational: " ++ show r)
          Just s  -> return s

deriving newtype instance FromJSON Aurum.CostModel
deriving newtype instance ToJSON Aurum.CostModel


languageToText :: Aurum.Language -> Text
languageToText Aurum.ZerepochV1 = "ZerepochV1"

languageFromText :: MonadFail m => Text -> m Aurum.Language
languageFromText "ZerepochV1" = pure Aurum.ZerepochV1
languageFromText lang = fail $ "Error decoding Language: " ++ show lang

instance FromJSON Aurum.Language where
  parseJSON = Aeson.withText "Language" languageFromText

instance ToJSON Aurum.Language where
  toJSON = Aeson.String . languageToText

instance ToJSONKey Aurum.Language where
  toJSONKey = toJSONKeyText languageToText

instance FromJSONKey Aurum.Language where
  fromJSONKey = Aeson.FromJSONKeyTextParser languageFromText

instance FromJSON Aurum.AurumGenesis where
  parseJSON = Aeson.withObject "Aurum Genesis" $ \o -> do
    coinsPerUTxOWord     <- o .:  "entropicPerUTxOWord"
                        <|> o .:  "bccPerUTxOWord" --TODO: deprecate
    cModels              <- o .:? "costModels"
    prices               <- o .:  "executionPrices"
    maxTxExUnits         <- o .:  "maxTxExUnits"
    maxBlockExUnits      <- o .:  "maxBlockExUnits"
    maxValSize           <- o .:  "maxValueSize"
    collateralPercentage <- o .:  "collateralPercentage"
    maxCollateralInputs  <- o .:  "maxCollateralInputs"
    case cModels of
      Nothing -> case Aurum.CostModel <$> defaultCostModelParams of
        Just m -> return Aurum.AurumGenesis
          { Aurum.coinsPerUTxOWord
          , Aurum.costmdls = Map.singleton Aurum.ZerepochV1 m
          , Aurum.prices
          , Aurum.maxTxExUnits
          , Aurum.maxBlockExUnits
          , Aurum.maxValSize
          , Aurum.collateralPercentage
          , Aurum.maxCollateralInputs
          }
        Nothing -> fail "Failed to extract the cost model params from defaultCostModel"
      Just costmdls -> return Aurum.AurumGenesis
        { Aurum.coinsPerUTxOWord
        , Aurum.costmdls
        , Aurum.prices
        , Aurum.maxTxExUnits
        , Aurum.maxBlockExUnits
        , Aurum.maxValSize
        , Aurum.collateralPercentage
        , Aurum.maxCollateralInputs
        }

-- We don't render the cost model so that we can
-- render it later in 'AurumGenWrapper' as a filepath
-- and keep the cost model (which is chunky) as a separate file.
instance ToJSON Aurum.AurumGenesis where
  toJSON v = object
      [ "entropicPerUTxOWord" .= Aurum.coinsPerUTxOWord v
      , "costModels" .= Aurum.costmdls v
      , "executionPrices" .= Aurum.prices v
      , "maxTxExUnits" .= Aurum.maxTxExUnits v
      , "maxBlockExUnits" .= Aurum.maxBlockExUnits v
      , "maxValueSize" .= Aurum.maxValSize v
      , "collateralPercentage" .= Aurum.collateralPercentage v
      , "maxCollateralInputs" .= Aurum.maxCollateralInputs v
      ]

instance ToJSON (Aurum.PParams era) where
  toJSON pp =
    Aeson.object
      [ "minFeeA" .= Aurum._minfeeA pp
      , "minFeeB" .= Aurum._minfeeB pp
      , "maxBlockBodySize" .= Aurum._maxBBSize pp
      , "maxTxSize" .= Aurum._maxTxSize pp
      , "maxBlockHeaderSize" .= Aurum._maxBHSize pp
      , "keyDeposit" .= Aurum._keyDeposit pp
      , "poolDeposit" .= Aurum._poolDeposit pp
      , "eMax" .= Aurum._eMax pp
      , "nOpt" .= Aurum._nOpt pp
      , "a0"  .= Aurum._a0 pp
      , "rho" .= Aurum._rho pp
      , "tau" .= Aurum._tau pp
      , "decentralisationParam" .= Aurum._d pp
      , "extraEntropy" .= Aurum._extraEntropy pp
      , "protocolVersion" .= Aurum._protocolVersion pp
      , "minPoolCost" .= Aurum._minPoolCost pp
      , "entropicPerUTxOWord" .= Aurum._coinsPerUTxOWord pp
      , "costmdls" .= Aurum._costmdls pp
      , "prices" .= Aurum._prices pp
      , "maxTxExUnits" .= Aurum._maxTxExUnits pp
      , "maxBlockExUnits" .= Aurum._maxBlockExUnits pp
      , "maxValSize" .= Aurum._maxValSize pp
      , "collateralPercentage" .= Aurum._collateralPercentage pp
      , "maxCollateralInputs " .= Aurum._maxCollateralInputs pp
      ]

instance FromJSON (Aurum.PParams era) where
  parseJSON =
    Aeson.withObject "PParams" $ \obj ->
      Aurum.PParams
        <$> obj .: "minFeeA"
        <*> obj .: "minFeeB"
        <*> obj .: "maxBlockBodySize"
        <*> obj .: "maxTxSize"
        <*> obj .: "maxBlockHeaderSize"
        <*> obj .: "keyDeposit"
        <*> obj .: "poolDeposit"
        <*> obj .: "eMax"
        <*> obj .: "nOpt"
        <*> obj .: "a0"
        <*> obj .: "rho"
        <*> obj .: "tau"
        <*> obj .: "decentralisationParam"
        <*> obj .: "extraEntropy"
        <*> obj .: "protocolVersion"
        <*> obj .: "minPoolCost" .!= mempty
        <*> obj .: "entropicPerUTxOWord"
        <*> obj .: "costmdls"
        <*> obj .: "prices"
        <*> obj .: "maxTxExUnits"
        <*> obj .: "maxBlockExUnits"
        <*> obj .: "maxValSize"
        <*> obj .: "collateralPercentage"
        <*> obj .: "maxCollateralInputs"

deriving instance ToJSON (Aurum.PParamsUpdate (Aurum.AurumEra StandardCrypto))

instance (Ledger.Era era, Show (Ledger.Value era), ToJSON (Ledger.Value era))
    => ToJSON (Aurum.TxOut era) where
  toJSON (Aurum.TxOut addr v dataHash) =
    object [ "address" .= toJSON addr
           , "value" .= toJSON v
           , "datahash" .= case strictMaybeToMaybe dataHash of
                             Nothing -> Aeson.Null
                             Just dHash ->
                               Aeson.String . Crypto.hashToTextAsHex
                                 $ SafeHash.extractHash dHash
           ]

deriving instance Show Aurum.AurumGenesis

deriving newtype instance ToJSON SystemStart
deriving newtype instance FromJSON SystemStart