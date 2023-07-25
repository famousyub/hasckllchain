{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- The Sophie ledger uses promoted data kinds which we have to use, but we do
-- not export any from this API. We also use them unticked as nature intended.
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}


-- | Queries from local clients to the node.
--
module Bcc.Api.Query (

    -- * Queries
    QueryInMode(..),
    QueryInEra(..),
    QueryInSophieBasedEra(..),
    QueryUTxOFilter(..),
    UTxO(..),
    UTxOInAnyEra(..),

    -- * Internal conversion functions
    toConsensusQuery,
    fromConsensusQueryResult,

    -- * Wrapper types used in queries
    SerialisedDebugLedgerState(..),
    ProtocolState(..),

    DebugLedgerState(..),

    EraHistory(..),
    SystemStart(..),

    SlotsInEpoch(..),
    SlotsToEpochEnd(..),

    slotToEpoch,

    LedgerState(..),

    getProgress,

    -- * Internal conversion functions
    toLedgerUTxO,
    fromLedgerUTxO,
  ) where

import           Data.Aeson (ToJSON (..), object, (.=))
import           Data.Bifunctor (bimap)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe)
import           Data.SOP.Strict (SListI)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Typeable
import           Prelude

import           Shardagnostic.Network.Protocol.LocalStateQuery.Client (Some (..))

import qualified Shardagnostic.Consensus.HardFork.Combinator as Consensus
import           Shardagnostic.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)
import qualified Shardagnostic.Consensus.HardFork.Combinator.AcrossEras as Consensus
import qualified Shardagnostic.Consensus.HardFork.Combinator.Degenerate as Consensus
import qualified Shardagnostic.Consensus.HardFork.History as History
import qualified Shardagnostic.Consensus.HardFork.History.Qry as Qry

import           Shardagnostic.Consensus.BlockchainTime.WallClock.Types (RelativeTime, SlotLength)
import qualified Shardagnostic.Consensus.Cole.Ledger as Consensus
import           Shardagnostic.Consensus.Bcc.Block (LedgerState (..), StandardCrypto)
import qualified Shardagnostic.Consensus.Bcc.Block as Consensus
import qualified Shardagnostic.Consensus.Ledger.Query as Consensus
import qualified Shardagnostic.Consensus.Sophie.Ledger as Consensus
import           Shardagnostic.Network.Block (Serialised)

import           Bcc.Binary
import           Bcc.Slotting.Time (SystemStart (..))

import qualified Bcc.Chain.Update.Validation.Interface as Cole.Update
import qualified Bcc.Ledger.Core as Core
import qualified Bcc.Ledger.Era as Ledger

import qualified Sophie.Spec.Ledger.API as Sophie
import qualified Sophie.Spec.Ledger.LedgerState as Sophie

import           Bcc.Api.Address
import           Bcc.Api.Block
import           Bcc.Api.Certificate
import           Bcc.Api.Eras
import           Bcc.Api.GenesisParameters
import           Bcc.Api.KeysSophie
import           Bcc.Api.Modes
import           Bcc.Api.NetworkId
import           Bcc.Api.Orphans ()
import           Bcc.Api.ProtocolParameters
import           Bcc.Api.TxBody
import           Bcc.Api.Value

import           Data.Word (Word64)

-- ----------------------------------------------------------------------------
-- Queries
--

data QueryInMode mode result where
  QueryCurrentEra
    :: ConsensusModeIsMultiEra mode
    -> QueryInMode mode AnyBccEra

  QueryInEra
    :: EraInMode era mode
    -> QueryInEra era result
    -> QueryInMode mode (Either EraMismatch result)

  QueryEraHistory
    :: ConsensusModeIsMultiEra mode
    -> QueryInMode mode (EraHistory mode)

  QuerySystemStart
    :: QueryInMode mode SystemStart

data EraHistory mode where
  EraHistory
    :: ConsensusBlockForMode mode ~ Consensus.HardForkBlock xs
    => ConsensusMode mode
    -> History.Interpreter xs
    -> EraHistory mode

getProgress :: SlotNo -> EraHistory mode -> Either Qry.PastHorizonException (RelativeTime, SlotLength)
getProgress slotNo (EraHistory _ interpreter) = Qry.interpretQuery interpreter (Qry.slotToWallclock slotNo)

--TODO: add support for these
--     QueryEraStart   :: ConsensusModeIsMultiEra mode
--                     -> EraInMode era mode
--                     -> QueryInMode mode (Maybe EraStart)

newtype SlotsInEpoch = SlotsInEpoch Word64

newtype SlotsToEpochEnd = SlotsToEpochEnd Word64

slotToEpoch :: SlotNo -> EraHistory mode -> Either Qry.PastHorizonException (EpochNo, SlotsInEpoch, SlotsToEpochEnd)
slotToEpoch slotNo (EraHistory _ interpreter) = case Qry.interpretQuery interpreter (Qry.slotToEpoch slotNo) of
  Right (epochNumber, slotsInEpoch, slotsToEpochEnd) -> Right (epochNumber, SlotsInEpoch slotsInEpoch, SlotsToEpochEnd slotsToEpochEnd)
  Left e -> Left e

deriving instance Show (QueryInMode mode result)

data QueryInEra era result where
     QueryColeUpdateState :: QueryInEra ColeEra ColeUpdateState

     QueryInSophieBasedEra :: SophieBasedEra era
                            -> QueryInSophieBasedEra era result
                            -> QueryInEra era result

deriving instance Show (QueryInEra era result)


data QueryInSophieBasedEra era result where
     QueryChainPoint
       :: QueryInSophieBasedEra era ChainPoint

     QueryEpoch
       :: QueryInSophieBasedEra era EpochNo

     QueryGenesisParameters
       :: QueryInSophieBasedEra era GenesisParameters

     QueryProtocolParameters
       :: QueryInSophieBasedEra era ProtocolParameters

     QueryProtocolParametersUpdate
       :: QueryInSophieBasedEra era
            (Map (Hash GenesisKey) ProtocolParametersUpdate)

     QueryStakeDistribution
       :: QueryInSophieBasedEra era (Map (Hash StakePoolKey) Rational)

     QueryUTxO
       :: QueryUTxOFilter
       -> QueryInSophieBasedEra era (UTxO era)

     QueryStakeAddresses
       :: Set StakeCredential
       -> NetworkId
       -> QueryInSophieBasedEra era (Map StakeAddress Entropic,
                                      Map StakeAddress PoolId)

     QueryStakePools
       :: QueryInSophieBasedEra era (Set PoolId)

     QueryStakePoolParameters
       :: Set PoolId
       -> QueryInSophieBasedEra era (Map PoolId StakePoolParameters)

     -- TODO: add support for RewardProvenance
     -- QueryPoolRanking
     --   :: QueryInSophieBasedEra era RewardProvenance

     QueryDebugLedgerState
       :: QueryInSophieBasedEra era (SerialisedDebugLedgerState era)

     QueryProtocolState
       :: QueryInSophieBasedEra era (ProtocolState era)

deriving instance Show (QueryInSophieBasedEra era result)


-- ----------------------------------------------------------------------------
-- Wrapper types used in queries
--

-- | Getting the /whole/ UTxO is obviously not efficient since the result can
-- be huge. Filtering by address is also not efficient because it requires a
-- linear search.
--
-- The 'QueryUTxOFilterByTxIn' is efficient since it fits with the structure of
-- the UTxO (which is indexed by 'TxIn').
--
data QueryUTxOFilter =
     -- | /O(n) time and space/ for utxo size n
     QueryUTxOWhole

     -- | /O(n) time, O(m) space/ for utxo size n, and address set size m
   | QueryUTxOByAddress (Set AddressAny)

     -- | /O(m log n) time, O(m) space/ for utxo size n, and address set size m
   | QueryUTxOByTxIn (Set TxIn)
  deriving (Eq, Show)

--TODO: provide appropriate instances for these types as needed, e.g. JSON

newtype ColeUpdateState = ColeUpdateState Cole.Update.State
  deriving Show

newtype UTxO era = UTxO (Map TxIn (TxOut era))
  deriving (Eq, Show)

data UTxOInAnyEra where
  UTxOInAnyEra :: BccEra era
               -> UTxO era
               -> UTxOInAnyEra

deriving instance Show UTxOInAnyEra

instance IsBccEra era => ToJSON (UTxO era) where
  toJSON (UTxO m) = toJSON m

newtype SerialisedDebugLedgerState era
  = SerialisedDebugLedgerState (Serialised (Sophie.NewEpochState (SophieLedgerEra era)))

data DebugLedgerState era where
  DebugLedgerState :: SophieLedgerEra era ~ ledgerera => Sophie.NewEpochState ledgerera -> DebugLedgerState era

instance (Typeable era, Sophie.TransLedgerState FromCBOR (SophieLedgerEra era)) => FromCBOR (DebugLedgerState era) where
  fromCBOR = DebugLedgerState <$> (fromCBOR :: Decoder s (Sophie.NewEpochState (SophieLedgerEra era)))

-- TODO: Sophie based era class!
instance ( IsSophieBasedEra era
         , SophieLedgerEra era ~ ledgerera
         , Consensus.SophieBasedEra ledgerera
         , ToJSON (Core.PParams ledgerera)
         , ToJSON (Core.PParamsDelta ledgerera)
         , ToJSON (Core.TxOut ledgerera)) => ToJSON (DebugLedgerState era) where
  toJSON (DebugLedgerState newEpochS) = object [ "lastEpoch" .= Sophie.nesEL newEpochS
                                          , "blocksBefore" .= Sophie.nesBprev newEpochS
                                          , "blocksCurrent" .= Sophie.nesBcur newEpochS
                                          , "stateBefore" .= Sophie.nesEs newEpochS
                                          , "possibleRewardUpdate" .= Sophie.nesRu newEpochS
                                          , "stakeDistrib" .= Sophie.nesPd newEpochS
                                          ]

newtype ProtocolState era
  = ProtocolState (Serialised (Sophie.ChainDepState (Ledger.Crypto (SophieLedgerEra era))))

toSophieAddrSet :: BccEra era
                 -> Set AddressAny
                 -> Set (Sophie.Addr Consensus.StandardCrypto)
toSophieAddrSet era =
    Set.fromList
  . map toSophieAddr
    -- Ignore any addresses that are not appropriate for the era,
    -- e.g. Sophie addresses in the Cole era, as these would not
    -- appear in the UTxO anyway.
  . mapMaybe (anyAddressInEra era)
  . Set.toList


toLedgerUTxO :: SophieLedgerEra era ~ ledgerera
             => Ledger.Crypto ledgerera ~ StandardCrypto
             => SophieBasedEra era
             -> UTxO era
             -> Sophie.UTxO ledgerera
toLedgerUTxO era (UTxO utxo) =
    Sophie.UTxO
  . Map.fromList
  . map (bimap toSophieTxIn (toSophieTxOut era))
  . Map.toList
  $ utxo

fromLedgerUTxO :: SophieLedgerEra era ~ ledgerera
               => Ledger.Crypto ledgerera ~ StandardCrypto
               => SophieBasedEra era
               -> Sophie.UTxO ledgerera
               -> UTxO era
fromLedgerUTxO era (Sophie.UTxO utxo) =
    UTxO
  . Map.fromList
  . map (bimap fromSophieTxIn (fromSophieTxOut era))
  . Map.toList
  $ utxo

fromSophiePoolDistr :: Sophie.PoolDistr StandardCrypto
                     -> Map (Hash StakePoolKey) Rational
fromSophiePoolDistr =
    --TODO: write an appropriate property to show it is safe to use
    -- Map.fromListAsc or to use Map.mapKeysMonotonic
    Map.fromList
  . map (bimap StakePoolKeyHash Sophie.individualPoolStake)
  . Map.toList
  . Sophie.unPoolDistr

fromSophieDelegations :: Map (Sophie.Credential Sophie.Staking StandardCrypto)
                              (Sophie.KeyHash Sophie.StakePool StandardCrypto)
                       -> Map StakeCredential PoolId
fromSophieDelegations =
    --TODO: write an appropriate property to show it is safe to use
    -- Map.fromListAsc or to use Map.mapKeysMonotonic
    -- In this case it may not be: the Ord instances for Sophie.Credential
    -- do not match the one for StakeCredential
    Map.fromList
  . map (bimap fromSophieStakeCredential StakePoolKeyHash)
  . Map.toList

fromSophieRewardAccounts :: Sophie.RewardAccounts Consensus.StandardCrypto
                          -> Map StakeCredential Entropic
fromSophieRewardAccounts =
    --TODO: write an appropriate property to show it is safe to use
    -- Map.fromListAsc or to use Map.mapKeysMonotonic
    Map.fromList
  . map (bimap fromSophieStakeCredential fromSophieEntropic)
  . Map.toList


-- ----------------------------------------------------------------------------
-- Conversions of queries into the consensus types.
--

toConsensusQuery :: forall mode block result.
                    ConsensusBlockForMode mode ~ block
                 => QueryInMode mode result
                 -> Some (Consensus.Query block)
toConsensusQuery (QueryCurrentEra BccModeIsMultiEra) =
    Some $ Consensus.BlockQuery $
      Consensus.QueryHardFork
        Consensus.GetCurrentEra

toConsensusQuery (QueryInEra ColeEraInColeMode QueryColeUpdateState) =
    Some $ Consensus.BlockQuery $
      Consensus.DegenQuery
        Consensus.GetUpdateInterfaceState

toConsensusQuery (QueryEraHistory BccModeIsMultiEra) =
    Some $ Consensus.BlockQuery $
      Consensus.QueryHardFork
        Consensus.GetInterpreter

toConsensusQuery QuerySystemStart = Some Consensus.GetSystemStart

toConsensusQuery (QueryInEra ColeEraInBccMode QueryColeUpdateState) =
    Some $ Consensus.BlockQuery $
      Consensus.QueryIfCurrentCole
        Consensus.GetUpdateInterfaceState

toConsensusQuery (QueryInEra erainmode (QueryInSophieBasedEra era q)) =
    case erainmode of
      ColeEraInColeMode     -> case era of {}
      SophieEraInSophieMode -> toConsensusQuerySophieBased erainmode q
      ColeEraInBccMode   -> case era of {}
      SophieEraInBccMode -> toConsensusQuerySophieBased erainmode q
      EvieEraInBccMode -> toConsensusQuerySophieBased erainmode q
      JenEraInBccMode    -> toConsensusQuerySophieBased erainmode q
      AurumEraInBccMode  -> toConsensusQuerySophieBased erainmode q


toConsensusQuerySophieBased
  :: forall era ledgerera mode block xs result.
     ConsensusBlockForEra era ~ Consensus.SophieBlock ledgerera
  => Ledger.Crypto ledgerera ~ Consensus.StandardCrypto
  => ConsensusBlockForMode mode ~ block
  => block ~ Consensus.HardForkBlock xs
  => EraInMode era mode
  -> QueryInSophieBasedEra era result
  -> Some (Consensus.Query block)
toConsensusQuerySophieBased erainmode QueryChainPoint =
    Some (consensusQueryInEraInMode erainmode Consensus.GetLedgerTip)

toConsensusQuerySophieBased erainmode QueryEpoch =
    Some (consensusQueryInEraInMode erainmode Consensus.GetEpochNo)

toConsensusQuerySophieBased erainmode QueryGenesisParameters =
    Some (consensusQueryInEraInMode erainmode Consensus.GetGenesisConfig)

toConsensusQuerySophieBased erainmode QueryProtocolParameters =
    Some (consensusQueryInEraInMode erainmode Consensus.GetCurrentPParams)

toConsensusQuerySophieBased erainmode QueryProtocolParametersUpdate =
    Some (consensusQueryInEraInMode erainmode Consensus.GetProposedPParamsUpdates)

toConsensusQuerySophieBased erainmode QueryStakeDistribution =
    Some (consensusQueryInEraInMode erainmode Consensus.GetStakeDistribution)

toConsensusQuerySophieBased erainmode (QueryUTxO QueryUTxOWhole) =
    Some (consensusQueryInEraInMode erainmode Consensus.GetUTxOWhole)

toConsensusQuerySophieBased erainmode (QueryUTxO (QueryUTxOByAddress addrs)) =
    Some (consensusQueryInEraInMode erainmode (Consensus.GetUTxOByAddress addrs'))
  where
    addrs' :: Set (Sophie.Addr Consensus.StandardCrypto)
    addrs' = toSophieAddrSet (eraInModeToEra erainmode) addrs

toConsensusQuerySophieBased erainmode (QueryUTxO (QueryUTxOByTxIn txins)) =
    Some (consensusQueryInEraInMode erainmode (Consensus.GetUTxOByTxIn txins'))
  where
    txins' :: Set (Sophie.TxIn Consensus.StandardCrypto)
    txins' = Set.map toSophieTxIn txins

toConsensusQuerySophieBased erainmode (QueryStakeAddresses creds _nId) =
    Some (consensusQueryInEraInMode erainmode
            (Consensus.GetFilteredDelegationsAndRewardAccounts creds'))
  where
    creds' :: Set (Sophie.Credential Sophie.Staking StandardCrypto)
    creds' = Set.map toSophieStakeCredential creds

toConsensusQuerySophieBased erainmode QueryStakePools =
    Some (consensusQueryInEraInMode erainmode Consensus.GetStakePools)

toConsensusQuerySophieBased erainmode (QueryStakePoolParameters poolids) =
    Some (consensusQueryInEraInMode erainmode (Consensus.GetStakePoolParams poolids'))
  where
    poolids' :: Set (Sophie.KeyHash Sophie.StakePool Consensus.StandardCrypto)
    poolids' = Set.map (\(StakePoolKeyHash kh) -> kh) poolids

toConsensusQuerySophieBased erainmode QueryDebugLedgerState =
    Some (consensusQueryInEraInMode erainmode (Consensus.GetCBOR Consensus.DebugNewEpochState))

toConsensusQuerySophieBased erainmode QueryProtocolState =
    Some (consensusQueryInEraInMode erainmode (Consensus.GetCBOR Consensus.DebugChainDepState))

consensusQueryInEraInMode
  :: forall era mode erablock modeblock result result' xs.
     ConsensusBlockForEra era   ~ erablock
  => ConsensusBlockForMode mode ~ modeblock
  => modeblock ~ Consensus.HardForkBlock xs
  => Consensus.HardForkQueryResult xs result ~ result'
  => EraInMode era mode
  -> Consensus.BlockQuery erablock  result
  -> Consensus.Query modeblock result'
consensusQueryInEraInMode erainmode =
    Consensus.BlockQuery
  . case erainmode of
      ColeEraInColeMode     -> Consensus.DegenQuery
      SophieEraInSophieMode -> Consensus.DegenQuery
      ColeEraInBccMode   -> Consensus.QueryIfCurrentCole
      SophieEraInBccMode -> Consensus.QueryIfCurrentSophie
      EvieEraInBccMode -> Consensus.QueryIfCurrentEvie
      JenEraInBccMode    -> Consensus.QueryIfCurrentJen
      AurumEraInBccMode  -> Consensus.QueryIfCurrentAurum

-- ----------------------------------------------------------------------------
-- Conversions of query results from the consensus types.
--

fromConsensusQueryResult :: forall mode block result result'.
                            ConsensusBlockForMode mode ~ block
                         => QueryInMode mode result
                         -> Consensus.Query block result'
                         -> result'
                         -> result
fromConsensusQueryResult (QueryEraHistory BccModeIsMultiEra) q' r' =
    case q' of
      Consensus.BlockQuery (Consensus.QueryHardFork Consensus.GetInterpreter)
        -> EraHistory BccMode r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult QuerySystemStart q' r' =
    case q' of
      Consensus.GetSystemStart
        -> r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult (QueryCurrentEra BccModeIsMultiEra) q' r' =
    case q' of
      Consensus.BlockQuery (Consensus.QueryHardFork Consensus.GetCurrentEra)
        -> anyEraInModeToAnyEra (fromConsensusEraIndex BccMode r')
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult (QueryInEra ColeEraInColeMode
                                     QueryColeUpdateState) q' r' =
    case (q', r') of
      (Consensus.BlockQuery (Consensus.DegenQuery Consensus.GetUpdateInterfaceState),
       Consensus.DegenQueryResult r'')
        -> Right (ColeUpdateState r'')
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult (QueryInEra ColeEraInBccMode
                                     QueryColeUpdateState) q' r' =
    case q' of
      Consensus.BlockQuery
        (Consensus.QueryIfCurrentCole Consensus.GetUpdateInterfaceState)
        -> bimap fromConsensusEraMismatch ColeUpdateState r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult (QueryInEra ColeEraInColeMode
                                     (QueryInSophieBasedEra era _)) _ _ =
    case era of {}

fromConsensusQueryResult (QueryInEra SophieEraInSophieMode
                                     (QueryInSophieBasedEra _era q)) q' r' =
    case (q', r') of
      (Consensus.BlockQuery (Consensus.DegenQuery q''),
       Consensus.DegenQueryResult r'')
        -> Right (fromConsensusQueryResultSophieBased
                    SophieBasedEraSophie q q'' r'')
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult (QueryInEra ColeEraInBccMode
                                     (QueryInSophieBasedEra era _)) _ _ =
    case era of {}

fromConsensusQueryResult (QueryInEra SophieEraInBccMode
                                     (QueryInSophieBasedEra _era q)) q' r' =
    case q' of
      Consensus.BlockQuery (Consensus.QueryIfCurrentSophie q'')
        -> bimap fromConsensusEraMismatch
                 (fromConsensusQueryResultSophieBased
                    SophieBasedEraSophie q q'')
                 r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult (QueryInEra EvieEraInBccMode
                                     (QueryInSophieBasedEra _era q)) q' r' =
    case q' of
      Consensus.BlockQuery (Consensus.QueryIfCurrentEvie q'')
        -> bimap fromConsensusEraMismatch
                 (fromConsensusQueryResultSophieBased
                    SophieBasedEraEvie q q'')
                 r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult (QueryInEra JenEraInBccMode
                                     (QueryInSophieBasedEra _era q)) q' r' =
    case q' of
      Consensus.BlockQuery (Consensus.QueryIfCurrentJen q'')
        -> bimap fromConsensusEraMismatch
                 (fromConsensusQueryResultSophieBased
                    SophieBasedEraJen q q'')
                 r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult (QueryInEra AurumEraInBccMode
                                     (QueryInSophieBasedEra _era q)) q' r' =
    case q' of
      Consensus.BlockQuery (Consensus.QueryIfCurrentAurum q'')
        -> bimap fromConsensusEraMismatch
                 (fromConsensusQueryResultSophieBased
                    SophieBasedEraAurum q q'')
                 r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResultSophieBased
  :: forall era ledgerera result result'.
     SophieLedgerEra era ~ ledgerera
  => Consensus.SophieBasedEra ledgerera
  => Ledger.Crypto ledgerera ~ Consensus.StandardCrypto
  => SophieBasedEra era
  -> QueryInSophieBasedEra era result
  -> Consensus.BlockQuery (Consensus.SophieBlock ledgerera) result'
  -> result'
  -> result
fromConsensusQueryResultSophieBased _ QueryChainPoint q' point =
    case q' of
      Consensus.GetLedgerTip -> fromConsensusPoint point
      _                      -> fromConsensusQueryResultMismatch

fromConsensusQueryResultSophieBased _ QueryEpoch q' epoch =
    case q' of
      Consensus.GetEpochNo -> epoch
      _                    -> fromConsensusQueryResultMismatch

fromConsensusQueryResultSophieBased _ QueryGenesisParameters q' r' =
    case q' of
      Consensus.GetGenesisConfig -> fromSophieGenesis
                                      (Consensus.getCompactGenesis r')
      _                          -> fromConsensusQueryResultMismatch

fromConsensusQueryResultSophieBased era QueryProtocolParameters q' r' =
    case q' of
      Consensus.GetCurrentPParams -> fromLedgerPParams era r'
      _                           -> fromConsensusQueryResultMismatch

fromConsensusQueryResultSophieBased era QueryProtocolParametersUpdate q' r' =
    case q' of
      Consensus.GetProposedPParamsUpdates -> fromLedgerProposedPPUpdates era r'
      _                                   -> fromConsensusQueryResultMismatch

fromConsensusQueryResultSophieBased _ QueryStakeDistribution q' r' =
    case q' of
      Consensus.GetStakeDistribution -> fromSophiePoolDistr r'
      _                              -> fromConsensusQueryResultMismatch

fromConsensusQueryResultSophieBased era (QueryUTxO QueryUTxOWhole) q' utxo' =
    case q' of
      Consensus.GetUTxOWhole -> fromLedgerUTxO era utxo'
      _                      -> fromConsensusQueryResultMismatch

fromConsensusQueryResultSophieBased era (QueryUTxO QueryUTxOByAddress{}) q' utxo' =
    case q' of
      Consensus.GetUTxOByAddress{} -> fromLedgerUTxO era utxo'
      _                            -> fromConsensusQueryResultMismatch

fromConsensusQueryResultSophieBased era (QueryUTxO QueryUTxOByTxIn{}) q' utxo' =
    case q' of
      Consensus.GetUTxOByTxIn{} -> fromLedgerUTxO era utxo'
      _                         -> fromConsensusQueryResultMismatch

fromConsensusQueryResultSophieBased _ (QueryStakeAddresses _ nId) q' r' =
    case q' of
      Consensus.GetFilteredDelegationsAndRewardAccounts{}
        -> let (delegs, rwaccs) = r'
           in ( Map.mapKeys (makeStakeAddress nId) $ fromSophieRewardAccounts rwaccs
              , Map.mapKeys (makeStakeAddress nId) $ fromSophieDelegations delegs
              )
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResultSophieBased _ QueryStakePools q' poolids' =
    case q' of
      Consensus.GetStakePools -> Set.map StakePoolKeyHash poolids'
      _                       -> fromConsensusQueryResultMismatch

fromConsensusQueryResultSophieBased _ QueryStakePoolParameters{} q' poolparams' =
    case q' of
      Consensus.GetStakePoolParams{} -> Map.map fromSophiePoolParams
                                      . Map.mapKeysMonotonic StakePoolKeyHash
                                      $ poolparams'
      _                              -> fromConsensusQueryResultMismatch

fromConsensusQueryResultSophieBased _ QueryDebugLedgerState{} q' r' =
    case q' of
      Consensus.GetCBOR Consensus.DebugNewEpochState -> SerialisedDebugLedgerState r'
      _                                              -> fromConsensusQueryResultMismatch

fromConsensusQueryResultSophieBased _ QueryProtocolState q' r' =
    case q' of
      Consensus.GetCBOR Consensus.DebugChainDepState -> ProtocolState r'
      _                                              -> fromConsensusQueryResultMismatch

-- | This should /only/ happen if we messed up the mapping in 'toConsensusQuery'
-- and 'fromConsensusQueryResult' so they are inconsistent with each other.
--
-- If we do encounter this error it means that 'toConsensusQuery' maps a
-- API query constructor to a certain consensus query constructor but that
-- 'fromConsensusQueryResult' apparently expects a different pairing.
--
-- For example, imagine if 'toConsensusQuery would (incorrectly) map
-- 'QueryChainPoint' to 'Consensus.GetEpochNo' but 'fromConsensusQueryResult'
-- (correctly) expected to find 'Consensus.GetLedgerTip'. This mismatch would
-- trigger this error.
--
-- Such mismatches should be preventable with an appropriate property test.
--
fromConsensusQueryResultMismatch :: a
fromConsensusQueryResultMismatch =
    error "fromConsensusQueryResult: internal query mismatch"


fromConsensusEraMismatch :: SListI xs
                         => Consensus.MismatchEraInfo xs -> EraMismatch
fromConsensusEraMismatch = Consensus.mkEraMismatch

