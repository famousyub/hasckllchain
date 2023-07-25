{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bcc.CLI.Sophie.Run.Query
  ( SophieQueryCmdError
  , SophieQueryCmdLocalStateQueryError (..)
  , renderSophieQueryCmdError
  , renderLocalStateQueryError
  , runQueryCmd
  , percentage
  , executeQuery
  , queryQueryTip
  ) where

import           Bcc.Api
import           Bcc.Api.Cole
import           Bcc.Api.Sophie

import           Bcc.Binary (decodeFull)
import           Bcc.CLI.Environment (EnvSocketError, readEnvSocketPath, renderEnvSocketError)
import           Bcc.CLI.Helpers (HelpersError (..), hushM, pPrintCBOR, renderHelpersError)
import           Bcc.CLI.Sophie.Orphans ()
import           Bcc.CLI.Sophie.Parsers (OutputFile (..), QueryCmd (..))
import           Bcc.CLI.Types
import           Bcc.Crypto.Hash (hashToBytesAsHex)
import           Bcc.Ledger.Coin
import           Bcc.Ledger.Crypto (StandardCrypto)
import           Bcc.Ledger.Keys (KeyHash (..), KeyRole (..))
import           Bcc.Prelude hiding (atomically)
import           Bcc.Slotting.EpochInfo (hoistEpochInfo)
import           Control.Monad.Trans.Except (except)
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistMaybe, left)
import           Data.Aeson (ToJSON (..), (.=))
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.List (nub)
import           Data.Time.Clock
import           Numeric (showEFloat)
import           Shardagnostic.Consensus.BlockchainTime.WallClock.Types (RelativeTime (..),
                   SystemStart (..), toRelativeTime)
import           Shardagnostic.Consensus.Bcc.Block as Consensus (EraMismatch (..))
import qualified Shardagnostic.Consensus.HardFork.History as Consensus
import           Shardagnostic.Network.Block (Serialised (..))
import           Shardagnostic.Network.Protocol.LocalStateQuery.Type (AcquireFailure (..))
import           Prelude (String, id)
import           Sophie.Spec.Ledger.EpochBoundary
import           Sophie.Spec.Ledger.LedgerState hiding (_delegations)
import           Sophie.Spec.Ledger.Scripts ()
import           Text.Printf (printf)

import qualified Bcc.CLI.Sophie.Output as O
import qualified Bcc.Ledger.Crypto as Crypto
import qualified Bcc.Ledger.Era as Era
import qualified Bcc.Ledger.Sophie.Constraints as Ledger
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as T
import qualified Data.Text.IO as Text
import qualified Data.Vector as Vector
import qualified Shardagnostic.Consensus.HardFork.History.Qry as Qry
import qualified Shardagnostic.Network.Protocol.LocalStateQuery.Type as LocalStateQuery
import qualified Sophie.Spec.Ledger.API.Protocol as Ledger
import qualified System.IO as IO

{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Use const" -}
{- HLINT ignore "Use let" -}

data SophieQueryCmdError
  = SophieQueryCmdEnvVarSocketErr !EnvSocketError
  | SophieQueryCmdLocalStateQueryError !SophieQueryCmdLocalStateQueryError
  | SophieQueryCmdWriteFileError !(FileError ())
  | SophieQueryCmdHelpersError !HelpersError
  | SophieQueryCmdAcquireFailure !AcquireFailure
  | SophieQueryCmdEraConsensusModeMismatch !AnyConsensusMode !AnyBccEra
  | SophieQueryCmdColeEra
  | SophieQueryCmdPoolIdError (Hash StakePoolKey)
  | SophieQueryCmdEraMismatch !EraMismatch
  | SophieQueryCmdUnsupportedMode !AnyConsensusMode
  | SophieQueryCmdPastHorizon !Qry.PastHorizonException
  | SophieQueryCmdSystemStartUnavailable
  deriving Show

renderSophieQueryCmdError :: SophieQueryCmdError -> Text
renderSophieQueryCmdError err =
  case err of
    SophieQueryCmdEnvVarSocketErr envSockErr -> renderEnvSocketError envSockErr
    SophieQueryCmdLocalStateQueryError lsqErr -> renderLocalStateQueryError lsqErr
    SophieQueryCmdWriteFileError fileErr -> Text.pack (displayError fileErr)
    SophieQueryCmdHelpersError helpersErr -> renderHelpersError helpersErr
    SophieQueryCmdAcquireFailure acquireFail -> Text.pack $ show acquireFail
    SophieQueryCmdColeEra -> "This query cannot be used for the Cole era"
    SophieQueryCmdPoolIdError poolId -> "The pool id does not exist: " <> show poolId
    SophieQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) (AnyBccEra era) ->
      "Consensus mode and era mismatch. Consensus mode: " <> show cMode <>
      " Era: " <> show era
    SophieQueryCmdEraMismatch (EraMismatch ledgerEra queryEra) ->
      "\nAn error mismatch occured." <> "\nSpecified query era: " <> queryEra <>
      "\nCurrent ledger era: " <> ledgerEra
    SophieQueryCmdUnsupportedMode mode -> "Unsupported mode: " <> renderMode mode
    SophieQueryCmdPastHorizon e -> "Past horizon: " <> show e
    SophieQueryCmdSystemStartUnavailable -> "System start unavailable"

runQueryCmd :: QueryCmd -> ExceptT SophieQueryCmdError IO ()
runQueryCmd cmd =
  case cmd of
    QueryProtocolParameters' consensusModeParams network mOutFile ->
      runQueryProtocolParameters consensusModeParams network mOutFile
    QueryTip consensusModeParams network mOutFile ->
      runQueryTip consensusModeParams network mOutFile
    QueryStakePools' consensusModeParams network mOutFile ->
      runQueryStakePools consensusModeParams network mOutFile
    QueryStakeDistribution' consensusModeParams network mOutFile ->
      runQueryStakeDistribution consensusModeParams network mOutFile
    QueryStakeAddressInfo consensusModeParams addr network mOutFile ->
      runQueryStakeAddressInfo consensusModeParams addr network mOutFile
    QueryDebugLedgerState' consensusModeParams network mOutFile ->
      runQueryLedgerState consensusModeParams network mOutFile
    QueryStakeSnapshot' consensusModeParams network poolid ->
      runQueryStakeSnapshot consensusModeParams network poolid
    QueryPoolParams' consensusModeParams network poolid ->
      runQueryPoolParams consensusModeParams network poolid
    QueryProtocolState' consensusModeParams network mOutFile ->
      runQueryProtocolState consensusModeParams network mOutFile
    QueryUTxO' consensusModeParams qFilter networkId mOutFile ->
      runQueryUTxO consensusModeParams qFilter networkId mOutFile

runQueryProtocolParameters
  :: AnyConsensusModeParams
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT SophieQueryCmdError IO ()
runQueryProtocolParameters (AnyConsensusModeParams cModeParams) network mOutFile = do
  SocketPath sockPath <- firstExceptT SophieQueryCmdEnvVarSocketErr
                           readEnvSocketPath
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  result <- liftIO $ executeLocalStateQueryExpr localNodeConnInfo Nothing $ \_ntcVersion -> runExceptT $ do
    anyE@(AnyBccEra era) <- lift $ determineEraExpr cModeParams

    case bccEraStyle era of
      LegacyColeEra -> left SophieQueryCmdColeEra
      SophieBasedEra sbe -> do
        let cMode = consensusModeOnly cModeParams

        eInMode <- toEraInMode era cMode
          & hoistMaybe (SophieQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE)

        ppResult <- lift . queryExpr $ QueryInEra eInMode $ QueryInSophieBasedEra sbe QueryProtocolParameters

        except ppResult & firstExceptT SophieQueryCmdEraMismatch

  writeProtocolParameters mOutFile =<< except (join (first SophieQueryCmdAcquireFailure result))
 where
  writeProtocolParameters
    :: Maybe OutputFile
    -> ProtocolParameters
    -> ExceptT SophieQueryCmdError IO ()
  writeProtocolParameters mOutFile' pparams =
    case mOutFile' of
      Nothing -> liftIO $ LBS.putStrLn (encodePretty pparams)
      Just (OutputFile fpath) ->
        handleIOExceptT (SophieQueryCmdWriteFileError . FileIOError fpath) $
          LBS.writeFile fpath (encodePretty pparams)

-- | Calculate the percentage sync rendered as text.
percentage
  :: RelativeTime
  -- ^ 'tolerance'.  If 'b' - 'a' < 'tolerance', then 100% is reported.  This even if we are 'tolerance' seconds
  -- behind, we are still considered fully synced.
  -> RelativeTime
  -- ^ 'nowTime'.  The time of the most recently synced block.
  -> RelativeTime
  -- ^ 'tipTime'.  The time of the tip of the block chain to which we need to sync.
  -> Text
percentage tolerance a b = Text.pack (printf "%.2f" pc)
  where -- All calculations are in seconds (Integer)
        t  = relativeTimeSeconds tolerance
        -- Plus 1 to prevent division by zero.  The 's' prefix stands for strictly-positive.
        sa = relativeTimeSeconds a + 1
        sb = relativeTimeSeconds b + 1
        -- Fast forward the 'nowTime` by the tolerance, but don't let the result exceed the tip time.
        ua = min (sa + t) sb
        ub = sb
        -- Final percentage to render as text.
        pc = id @Double (fromIntegral ua / fromIntegral  ub) * 100.0

relativeTimeSeconds :: RelativeTime -> Integer
relativeTimeSeconds (RelativeTime dt) = floor (nominalDiffTimeToSeconds dt)

runQueryTip
  :: AnyConsensusModeParams
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT SophieQueryCmdError IO ()
runQueryTip (AnyConsensusModeParams cModeParams) network mOutFile = do
  SocketPath sockPath <- firstExceptT SophieQueryCmdEnvVarSocketErr readEnvSocketPath

  case consensusModeOnly cModeParams of
    BccMode -> do
      let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

      (chainTip, eLocalState) <- liftIO $ queryQueryTip localNodeConnInfo Nothing

      mLocalState <- hushM (first SophieQueryCmdAcquireFailure eLocalState) $ \e ->
        liftIO . T.hPutStrLn IO.stderr $ "Warning: Local state unavailable: " <> renderSophieQueryCmdError e

      let tipSlotNo = case chainTip of
            ChainTipAtGenesis -> 0
            ChainTip slotNo _ _ -> slotNo

      mLocalStateOutput :: Maybe O.QueryTipLocalStateOutput <- fmap join . forM mLocalState $ \localState -> do
        case slotToEpoch tipSlotNo (O.eraHistory localState) of
          Left e -> do
            liftIO . T.hPutStrLn IO.stderr $
              "Warning: Epoch unavailable: " <> renderSophieQueryCmdError (SophieQueryCmdPastHorizon e)
            return Nothing
          Right (epochNo, _, _) -> do
            syncProgressResult <- runExceptT $ do
              systemStart <- fmap getSystemStart (O.mSystemStart localState) & hoistMaybe SophieQueryCmdSystemStartUnavailable
              nowSeconds <- toRelativeTime (SystemStart systemStart) <$> liftIO getCurrentTime
              tipTimeResult <- getProgress tipSlotNo (O.eraHistory localState) & bimap SophieQueryCmdPastHorizon fst & except

              let tolerance = RelativeTime (secondsToNominalDiffTime 600)

              return $ flip (percentage tolerance) nowSeconds tipTimeResult

            mSyncProgress <- hushM syncProgressResult $ \e -> do
              liftIO . T.hPutStrLn IO.stderr $ "Warning: Sync progress unavailable: " <> renderSophieQueryCmdError e

            return $ Just $ O.QueryTipLocalStateOutput
              { O.mEra = Just (O.era localState)
              , O.mEpoch = Just epochNo
              , O.mSyncProgress = mSyncProgress
              }


      let jsonOutput = encodePretty $ O.QueryTipOutput
            { O.chainTip = chainTip
            , O.mLocalState = mLocalStateOutput
            }

      case mOutFile of
        Just (OutputFile fpath) -> liftIO $ LBS.writeFile fpath jsonOutput
        Nothing                 -> liftIO $ LBS.putStrLn        jsonOutput

    mode -> left (SophieQueryCmdUnsupportedMode (AnyConsensusMode mode))

-- | Query the UTxO, filtered by a given set of addresses, from a Sophie node
-- via the local state query protocol.
--

runQueryUTxO
  :: AnyConsensusModeParams
  -> QueryUTxOFilter
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT SophieQueryCmdError IO ()
runQueryUTxO (AnyConsensusModeParams cModeParams)
             qfilter network mOutFile = do
  SocketPath sockPath <- firstExceptT SophieQueryCmdEnvVarSocketErr readEnvSocketPath
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  anyE@(AnyBccEra era) <- determineEra cModeParams localNodeConnInfo
  let cMode = consensusModeOnly cModeParams
  sbe <- getSbe $ bccEraStyle era

  case toEraInMode era cMode of
    Just eInMode -> do
      let query   = QueryInSophieBasedEra sbe (QueryUTxO qfilter)
          qInMode = QueryInEra eInMode query
      result <- executeQuery
                  era
                  cModeParams
                  localNodeConnInfo
                  qInMode
      writeFilteredUTxOs sbe mOutFile result
    Nothing -> left $ SophieQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE


-- | Query the current and future parameters for a stake pool, including the retirement date.
-- Any of these may be empty (in which case a null will be displayed).
--

runQueryPoolParams
  :: AnyConsensusModeParams
  -> NetworkId
  -> Hash StakePoolKey
  -> ExceptT SophieQueryCmdError IO ()
runQueryPoolParams (AnyConsensusModeParams cModeParams) network poolid = do
  SocketPath sockPath <- firstExceptT SophieQueryCmdEnvVarSocketErr readEnvSocketPath
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  anyE@(AnyBccEra era) <- determineEra cModeParams localNodeConnInfo
  let cMode = consensusModeOnly cModeParams
  sbe <- getSbe $ bccEraStyle era

  eInMode <- toEraInMode era cMode
    & hoistMaybe (SophieQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE)

  let qInMode = QueryInEra eInMode . QueryInSophieBasedEra sbe $ QueryDebugLedgerState
  result <- executeQuery era cModeParams localNodeConnInfo qInMode
  obtainLedgerEraClassConstraints sbe (writePoolParams poolid) result


-- | Obtain stake snapshot information for a pool, plus information about the total active stake.
-- This information can be used for leader slot calculation, for example, and has been requested by SPOs.
-- Obtaining the information directly is significantly more time and memory efficient than using a full ledger state dump.
runQueryStakeSnapshot
  :: AnyConsensusModeParams
  -> NetworkId
  -> Hash StakePoolKey
  -> ExceptT SophieQueryCmdError IO ()
runQueryStakeSnapshot (AnyConsensusModeParams cModeParams) network poolid = do
  SocketPath sockPath <- firstExceptT SophieQueryCmdEnvVarSocketErr readEnvSocketPath
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  anyE@(AnyBccEra era) <- determineEra cModeParams localNodeConnInfo
  let cMode = consensusModeOnly cModeParams
  sbe <- getSbe $ bccEraStyle era

  eInMode <- toEraInMode era cMode
    & hoistMaybe (SophieQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE)

  let qInMode = QueryInEra eInMode . QueryInSophieBasedEra sbe $ QueryDebugLedgerState
  result <- executeQuery era cModeParams localNodeConnInfo qInMode
  obtainLedgerEraClassConstraints sbe (writeStakeSnapshot poolid) result


runQueryLedgerState
  :: AnyConsensusModeParams
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT SophieQueryCmdError IO ()
runQueryLedgerState (AnyConsensusModeParams cModeParams)
                    network mOutFile = do
  SocketPath sockPath <- firstExceptT SophieQueryCmdEnvVarSocketErr readEnvSocketPath
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  anyE@(AnyBccEra era) <- determineEra cModeParams localNodeConnInfo
  let cMode = consensusModeOnly cModeParams
  sbe <- getSbe $ bccEraStyle era

  case toEraInMode era cMode of
    Just eInMode -> do
      let qInMode = QueryInEra eInMode
                      . QueryInSophieBasedEra sbe
                      $ QueryDebugLedgerState
      result <- executeQuery
                  era
                  cModeParams
                  localNodeConnInfo
                  qInMode
      obtainLedgerEraClassConstraints sbe (writeLedgerState mOutFile) result
    Nothing -> left $ SophieQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE


runQueryProtocolState
  :: AnyConsensusModeParams
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT SophieQueryCmdError IO ()
runQueryProtocolState (AnyConsensusModeParams cModeParams)
                      network mOutFile = do
  SocketPath sockPath <- firstExceptT SophieQueryCmdEnvVarSocketErr readEnvSocketPath
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  anyE@(AnyBccEra era) <- determineEra cModeParams localNodeConnInfo
  let cMode = consensusModeOnly cModeParams
  sbe <- getSbe $ bccEraStyle era

  case toEraInMode era cMode of
    Just eInMode -> do
      let qInMode = QueryInEra eInMode
                      . QueryInSophieBasedEra sbe
                      $ QueryProtocolState
      result <- executeQuery
                  era
                  cModeParams
                  localNodeConnInfo
                  qInMode
      writeProtocolState mOutFile result
    Nothing -> left $ SophieQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE

-- | Query the current delegations and reward accounts, filtered by a given
-- set of addresses, from a Sophie node via the local state query protocol.

runQueryStakeAddressInfo
  :: AnyConsensusModeParams
  -> StakeAddress
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT SophieQueryCmdError IO ()
runQueryStakeAddressInfo (AnyConsensusModeParams cModeParams)
                         (StakeAddress _ addr) network mOutFile = do
  SocketPath sockPath <- firstExceptT SophieQueryCmdEnvVarSocketErr readEnvSocketPath
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  anyE@(AnyBccEra era) <- determineEra cModeParams localNodeConnInfo
  let cMode = consensusModeOnly cModeParams
  sbe <- getSbe $ bccEraStyle era

  case toEraInMode era cMode of
    Just eInMode -> do
      let stakeAddr = Set.singleton $ fromSophieStakeCredential addr
          query = QueryInEra eInMode
                    . QueryInSophieBasedEra sbe
                    $ QueryStakeAddresses stakeAddr network

      result <- executeQuery
                  era
                  cModeParams
                  localNodeConnInfo
                  query
      writeStakeAddressInfo mOutFile $ DelegationsAndRewards result
    Nothing -> left $ SophieQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE

-- -------------------------------------------------------------------------------------------------

-- | An error that can occur while querying a node's local state.
data SophieQueryCmdLocalStateQueryError
  = AcquireFailureError !LocalStateQuery.AcquireFailure
  | EraMismatchError !EraMismatch
  -- ^ A query from a certain era was applied to a ledger from a different
  -- era.
  | ColeProtocolNotSupportedError
  -- ^ The query does not support the Cole protocol.
  | SophieProtocolEraMismatch
  -- ^ The Sophie protocol only supports the Sophie era.
  deriving (Eq, Show)

renderLocalStateQueryError :: SophieQueryCmdLocalStateQueryError -> Text
renderLocalStateQueryError lsqErr =
  case lsqErr of
    AcquireFailureError err -> "Local state query acquire failure: " <> show err
    EraMismatchError err ->
      "A query from a certain era was applied to a ledger from a different era: " <> show err
    ColeProtocolNotSupportedError ->
      "The attempted local state query does not support the Cole protocol."
    SophieProtocolEraMismatch ->
        "The Sophie protocol mode can only be used with the Sophie era, "
     <> "i.e. with --sophie-mode use --sophie-era flag"

writeStakeAddressInfo
  :: Maybe OutputFile
  -> DelegationsAndRewards
  -> ExceptT SophieQueryCmdError IO ()
writeStakeAddressInfo mOutFile delegsAndRewards =
  case mOutFile of
    Nothing -> liftIO $ LBS.putStrLn (encodePretty delegsAndRewards)
    Just (OutputFile fpath) ->
      handleIOExceptT (SophieQueryCmdWriteFileError . FileIOError fpath)
        $ LBS.writeFile fpath (encodePretty delegsAndRewards)

writeLedgerState :: forall era ledgerera.
                    SophieLedgerEra era ~ ledgerera
                 => ToJSON (DebugLedgerState era)
                 => FromCBOR (DebugLedgerState era)
                 => Maybe OutputFile
                 -> SerialisedDebugLedgerState era
                 -> ExceptT SophieQueryCmdError IO ()
writeLedgerState mOutFile qState@(SerialisedDebugLedgerState serLedgerState) =
  case mOutFile of
    Nothing -> case decodeLedgerState qState of
                 Left bs -> firstExceptT SophieQueryCmdHelpersError $ pPrintCBOR bs
                 Right ledgerState -> liftIO . LBS.putStrLn $ encodePretty ledgerState
    Just (OutputFile fpath) ->
      handleIOExceptT (SophieQueryCmdWriteFileError . FileIOError fpath)
        $ LBS.writeFile fpath $ unSerialised serLedgerState

writeStakeSnapshot :: forall era ledgerera. ()
  => SophieLedgerEra era ~ ledgerera
  => Era.Crypto ledgerera ~ StandardCrypto
  => FromCBOR (DebugLedgerState era)
  => PoolId
  -> SerialisedDebugLedgerState era
  -> ExceptT SophieQueryCmdError IO ()
writeStakeSnapshot (StakePoolKeyHash hk) qState =
  case decodeLedgerState qState of
    -- In the event of decode failure print the CBOR instead
    Left bs -> firstExceptT SophieQueryCmdHelpersError $ pPrintCBOR bs

    Right ledgerState -> do
      -- Ledger State
      let (DebugLedgerState snapshot) = ledgerState

      -- The three stake snapshots, obtained from the ledger state
      let (SnapShots markS setS goS _) = esSnapshots $ nesEs snapshot

      -- Calculate the three pool and active stake values for the given pool
      liftIO . LBS.putStrLn $ encodePretty $ Stakes
        { markPool = getPoolStake hk markS
        , setPool = getPoolStake hk setS
        , goPool = getPoolStake hk goS
        , markTotal = getAllStake markS
        , setTotal = getAllStake setS
        , goTotal = getAllStake goS
        }

-- | Sum all the stake that is held by the pool
getPoolStake :: KeyHash Bcc.Ledger.Keys.StakePool crypto -> SnapShot crypto -> Integer
getPoolStake hash ss = pStake
  where
    Coin pStake = fold s
    (Stake s) = poolStake hash (_delegations ss) (_stake ss)

-- | Sum the active stake from a snapshot
getAllStake :: SnapShot crypto -> Integer
getAllStake (SnapShot stake _ _) = activeStake
  where
    Coin activeStake = fold . unStake $ stake

-- | This function obtains the pool parameters, equivalent to the following jq query on the output of query ledger-state
--   .nesEs.esLState._delegationState._pstate._pParams.<pool_id>
writePoolParams :: forall era ledgerera. ()
  => SophieLedgerEra era ~ ledgerera
  => FromCBOR (DebugLedgerState era)
  => Crypto.Crypto (Era.Crypto ledgerera)
  => Era.Crypto ledgerera ~ StandardCrypto
  => PoolId
  -> SerialisedDebugLedgerState era
  -> ExceptT SophieQueryCmdError IO ()
writePoolParams (StakePoolKeyHash hk) qState =
  case decodeLedgerState qState of
    -- In the event of decode failure print the CBOR instead
    Left bs -> firstExceptT SophieQueryCmdHelpersError $ pPrintCBOR bs

    Right ledgerState -> do
      let DebugLedgerState snapshot = ledgerState
      let poolState = _pstate $ _delegationState $ esLState $ nesEs snapshot

      -- Pool parameters
      let poolParams = Map.lookup hk $ _pParams poolState
      let fPoolParams = Map.lookup hk $ _fPParams poolState
      let retiring = Map.lookup hk $ _retiring poolState

      liftIO . LBS.putStrLn $ encodePretty $ Params poolParams fPoolParams retiring

decodeLedgerState :: forall era. ()
  => FromCBOR (DebugLedgerState era)
  => SerialisedDebugLedgerState era
  -> Either LBS.ByteString (DebugLedgerState era)
decodeLedgerState (SerialisedDebugLedgerState (Serialised ls)) = first (const ls) (decodeFull ls)

writeProtocolState :: Crypto.Crypto StandardCrypto
                   => Maybe OutputFile
                   -> ProtocolState era
                   -> ExceptT SophieQueryCmdError IO ()
writeProtocolState mOutFile ps@(ProtocolState pstate) =
  case mOutFile of
    Nothing -> case decodeProtocolState ps of
                 Left bs -> firstExceptT SophieQueryCmdHelpersError $ pPrintCBOR bs
                 Right chainDepstate -> liftIO . LBS.putStrLn $ encodePretty chainDepstate
    Just (OutputFile fpath) ->
      handleIOExceptT (SophieQueryCmdWriteFileError . FileIOError fpath)
        . LBS.writeFile fpath $ unSerialised pstate
 where
  decodeProtocolState
    :: ProtocolState era
    -> Either LBS.ByteString (Ledger.ChainDepState StandardCrypto)
  decodeProtocolState (ProtocolState (Serialised pbs)) =
    first (const pbs) (decodeFull pbs)

writeFilteredUTxOs :: SophieBasedEra era
                   -> Maybe OutputFile
                   -> UTxO era
                   -> ExceptT SophieQueryCmdError IO ()
writeFilteredUTxOs sophieBasedEra' mOutFile utxo =
    case mOutFile of
      Nothing -> liftIO $ printFilteredUTxOs sophieBasedEra' utxo
      Just (OutputFile fpath) ->
        case sophieBasedEra' of
          SophieBasedEraSophie -> writeUTxo fpath utxo
          SophieBasedEraEvie -> writeUTxo fpath utxo
          SophieBasedEraJen -> writeUTxo fpath utxo
          SophieBasedEraAurum -> writeUTxo fpath utxo
 where
   writeUTxo fpath utxo' =
     handleIOExceptT (SophieQueryCmdWriteFileError . FileIOError fpath)
       $ LBS.writeFile fpath (encodePretty utxo')

printFilteredUTxOs :: SophieBasedEra era -> UTxO era -> IO ()
printFilteredUTxOs sophieBasedEra' (UTxO utxo) = do
  Text.putStrLn title
  putStrLn $ replicate (Text.length title + 2) '-'
  case sophieBasedEra' of
    SophieBasedEraSophie ->
      mapM_ (printUtxo sophieBasedEra') $ Map.toList utxo
    SophieBasedEraEvie ->
      mapM_ (printUtxo sophieBasedEra') $ Map.toList utxo
    SophieBasedEraJen    ->
      mapM_ (printUtxo sophieBasedEra') $ Map.toList utxo
    SophieBasedEraAurum ->
      mapM_ (printUtxo sophieBasedEra') $ Map.toList utxo
 where
   title :: Text
   title =
     "                           TxHash                                 TxIx        Amount"

printUtxo
  :: SophieBasedEra era
  -> (TxIn, TxOut era)
  -> IO ()
printUtxo sophieBasedEra' txInOutTuple =
  case sophieBasedEra' of
    SophieBasedEraSophie ->
      let (TxIn (TxId txhash) (TxIx index), TxOut _ value _) = txInOutTuple
      in Text.putStrLn $
           mconcat
             [ Text.decodeLatin1 (hashToBytesAsHex txhash)
             , textShowN 6 index
             , "        " <> printableValue value
             ]

    SophieBasedEraEvie ->
      let (TxIn (TxId txhash) (TxIx index), TxOut _ value _) = txInOutTuple
      in Text.putStrLn $
           mconcat
             [ Text.decodeLatin1 (hashToBytesAsHex txhash)
             , textShowN 6 index
             , "        " <> printableValue value
             ]
    SophieBasedEraJen ->
      let (TxIn (TxId txhash) (TxIx index), TxOut _ value _) = txInOutTuple
      in Text.putStrLn $
           mconcat
             [ Text.decodeLatin1 (hashToBytesAsHex txhash)
             , textShowN 6 index
             , "        " <> printableValue value
             ]
    SophieBasedEraAurum ->
      let (TxIn (TxId txhash) (TxIx index), TxOut _ value mDatum) = txInOutTuple
      in Text.putStrLn $
           mconcat
             [ Text.decodeLatin1 (hashToBytesAsHex txhash)
             , textShowN 6 index
             , "        " <> printableValue value <> " + " <> Text.pack (show mDatum)
             ]
 where
  textShowN :: Show a => Int -> a -> Text
  textShowN len x =
    let str = show x
        slen = length str
    in Text.pack $ replicate (max 1 (len - slen)) ' ' ++ str

  printableValue :: TxOutValue era -> Text
  printableValue (TxOutValue _ val) = renderValue val
  printableValue (TxOutBccOnly _ (Entropic i)) = Text.pack $ show i

runQueryStakePools
  :: AnyConsensusModeParams
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT SophieQueryCmdError IO ()
runQueryStakePools (AnyConsensusModeParams cModeParams)
                          network mOutFile = do
  SocketPath sockPath <- firstExceptT SophieQueryCmdEnvVarSocketErr readEnvSocketPath

  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  result <- ExceptT . fmap (join . first SophieQueryCmdAcquireFailure) $
    executeLocalStateQueryExpr localNodeConnInfo Nothing $ \_ntcVersion -> runExceptT @SophieQueryCmdError $ do
      anyE@(AnyBccEra era) <- case consensusModeOnly cModeParams of
        ColeMode -> return $ AnyBccEra ColeEra
        SophieMode -> return $ AnyBccEra SophieEra
        BccMode -> lift . queryExpr $ QueryCurrentEra BccModeIsMultiEra

      let cMode = consensusModeOnly cModeParams

      case toEraInMode era cMode of
        Just eInMode -> do
          sbe <- getSbe $ bccEraStyle era

          firstExceptT SophieQueryCmdEraMismatch . ExceptT $
            queryExpr . QueryInEra eInMode . QueryInSophieBasedEra sbe $ QueryStakePools

        Nothing -> left $ SophieQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE

  writeStakePools mOutFile result

writeStakePools
  :: Maybe OutputFile
  -> Set PoolId
  -> ExceptT SophieQueryCmdError IO ()
writeStakePools (Just (OutputFile outFile)) stakePools =
  handleIOExceptT (SophieQueryCmdWriteFileError . FileIOError outFile) $
    LBS.writeFile outFile (encodePretty stakePools)

writeStakePools Nothing stakePools =
  forM_ (Set.toList stakePools) $ \poolId ->
    liftIO . putStrLn $ Text.unpack (serialiseToBech32 poolId)

runQueryStakeDistribution
  :: AnyConsensusModeParams
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT SophieQueryCmdError IO ()
runQueryStakeDistribution (AnyConsensusModeParams cModeParams)
                          network mOutFile = do
  SocketPath sockPath <- firstExceptT SophieQueryCmdEnvVarSocketErr readEnvSocketPath
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  anyE@(AnyBccEra era) <- determineEra cModeParams localNodeConnInfo
  let cMode = consensusModeOnly cModeParams
  sbe <- getSbe $ bccEraStyle era

  case toEraInMode era cMode of
    Just eInMode -> do
      let query = QueryInEra eInMode
                    . QueryInSophieBasedEra sbe
                    $ QueryStakeDistribution
      result <- executeQuery
                  era
                  cModeParams
                  localNodeConnInfo
                  query
      writeStakeDistribution mOutFile result
    Nothing -> left $ SophieQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE


writeStakeDistribution
  :: Maybe OutputFile
  -> Map PoolId Rational
  -> ExceptT SophieQueryCmdError IO ()
writeStakeDistribution (Just (OutputFile outFile)) stakeDistrib =
  handleIOExceptT (SophieQueryCmdWriteFileError . FileIOError outFile) $
    LBS.writeFile outFile (encodePretty stakeDistrib)

writeStakeDistribution Nothing stakeDistrib =
  liftIO $ printStakeDistribution stakeDistrib


printStakeDistribution :: Map PoolId Rational -> IO ()
printStakeDistribution stakeDistrib = do
  Text.putStrLn title
  putStrLn $ replicate (Text.length title + 2) '-'
  sequence_
    [ putStrLn $ showStakeDistr poolId stakeFraction
    | (poolId, stakeFraction) <- Map.toList stakeDistrib ]
 where
   title :: Text
   title =
     "                           PoolId                                 Stake frac"

   showStakeDistr :: PoolId
                  -> Rational
                  -- ^ Stake fraction
                  -> String
   showStakeDistr poolId stakeFraction =
     concat
       [ Text.unpack (serialiseToBech32 poolId)
       , "   "
       , showEFloat (Just 3) (fromRational stakeFraction :: Double) ""
       ]

-- | A mapping of Sophie reward accounts to both the stake pool that they
-- delegate to and their reward account balance.
newtype DelegationsAndRewards
  = DelegationsAndRewards (Map StakeAddress Entropic, Map StakeAddress PoolId)


mergeDelegsAndRewards :: DelegationsAndRewards -> [(StakeAddress, Maybe Entropic, Maybe PoolId)]
mergeDelegsAndRewards (DelegationsAndRewards (rewardsMap, delegMap)) =
 [ (stakeAddr, Map.lookup stakeAddr rewardsMap, Map.lookup stakeAddr delegMap)
 | stakeAddr <- nub $ Map.keys rewardsMap ++ Map.keys delegMap
 ]


instance ToJSON DelegationsAndRewards where
  toJSON delegsAndRwds =
      Aeson.Array . Vector.fromList
        . map delegAndRwdToJson $ mergeDelegsAndRewards delegsAndRwds
    where
      delegAndRwdToJson :: (StakeAddress, Maybe Entropic, Maybe PoolId) -> Aeson.Value
      delegAndRwdToJson (addr, mRewards, mPoolId) =
        Aeson.object
          [ "address" .= serialiseAddress addr
          , "delegation" .= mPoolId
          , "rewardAccountBalance" .= mRewards
          ]

-- Helpers

calcEraInMode
  :: BccEra era
  -> ConsensusMode mode
  -> ExceptT SophieQueryCmdError IO (EraInMode era mode)
calcEraInMode era mode=
  hoistMaybe (SophieQueryCmdEraConsensusModeMismatch (AnyConsensusMode mode) (anyBccEra era))
                   $ toEraInMode era mode

determineEra
  :: ConsensusModeParams mode
  -> LocalNodeConnectInfo mode
  -> ExceptT SophieQueryCmdError IO AnyBccEra
determineEra cModeParams localNodeConnInfo =
  case consensusModeOnly cModeParams of
    ColeMode -> return $ AnyBccEra ColeEra
    SophieMode -> return $ AnyBccEra SophieEra
    BccMode -> do
      eraQ <- liftIO . queryNodeLocalState localNodeConnInfo Nothing
                     $ QueryCurrentEra BccModeIsMultiEra
      case eraQ of
        Left acqFail -> left $ SophieQueryCmdAcquireFailure acqFail
        Right anyCarEra -> return anyCarEra

executeQuery
  :: forall result era mode. BccEra era
  -> ConsensusModeParams mode
  -> LocalNodeConnectInfo mode
  -> QueryInMode mode (Either EraMismatch result)
  -> ExceptT SophieQueryCmdError IO result
executeQuery era cModeP localNodeConnInfo q = do
  eraInMode <- calcEraInMode era $ consensusModeOnly cModeP
  case eraInMode of
    ColeEraInColeMode -> left SophieQueryCmdColeEra
    _ -> liftIO execQuery >>= queryResult
 where
   execQuery :: IO (Either AcquireFailure (Either EraMismatch result))
   execQuery = queryNodeLocalState localNodeConnInfo Nothing q

getSbe :: Monad m => BccEraStyle era -> ExceptT SophieQueryCmdError m (SophieBasedEra era)
getSbe LegacyColeEra = left SophieQueryCmdColeEra
getSbe (SophieBasedEra sbe) = return sbe

queryResult
  :: Either AcquireFailure (Either EraMismatch a)
  -> ExceptT SophieQueryCmdError IO a
queryResult eAcq =
  case eAcq of
    Left acqFailure -> left $ SophieQueryCmdAcquireFailure acqFailure
    Right eResult ->
      case eResult of
        Left err -> left . SophieQueryCmdLocalStateQueryError $ EraMismatchError err
        Right result -> return result

obtainLedgerEraClassConstraints
  :: SophieLedgerEra era ~ ledgerera
  => SophieBasedEra era
  -> ((Ledger.SophieBased ledgerera
      , ToJSON (DebugLedgerState era)
      , FromCBOR (DebugLedgerState era)
      , Era.Crypto ledgerera ~ StandardCrypto
      ) => a) -> a
obtainLedgerEraClassConstraints SophieBasedEraSophie f = f
obtainLedgerEraClassConstraints SophieBasedEraEvie f = f
obtainLedgerEraClassConstraints SophieBasedEraJen    f = f
obtainLedgerEraClassConstraints SophieBasedEraAurum  f = f


queryQueryTip
  :: LocalNodeConnectInfo BccMode
  -> Maybe ChainPoint
  -> IO (ChainTip, Either AcquireFailure O.QueryTipLocalState)
queryQueryTip connectInfo mpoint = do
  liftIO $ executeLocalStateQueryExprWithChainSync connectInfo mpoint
    $ \ntcVersion -> do
        era <- queryExpr (QueryCurrentEra BccModeIsMultiEra)
        eraHistory@(EraHistory _ interpreter)
          <- queryExpr (QueryEraHistory BccModeIsMultiEra)
        mSystemStart <- if ntcVersion >= NodeToClientV_9
                        then Just <$> queryExpr QuerySystemStart
                        else return Nothing
        return O.QueryTipLocalState
          { O.era = era
          , O.eraHistory = eraHistory
          , O.mSystemStart = mSystemStart
          , O.epochInfo = hoistEpochInfo (first TransactionValidityIntervalError . runExcept)
                            $ Consensus.interpreterToEpochInfo interpreter
          }