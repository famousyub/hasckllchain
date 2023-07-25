{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Bcc.Api.LedgerState
  ( -- * Initialization / Accumulation
    Env(..)
  , envSecurityParam
  , LedgerState
      ( ..
      , LedgerStateCole
      , LedgerStateSophie
      , LedgerStateEvie
      , LedgerStateJen
      )
  , initialLedgerState
  , applyBlock
  , ValidationMode(..)
  , applyBlockWithEvents

    -- * Traversing the block chain
  , foldBlocks
  , chainSyncClientWithLedgerState
  , chainSyncClientPipelinedWithLedgerState

   -- * Errors
  , FoldBlocksError(..)
  , GenesisConfigError(..)
  , InitialLedgerStateError(..)
  , renderFoldBlocksError
  , renderGenesisConfigError
  , renderInitialLedgerStateError
  )
  where

import           Prelude

import           Control.Exception
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Except.Extra
import           Data.Aeson as Aeson
import qualified Data.Aeson.Types as Data.Aeson.Types.Internal
import           Data.ByteArray (ByteArrayAccess)
import qualified Data.ByteArray
import           Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import           Data.ByteString.Short as BSS
import           Data.Foldable
import           Data.IORef
import           Data.SOP.Strict (NP (..))
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Word
import qualified Data.Yaml as Yaml
import           System.FilePath

import           Bcc.Api.Block
import           Bcc.Api.Eras
import           Bcc.Api.IPC (ConsensusModeParams,
                   LocalChainSyncClient (LocalChainSyncClientPipelined),
                   LocalNodeClientProtocols (..), LocalNodeClientProtocolsInMode,
                   LocalNodeConnectInfo (..), connectToLocalNode)
import           Bcc.Api.LedgerEvent (LedgerEvent, toLedgerEvent)
import           Bcc.Api.Modes (BccMode)
import           Bcc.Api.NetworkId (NetworkId (..), NetworkMagic (NetworkMagic))
import qualified Bcc.Chain.Genesis
import qualified Bcc.Chain.Update
import           Bcc.Crypto (ProtocolMagicId (unProtocolMagicId), RequiresNetworkMagic (..))
import qualified Bcc.Crypto.Hash.Blake2b
import qualified Bcc.Crypto.Hash.Class
import qualified Bcc.Crypto.Hashing
import qualified Bcc.Crypto.ProtocolMagic
import           Bcc.Ledger.Aurum.Genesis (AurumGenesis (..))
import qualified Bcc.Ledger.BaseTypes as Sophie.Spec
import qualified Bcc.Ledger.Credential as Sophie.Spec
import qualified Bcc.Ledger.Keys as Sophie.Spec
import           Bcc.Slotting.Slot (WithOrigin (At, Origin))
import qualified Bcc.Slotting.Slot as Slot
import           Network.TypedProtocol.Pipelined (Nat (..))
import qualified Shardagnostic.Consensus.Block.Abstract as Consensus
import qualified Shardagnostic.Consensus.Cole.Ledger.Block as Cole
import qualified Shardagnostic.Consensus.Bcc as Consensus
import qualified Shardagnostic.Consensus.Bcc.Block as Consensus
import qualified Shardagnostic.Consensus.Bcc.CanHardFork as Consensus
import qualified Shardagnostic.Consensus.Bcc.Node as Consensus
import qualified Shardagnostic.Consensus.Config as Consensus
import qualified Shardagnostic.Consensus.HardFork.Combinator as Consensus
import qualified Shardagnostic.Consensus.HardFork.Combinator.AcrossEras as HFC
import qualified Shardagnostic.Consensus.HardFork.Combinator.Basics as HFC
import qualified Shardagnostic.Consensus.Ledger.Abstract as Ledger
import qualified Shardagnostic.Consensus.Ledger.Extended as Ledger
import           Shardagnostic.Consensus.Ledger.Basics (LedgerResult (lrEvents), lrResult)
import qualified Shardagnostic.Consensus.Mempool.TxLimits as TxLimits
import qualified Shardagnostic.Consensus.Node.ProtocolInfo as Consensus
import qualified Shardagnostic.Consensus.Sophie.Eras as Sophie
import qualified Shardagnostic.Consensus.Sophie.Ledger.Block as Sophie
import qualified Shardagnostic.Consensus.Sophie.Ledger.Ledger as Sophie
import qualified Shardagnostic.Consensus.Sophie.Protocol as Sophie
import qualified Shardagnostic.Network.Block
import qualified Shardagnostic.Network.Protocol.ChainSync.Client as CS
import qualified Shardagnostic.Network.Protocol.ChainSync.ClientPipelined as CSP
import           Shardagnostic.Network.Protocol.ChainSync.PipelineDecision
import qualified Sophie.Spec.Ledger.Genesis as Sophie.Spec
import qualified Sophie.Spec.Ledger.PParams as Sophie.Spec
import Data.Maybe (mapMaybe)
import Shardagnostic.Consensus.TypeFamilyWrappers (WrapLedgerEvent(WrapLedgerEvent))

data InitialLedgerStateError
  = ILSEConfigFile Text
  -- ^ Failed to read or parse the network config file.
  | ILSEGenesisFile GenesisConfigError
  -- ^ Failed to read or parse a genesis file linked from the network config file.
  | ILSELedgerConsensusConfig GenesisConfigError
  -- ^ Failed to derive the Ledger or Consensus config.

renderInitialLedgerStateError :: InitialLedgerStateError -> Text
renderInitialLedgerStateError ilse = case ilse of
  ILSEConfigFile err ->
    "Failed to read or parse the network config file: " <> err
  ILSEGenesisFile err ->
    "Failed to read or parse a genesis file linked from the network config file: "
    <> renderGenesisConfigError err
  ILSELedgerConsensusConfig err ->
    "Failed to derive the Ledger or Consensus config: "
    <> renderGenesisConfigError err

-- | Get the environment and initial ledger state.
initialLedgerState
  :: FilePath
  -- ^ Path to the bcc-node config file (e.g. <path to bcc-node project>/configuration/bcc/mainnet-config.json)
  ->  ExceptT InitialLedgerStateError IO (Env, LedgerState)
  -- ^ The environment and initial ledger state
initialLedgerState networkConfigFile = do
  -- TODO Once support for querying the ledger config is added to the node, we
  -- can remove the networkConfigFile argument and much of the code in this
  -- module.
  config <- withExceptT ILSEConfigFile
                  (readNetworkConfig (NetworkConfigFile networkConfigFile))
  genesisConfig <- withExceptT ILSEGenesisFile (readBccGenesisConfig config)
  env <- withExceptT ILSELedgerConsensusConfig (except (genesisConfigToEnv genesisConfig))
  let ledgerState = initLedgerStateVar genesisConfig
  return (env, ledgerState)

-- | Apply a single block to the current ledger state.
applyBlock
  :: Env
  -- ^ The environment returned by @initialLedgerState@
  -> LedgerState
  -- ^ The current ledger state
  -> ValidationMode
  -> Block era
  -- ^ Some block to apply
  -> Either Text LedgerState
  -- ^ The new ledger state (or an error).
applyBlock env oldState validationMode block
  = applyBlock' env oldState validationMode $ case block of
      ColeBlock coleBlock -> Consensus.BlockCole coleBlock
      SophieBlock blockEra sophieBlock -> case blockEra of
        SophieBasedEraSophie -> Consensus.BlockSophie sophieBlock
        SophieBasedEraEvie -> Consensus.BlockEvie sophieBlock
        SophieBasedEraJen    -> Consensus.BlockJen sophieBlock
        SophieBasedEraAurum  -> Consensus.BlockAurum sophieBlock

pattern LedgerStateCole
  :: Ledger.LedgerState Cole.ColeBlock
  -> LedgerState
pattern LedgerStateCole st <- LedgerState (Consensus.LedgerStateCole st)

pattern LedgerStateSophie
  :: Ledger.LedgerState (Sophie.SophieBlock (Sophie.SophieEra Sophie.StandardCrypto))
  -> LedgerState
pattern LedgerStateSophie st <- LedgerState  (Consensus.LedgerStateSophie st)

pattern LedgerStateEvie
  :: Ledger.LedgerState (Sophie.SophieBlock (Sophie.EvieEra Sophie.StandardCrypto))
  -> LedgerState
pattern LedgerStateEvie st <- LedgerState  (Consensus.LedgerStateEvie st)

pattern LedgerStateJen
  :: Ledger.LedgerState (Sophie.SophieBlock (Sophie.JenEra Sophie.StandardCrypto))
  -> LedgerState
pattern LedgerStateJen st <- LedgerState  (Consensus.LedgerStateJen st)

{-# COMPLETE LedgerStateCole
           , LedgerStateSophie
           , LedgerStateEvie
           , LedgerStateJen #-}

data FoldBlocksError
  = FoldBlocksInitialLedgerStateError InitialLedgerStateError
  | FoldBlocksApplyBlockError Text

renderFoldBlocksError :: FoldBlocksError -> Text
renderFoldBlocksError fbe = case fbe of
  FoldBlocksInitialLedgerStateError err -> renderInitialLedgerStateError err
  FoldBlocksApplyBlockError err -> "Failed when applying a block: " <> err

-- | Monadic fold over all blocks and ledger states. Stopping @k@ blocks before
-- the node's tip where @k@ is the security parameter.
foldBlocks
  :: forall a.
  FilePath
  -- ^ Path to the bcc-node config file (e.g. <path to bcc-node project>/configuration/bcc/mainnet-config.json)
  -> ConsensusModeParams BccMode
  -- ^ This is needed for the number of slots per epoch for the Cole era (on
  -- mainnet that should be 21600).
  -> FilePath
  -- ^ Path to local bcc-node socket. This is the path specified by the @--socket-path@ command line option when running the node.
  -> ValidationMode
  -> a
  -- ^ The initial accumulator state.
  -> (Env -> LedgerState -> BlockInMode BccMode -> a -> IO a)
  -- ^ Accumulator function Takes:
  --
  --  * Environment (this is a constant over the whole fold).
  --  * The Ledger state (with block @i@ applied) at block @i@.
  --  * Block @i@.
  --  * The accumulator state at block @i - 1@.
  --
  -- And returns:
  --
  --  * The accumulator state at block @i@
  --
  -- Note: This function can safely assume no rollback will occur even though
  -- internally this is implemented with a client protocol that may require
  -- rollback. This is achieved by only calling the accumulator on states/blocks
  -- that are older than the security parameter, k. This has the side effect of
  -- truncating the last k blocks before the node's tip.
  -> ExceptT FoldBlocksError IO a
  -- ^ The final state
foldBlocks nodeConfigFilePath bccModeParams socketPath validationMode state0 accumulate = do
  -- NOTE this was originally implemented with a non-pipelined client then
  -- changed to a pipelined client for a modest speedup:
  --  * Non-pipelined: 1h  0m  19s
  --  * Pipelined:        46m  23s

  (env, ledgerState) <- withExceptT FoldBlocksInitialLedgerStateError
                            (initialLedgerState nodeConfigFilePath)

  -- Place to store the accumulated state
  -- This is a bit ugly, but easy.
  errorIORef <- lift $ newIORef Nothing
  stateIORef <- lift $ newIORef state0

  -- Derive the NetworkId as described in network-magic.md from the
  -- bcc-ledger-specs repo.
  let coleConfig
        = (\(Consensus.WrapPartialLedgerConfig (Consensus.ColePartialLedgerConfig bc _) :* _) -> bc)
        . HFC.getPerEraLedgerConfig
        . HFC.hardForkLedgerConfigPerEra
        $ envLedgerConfig env

      networkMagic
        = NetworkMagic
        $ unProtocolMagicId
        $ Bcc.Chain.Genesis.gdProtocolMagicId
        $ Bcc.Chain.Genesis.configGenesisData coleConfig

      networkId = case Bcc.Chain.Genesis.configReqNetMagic coleConfig of
        RequiresNoMagic -> Mainnet
        RequiresMagic -> Testnet networkMagic

  -- Connect to the node.
  let connectInfo :: LocalNodeConnectInfo BccMode
      connectInfo =
          LocalNodeConnectInfo {
            localConsensusModeParams = bccModeParams,
            localNodeNetworkId       = networkId,
            localNodeSocketPath      = socketPath
          }

  lift $ connectToLocalNode
    connectInfo
    (protocols stateIORef errorIORef env ledgerState)

  lift (readIORef errorIORef) >>= \case
    Just err -> throwE (FoldBlocksApplyBlockError err)
    Nothing -> lift $ readIORef stateIORef
  where

    protocols :: IORef a -> IORef (Maybe Text) -> Env -> LedgerState -> LocalNodeClientProtocolsInMode BccMode
    protocols stateIORef errorIORef env ledgerState =
        LocalNodeClientProtocols {
          localChainSyncClient    = LocalChainSyncClientPipelined (chainSyncClient 50 stateIORef errorIORef env ledgerState),
          localTxSubmissionClient = Nothing,
          localStateQueryClient   = Nothing
        }

    -- | Defines the client side of the chain sync protocol.
    chainSyncClient :: Word32
                    -- ^ The maximum number of concurrent requests.
                    -> IORef a
                    -> IORef (Maybe Text)
                    -- ^ Resulting error if any. Written to once on protocol
                    -- completion.
                    -> Env
                    -> LedgerState
                    -> CSP.ChainSyncClientPipelined
                        (BlockInMode BccMode)
                        ChainPoint
                        ChainTip
                        IO ()
                    -- ^ Client returns maybe an error.
    chainSyncClient pipelineSize stateIORef errorIORef env ledgerState0
      = CSP.ChainSyncClientPipelined $ pure $ clientIdle_RequestMoreN Origin Origin Zero initialLedgerStateHistory
      where
          initialLedgerStateHistory = Seq.singleton (0, ledgerState0, Origin)

          clientIdle_RequestMoreN
            :: WithOrigin BlockNo
            -> WithOrigin BlockNo
            -> Nat n -- Number of requests inflight.
            -> LedgerStateHistory
            -> CSP.ClientPipelinedStIdle n (BlockInMode BccMode) ChainPoint ChainTip IO ()
          clientIdle_RequestMoreN clientTip serverTip n knownLedgerStates
            = case pipelineDecisionMax pipelineSize n clientTip serverTip  of
                Collect -> case n of
                  Succ predN -> CSP.CollectResponse Nothing (clientNextN predN knownLedgerStates)
                _ -> CSP.SendMsgRequestNextPipelined (clientIdle_RequestMoreN clientTip serverTip (Succ n) knownLedgerStates)

          clientNextN
            :: Nat n -- Number of requests inflight.
            -> LedgerStateHistory
            -> CSP.ClientStNext n (BlockInMode BccMode) ChainPoint ChainTip IO ()
          clientNextN n knownLedgerStates =
            CSP.ClientStNext {
                CSP.recvMsgRollForward = \blockInMode@(BlockInMode block@(Block (BlockHeader slotNo _ currBlockNo) _) _era) serverChainTip -> do
                  let newLedgerStateE = applyBlock
                        env
                        (maybe
                          (error "Impossible! Missing Ledger state")
                          (\(_,x,_) -> x)
                          (Seq.lookup 0 knownLedgerStates)
                        )
                        validationMode
                        block
                  case newLedgerStateE of
                    Left err -> clientIdle_DoneN n (Just err)
                    Right newLedgerState -> do
                      let (knownLedgerStates', committedStates) = pushLedgerState env knownLedgerStates slotNo newLedgerState blockInMode
                          newClientTip = At currBlockNo
                          newServerTip = fromChainTip serverChainTip
                      forM_ committedStates $ \(_, currLedgerState, currBlockMay) -> case currBlockMay of
                          Origin -> return ()
                          At currBlock -> do
                            newState <- accumulate env currLedgerState currBlock =<< readIORef stateIORef
                            writeIORef stateIORef newState
                      if newClientTip == newServerTip
                        then  clientIdle_DoneN n Nothing
                        else return (clientIdle_RequestMoreN newClientTip newServerTip n knownLedgerStates')
              , CSP.recvMsgRollBackward = \chainPoint serverChainTip -> do
                  let newClientTip = Origin -- We don't actually keep track of blocks so we temporarily "forget" the tip.
                      newServerTip = fromChainTip serverChainTip
                      truncatedKnownLedgerStates = case chainPoint of
                          ChainPointAtGenesis -> initialLedgerStateHistory
                          ChainPoint slotNo _ -> rollBackLedgerStateHist knownLedgerStates slotNo
                  return (clientIdle_RequestMoreN newClientTip newServerTip n truncatedKnownLedgerStates)
              }

          clientIdle_DoneN
            :: Nat n -- Number of requests inflight.
            -> Maybe Text -- Return value (maybe an error)
            -> IO (CSP.ClientPipelinedStIdle n (BlockInMode BccMode) ChainPoint ChainTip IO ())
          clientIdle_DoneN n errorMay = case n of
            Succ predN -> return (CSP.CollectResponse Nothing (clientNext_DoneN predN errorMay)) -- Ignore remaining message responses
            Zero -> do
              writeIORef errorIORef errorMay
              return (CSP.SendMsgDone ())

          clientNext_DoneN
            :: Nat n -- Number of requests inflight.
            -> Maybe Text -- Return value (maybe an error)
            -> CSP.ClientStNext n (BlockInMode BccMode) ChainPoint ChainTip IO ()
          clientNext_DoneN n errorMay =
            CSP.ClientStNext {
                CSP.recvMsgRollForward = \_ _ -> clientIdle_DoneN n errorMay
              , CSP.recvMsgRollBackward = \_ _ -> clientIdle_DoneN n errorMay
              }

          fromChainTip :: ChainTip -> WithOrigin BlockNo
          fromChainTip ct = case ct of
            ChainTipAtGenesis -> Origin
            ChainTip _ _ bno -> At bno

-- | Wrap a 'ChainSyncClient' with logic that tracks the ledger state.
chainSyncClientWithLedgerState
  :: forall m a.
     Monad m
  => Env
  -> LedgerState
  -- ^ Initial ledger state
  -> ValidationMode
  -> CS.ChainSyncClient (BlockInMode BccMode, Either Text LedgerState)
                        ChainPoint
                        ChainTip
                        m
                        a
  -- ^ A client to wrap. The block is annotated with a 'Either Text
  -- LedgerState'. This is either an error from validating a block or
  -- the current 'LedgerState' from applying the current block. If we
  -- trust the node, then we generally expect blocks to validate. Also note that
  -- after a block fails to validate we may still roll back to a validated
  -- block, in which case the valid 'LedgerState' will be passed here again.
  -> CS.ChainSyncClient (BlockInMode BccMode)
                        ChainPoint
                        ChainTip
                        m
                        a
  -- ^ A client that acts just like the wrapped client but doesn't require the
  -- 'LedgerState' annotation on the block type.
chainSyncClientWithLedgerState env ledgerState0 validationMode (CS.ChainSyncClient clientTop)
  = CS.ChainSyncClient (goClientStIdle initialLedgerStateHistory <$> clientTop)
  where
    goClientStIdle
      :: History (Either Text LedgerState)
      -> CS.ClientStIdle (BlockInMode BccMode, Either Text LedgerState) ChainPoint ChainTip m a
      -> CS.ClientStIdle (BlockInMode BccMode                         ) ChainPoint ChainTip m a
    goClientStIdle history client = case client of
      CS.SendMsgRequestNext a b -> CS.SendMsgRequestNext (goClientStNext history a) (goClientStNext history <$> b)
      CS.SendMsgFindIntersect ps a -> CS.SendMsgFindIntersect ps (goClientStIntersect history a)
      CS.SendMsgDone a -> CS.SendMsgDone a

    -- This is where the magic happens. We intercept the blocks and rollbacks
    -- and use it to maintain the correct ledger state.
    goClientStNext
      :: History (Either Text LedgerState)
      -> CS.ClientStNext (BlockInMode BccMode, Either Text LedgerState) ChainPoint ChainTip m a
      -> CS.ClientStNext (BlockInMode BccMode                         ) ChainPoint ChainTip m a
    goClientStNext history (CS.ClientStNext recvMsgRollForward recvMsgRollBackward) = CS.ClientStNext
      (\blkInMode@(BlockInMode blk@(Block (BlockHeader slotNo _ _) _) _) tip -> CS.ChainSyncClient $ let
          newLedgerStateE = case Seq.lookup 0 history of
            Nothing -> Left "Rolled back too far."
            Just (_, Left err, _) -> Left err
            Just (_, Right oldLedgerState, _) -> applyBlock
                  env
                  oldLedgerState
                  validationMode
                  blk
          (history', _) = pushLedgerState env history slotNo newLedgerStateE blkInMode
          in goClientStIdle history' <$> CS.runChainSyncClient (recvMsgRollForward (blkInMode, newLedgerStateE) tip)
      )
      (\point tip -> let
              history' = case point of
                            ChainPointAtGenesis -> initialLedgerStateHistory
                            ChainPoint slotNo _ -> rollBackLedgerStateHist history slotNo
            in CS.ChainSyncClient $ goClientStIdle history' <$> CS.runChainSyncClient (recvMsgRollBackward point tip)
      )

    goClientStIntersect
      :: History (Either Text LedgerState)
      -> CS.ClientStIntersect (BlockInMode BccMode, Either Text LedgerState) ChainPoint ChainTip m a
      -> CS.ClientStIntersect (BlockInMode BccMode                         ) ChainPoint ChainTip m a
    goClientStIntersect history (CS.ClientStIntersect recvMsgIntersectFound recvMsgIntersectNotFound) = CS.ClientStIntersect
      (\point tip -> CS.ChainSyncClient (goClientStIdle history <$> CS.runChainSyncClient (recvMsgIntersectFound point tip)))
      (\tip -> CS.ChainSyncClient (goClientStIdle history <$> CS.runChainSyncClient (recvMsgIntersectNotFound tip)))

    initialLedgerStateHistory :: History (Either Text LedgerState)
    initialLedgerStateHistory = Seq.singleton (0, Right ledgerState0, Origin)

-- | See 'chainSyncClientWithLedgerState'.
chainSyncClientPipelinedWithLedgerState
  :: forall m a.
     Monad m
  => Env
  -> LedgerState
  -> ValidationMode
  -> CSP.ChainSyncClientPipelined
                        (BlockInMode BccMode, Either Text LedgerState)
                        ChainPoint
                        ChainTip
                        m
                        a
  -> CSP.ChainSyncClientPipelined
                        (BlockInMode BccMode)
                        ChainPoint
                        ChainTip
                        m
                        a
chainSyncClientPipelinedWithLedgerState env ledgerState0 validationMode (CSP.ChainSyncClientPipelined clientTop)
  = CSP.ChainSyncClientPipelined (goClientPipelinedStIdle initialLedgerStateHistory Zero <$> clientTop)
  where
    goClientPipelinedStIdle
      :: History (Either Text LedgerState)
      -> Nat n
      -> CSP.ClientPipelinedStIdle n (BlockInMode BccMode, Either Text LedgerState) ChainPoint ChainTip m a
      -> CSP.ClientPipelinedStIdle n (BlockInMode BccMode                           ) ChainPoint ChainTip m a
    goClientPipelinedStIdle history n client = case client of
      CSP.SendMsgRequestNext a b -> CSP.SendMsgRequestNext (goClientStNext history n a) (goClientStNext history n <$> b)
      CSP.SendMsgRequestNextPipelined a ->  CSP.SendMsgRequestNextPipelined (goClientPipelinedStIdle history (Succ n) a)
      CSP.SendMsgFindIntersect ps a -> CSP.SendMsgFindIntersect ps (goClientPipelinedStIntersect history n a)
      CSP.CollectResponse a b -> case n of
        Succ nPrev -> CSP.CollectResponse ((fmap . fmap) (goClientPipelinedStIdle history n) a) (goClientStNext history nPrev b)
      CSP.SendMsgDone a -> CSP.SendMsgDone a

    -- This is where the magic happens. We intercept the blocks and rollbacks
    -- and use it to maintain the correct ledger state.
    goClientStNext
      :: History (Either Text LedgerState)
      -> Nat n
      -> CSP.ClientStNext n (BlockInMode BccMode, Either Text LedgerState) ChainPoint ChainTip m a
      -> CSP.ClientStNext n (BlockInMode BccMode                           ) ChainPoint ChainTip m a
    goClientStNext history n (CSP.ClientStNext recvMsgRollForward recvMsgRollBackward) = CSP.ClientStNext
      (\blkInMode@(BlockInMode blk@(Block (BlockHeader slotNo _ _) _) _) tip -> let
          newLedgerStateE = case Seq.lookup 0 history of
            Nothing -> Left "Rolled back too far."
            Just (_, Left err, _) -> Left err
            Just (_, Right oldLedgerState, _) -> applyBlock
                  env
                  oldLedgerState
                  validationMode
                  blk
          (history', _) = pushLedgerState env history slotNo newLedgerStateE blkInMode
        in goClientPipelinedStIdle history' n <$> recvMsgRollForward (blkInMode, newLedgerStateE) tip
      )
      (\point tip -> let
          history' = case point of
                        ChainPointAtGenesis -> initialLedgerStateHistory
                        ChainPoint slotNo _ -> rollBackLedgerStateHist history slotNo
        in goClientPipelinedStIdle history' n <$> recvMsgRollBackward point tip
      )

    goClientPipelinedStIntersect
      :: History (Either Text LedgerState)
      -> Nat n
      -> CSP.ClientPipelinedStIntersect (BlockInMode BccMode, Either Text LedgerState) ChainPoint ChainTip m a
      -> CSP.ClientPipelinedStIntersect (BlockInMode BccMode                           ) ChainPoint ChainTip m a
    goClientPipelinedStIntersect history _ (CSP.ClientPipelinedStIntersect recvMsgIntersectFound recvMsgIntersectNotFound) = CSP.ClientPipelinedStIntersect
      (\point tip -> goClientPipelinedStIdle history Zero <$> recvMsgIntersectFound point tip)
      (\tip -> goClientPipelinedStIdle history Zero <$> recvMsgIntersectNotFound tip)

    initialLedgerStateHistory :: History (Either Text LedgerState)
    initialLedgerStateHistory = Seq.singleton (0, Right ledgerState0, Origin)

{- HLINT ignore chainSyncClientPipelinedWithLedgerState "Use fmap" -}

-- | A history of k (security parameter) recent ledger states. The head is the
-- most recent item. Elements are:
--
-- * Slot number that a new block occurred
-- * The ledger state after applying the new block
-- * The new block
--
type LedgerStateHistory = History LedgerState
type History a = Seq (SlotNo, a, WithOrigin (BlockInMode BccMode))

-- | Add a new ledger state to the history
pushLedgerState
  :: Env                -- ^ Environement used to get the security param, k.
  -> History a          -- ^ History of k items.
  -> SlotNo             -- ^ Slot number of the new item.
  -> a                  -- ^ New item to add to the history
  -> BlockInMode BccMode
                        -- ^ The block that (when applied to the previous
                        -- item) resulted in the new item.
  -> (History a, History a)
  -- ^ ( The new history with the new item appended
  --   , Any exisiting items that are now past the security parameter
  --      and hence can no longer be rolled back.
  --   )
pushLedgerState env hist ix st block
  = Seq.splitAt
      (fromIntegral $ envSecurityParam env + 1)
      ((ix, st, At block) Seq.:<| hist)

rollBackLedgerStateHist :: History a -> SlotNo -> History a
rollBackLedgerStateHist hist maxInc = Seq.dropWhileL ((> maxInc) . (\(x,_,_) -> x)) hist

--------------------------------------------------------------------------------
-- Everything below was copied/adapted from db-sync                           --
--------------------------------------------------------------------------------

genesisConfigToEnv
  :: GenesisConfig
  -> Either GenesisConfigError Env
genesisConfigToEnv
  -- enp
  genCfg =
    case genCfg of
      GenesisBcc _ bCfg sCfg _
        | Bcc.Crypto.ProtocolMagic.unProtocolMagicId (Bcc.Chain.Genesis.configProtocolMagicId bCfg) /= Sophie.Spec.sgNetworkMagic (scConfig sCfg) ->
            Left . NEBccConfig $
              mconcat
                [ "ProtocolMagicId ", textShow (Bcc.Crypto.ProtocolMagic.unProtocolMagicId $ Bcc.Chain.Genesis.configProtocolMagicId bCfg)
                , " /= ", textShow (Sophie.Spec.sgNetworkMagic $ scConfig sCfg)
                ]
        | Bcc.Chain.Genesis.gdStartTime (Bcc.Chain.Genesis.configGenesisData bCfg) /= Sophie.Spec.sgSystemStart (scConfig sCfg) ->
            Left . NEBccConfig $
              mconcat
                [ "SystemStart ", textShow (Bcc.Chain.Genesis.gdStartTime $ Bcc.Chain.Genesis.configGenesisData bCfg)
                , " /= ", textShow (Sophie.Spec.sgSystemStart $ scConfig sCfg)
                ]
        | otherwise ->
            let
              topLevelConfig = Consensus.pInfoConfig (mkProtocolInfoBcc genCfg)
            in
            Right $ Env
                  { envLedgerConfig = Consensus.topLevelConfigLedger topLevelConfig
                  , envProtocolConfig = Consensus.topLevelConfigProtocol topLevelConfig
                  }

readNetworkConfig :: NetworkConfigFile -> ExceptT Text IO NodeConfig
readNetworkConfig (NetworkConfigFile ncf) = do
    ncfg <- (except . parseNodeConfig) =<< readByteString ncf "node"
    return ncfg
      { ncColeGenesisFile = adjustGenesisFilePath (mkAdjustPath ncf) (ncColeGenesisFile ncfg)
      , ncSophieGenesisFile = adjustGenesisFilePath (mkAdjustPath ncf) (ncSophieGenesisFile ncfg)
      , ncAurumGenesisFile = adjustGenesisFilePath (mkAdjustPath ncf) (ncAurumGenesisFile ncfg)
      }

data NodeConfig = NodeConfig
  { ncPBftSignatureThreshold :: !(Maybe Double)
  , ncColeGenesisFile :: !GenesisFile
  , ncColeGenesisHash :: !GenesisHashCole
  , ncSophieGenesisFile :: !GenesisFile
  , ncSophieGenesisHash :: !GenesisHashSophie
  , ncAurumGenesisFile :: !GenesisFile
  , ncAurumGenesisHash :: !GenesisHashAurum
  , ncRequiresNetworkMagic :: !Bcc.Crypto.RequiresNetworkMagic
  , ncColeSoftwareVersion :: !Bcc.Chain.Update.SoftwareVersion
  , ncColeProtocolVersion :: !Bcc.Chain.Update.ProtocolVersion

  -- Per-era parameters for the hardfok transitions:
  , ncColeToSophie   :: !(Consensus.ProtocolTransitionParamsSophieBased
                              Sophie.StandardSophie)
  , ncSophieToEvie :: !(Consensus.ProtocolTransitionParamsSophieBased
                              Sophie.StandardEvie)
  , ncEvieToJen    :: !(Consensus.ProtocolTransitionParamsSophieBased
                              Sophie.StandardJen)
  , ncJenToAurum     :: !Consensus.TriggerHardFork
  }

instance FromJSON NodeConfig where
  parseJSON v =
      Aeson.withObject "NodeConfig" parse v
    where
      parse :: Object -> Data.Aeson.Types.Internal.Parser NodeConfig
      parse o =
        NodeConfig
          <$> o .:? "PBftSignatureThreshold"
          <*> fmap GenesisFile (o .: "ColeGenesisFile")
          <*> fmap GenesisHashCole (o .: "ColeGenesisHash")
          <*> fmap GenesisFile (o .: "SophieGenesisFile")
          <*> fmap GenesisHashSophie (o .: "SophieGenesisHash")
          <*> fmap GenesisFile (o .: "AurumGenesisFile")
          <*> fmap GenesisHashAurum (o .: "AurumGenesisHash")
          <*> o .: "RequiresNetworkMagic"
          <*> parseColeSoftwareVersion o
          <*> parseColeProtocolVersion o
          <*> (Consensus.ProtocolTransitionParamsSophieBased ()
                 <$> parseSophieHardForkEpoch o)
          <*> (Consensus.ProtocolTransitionParamsSophieBased ()
                 <$> parseEvieHardForkEpoch o)
          <*> (Consensus.ProtocolTransitionParamsSophieBased ()
                 <$> parseJenHardForkEpoch o)
          <*> parseAurumHardForkEpoch o

      parseColeProtocolVersion :: Object -> Data.Aeson.Types.Internal.Parser Bcc.Chain.Update.ProtocolVersion
      parseColeProtocolVersion o =
        Bcc.Chain.Update.ProtocolVersion
          <$> o .: "LastKnownBlockVersion-Major"
          <*> o .: "LastKnownBlockVersion-Sentry"

      parseColeSoftwareVersion :: Object -> Data.Aeson.Types.Internal.Parser Bcc.Chain.Update.SoftwareVersion
      parseColeSoftwareVersion o =
        Bcc.Chain.Update.SoftwareVersion
          <$> fmap Bcc.Chain.Update.ApplicationName (o .: "ApplicationName")
          <*> o .: "ApplicationVersion"

      parseSophieHardForkEpoch :: Object -> Data.Aeson.Types.Internal.Parser Consensus.TriggerHardFork
      parseSophieHardForkEpoch o =
        asum
          [ Consensus.TriggerHardForkAtEpoch <$> o .: "TestSophieHardForkAtEpoch"
          , pure $ Consensus.TriggerHardForkAtVersion 2 -- Mainnet default
          ]

      parseEvieHardForkEpoch :: Object -> Data.Aeson.Types.Internal.Parser Consensus.TriggerHardFork
      parseEvieHardForkEpoch o =
        asum
          [ Consensus.TriggerHardForkAtEpoch <$> o .: "TestEvieHardForkAtEpoch"
          , pure $ Consensus.TriggerHardForkAtVersion 3 -- Mainnet default
          ]

      parseJenHardForkEpoch :: Object -> Data.Aeson.Types.Internal.Parser Consensus.TriggerHardFork
      parseJenHardForkEpoch o =
        asum
          [ Consensus.TriggerHardForkAtEpoch <$> o .: "TestJenHardForkAtEpoch"
          , pure $ Consensus.TriggerHardForkAtVersion 4 -- Mainnet default
          ]

      parseAurumHardForkEpoch :: Object -> Data.Aeson.Types.Internal.Parser Consensus.TriggerHardFork
      parseAurumHardForkEpoch o =
        asum
          [ Consensus.TriggerHardForkAtEpoch <$> o .: "TestAurumHardForkAtEpoch"
          , pure $ Consensus.TriggerHardForkAtVersion 5 -- Mainnet default
          ]

parseNodeConfig :: ByteString -> Either Text NodeConfig
parseNodeConfig bs =
  case Yaml.decodeEither' bs of
    Left err -> Left $ "Error parsing node config: " <> textShow err
    Right nc -> Right nc

adjustGenesisFilePath :: (FilePath -> FilePath) -> GenesisFile -> GenesisFile
adjustGenesisFilePath f (GenesisFile p) = GenesisFile (f p)

mkAdjustPath :: FilePath -> (FilePath -> FilePath)
mkAdjustPath nodeConfigFilePath fp = takeDirectory nodeConfigFilePath </> fp

readByteString :: FilePath -> Text -> ExceptT Text IO ByteString
readByteString fp cfgType = ExceptT $
  catch (Right <$> BS.readFile fp) $ \(_ :: IOException) ->
    return $ Left $ mconcat
      [ "Cannot read the ", cfgType, " configuration file at : ", Text.pack fp ]

initLedgerStateVar :: GenesisConfig -> LedgerState
initLedgerStateVar genesisConfig = LedgerState
  { clsState = Ledger.ledgerState $ Consensus.pInfoInitLedger protocolInfo
  }
  where
    protocolInfo = mkProtocolInfoBcc genesisConfig

newtype LedgerState = LedgerState
  { clsState :: Ledger.LedgerState
                  (HFC.HardForkBlock
                    (Consensus.BccEras Consensus.StandardCrypto))
  }

data LedgerStateEvents = LedgerStateEvents
  { lseState ::
      Ledger.LedgerState
        ( HFC.HardForkBlock
            (Consensus.BccEras Consensus.StandardCrypto)
        ),
    lseEvents :: [LedgerEvent]
  }

toLedgerStateEvents ::
  LedgerResult
    ( Sophie.LedgerState
        (HFC.HardForkBlock (Consensus.BccEras Sophie.StandardCrypto))
    )
    ( Sophie.LedgerState
        (HFC.HardForkBlock (Consensus.BccEras Sophie.StandardCrypto))
    ) ->
  LedgerStateEvents
toLedgerStateEvents lr = LedgerStateEvents
  { lseState = lrResult lr
  , lseEvents = mapMaybe (toLedgerEvent
      . WrapLedgerEvent @(HFC.HardForkBlock (Consensus.BccEras Sophie.StandardCrypto)))
      $ lrEvents lr
  }

-- Usually only one constructor, but may have two when we are preparing for a HFC event.
data GenesisConfig
  = GenesisBcc
      !NodeConfig
      !Bcc.Chain.Genesis.Config
      !SophieConfig
      !AurumGenesis

data SophieConfig = SophieConfig
  { scConfig :: !(Sophie.Spec.SophieGenesis Sophie.StandardSophie)
  , scGenesisHash :: !GenesisHashSophie
  }

newtype GenesisFile = GenesisFile
  { unGenesisFile :: FilePath
  } deriving Show


newtype GenesisHashCole = GenesisHashCole
  { unGenesisHashCole :: Text
  } deriving newtype (Eq, Show)

newtype GenesisHashSophie = GenesisHashSophie
  { unGenesisHashSophie :: Bcc.Crypto.Hash.Class.Hash Bcc.Crypto.Hash.Blake2b.Blake2b_256 ByteString
  } deriving newtype (Eq, Show)

newtype GenesisHashAurum = GenesisHashAurum
  { unGenesisHashAurum :: Bcc.Crypto.Hash.Class.Hash Bcc.Crypto.Hash.Blake2b.Blake2b_256 ByteString
  } deriving newtype (Eq, Show)

newtype VestedFile = VestedFile
  { unVestedFile :: FilePath
  } deriving Show

newtype VestedHashCole = VestedHashCole
  { _unVestedHashCole :: Text
  } deriving newtype (Eq, Show)

newtype VestedHashSophie = VestedHashSophie
  { _unVestedHashSophie :: Bcc.Crypto.Hash.Class.Hash Bcc.Crypto.Hash.Blake2b.Blake2b_256 ByteString
  } deriving newtype (Eq, Show)

newtype VestedHashAurum = VestedHashAurum
  { _unVestedHashAurum :: Bcc.Crypto.Hash.Class.Hash Bcc.Crypto.Hash.Blake2b.Blake2b_256 ByteString
  } deriving newtype (Eq, Show)

newtype LedgerStateDir = LedgerStateDir
  {  unLedgerStateDir :: FilePath
  } deriving Show

newtype NetworkName = NetworkName
  { unNetworkName :: Text
  } deriving Show

newtype NetworkConfigFile = NetworkConfigFile
  { unNetworkConfigFile :: FilePath
  } deriving Show

newtype SocketPath = SocketPath
  { unSocketPath :: FilePath
  } deriving Show

mkProtocolInfoBcc ::
  GenesisConfig ->
  Consensus.ProtocolInfo
    IO
    (HFC.HardForkBlock
            (Consensus.BccEras Consensus.StandardCrypto))
mkProtocolInfoBcc (GenesisBcc dnc coleGenesis sophieGenesis aurumGenesis)
  = Consensus.protocolInfoBcc
          Consensus.ProtocolParamsCole
            { Consensus.coleGenesis = coleGenesis
            , Consensus.colePbftSignatureThreshold = Consensus.PBftSignatureThreshold <$> ncPBftSignatureThreshold dnc
            , Consensus.coleProtocolVersion = ncColeProtocolVersion dnc
            , Consensus.coleSoftwareVersion = ncColeSoftwareVersion dnc
            , Consensus.coleLeaderCredentials = Nothing
            , Consensus.coleMaxTxCapacityOverrides = TxLimits.mkOverrides TxLimits.noOverridesMeasure
            }
          Consensus.ProtocolParamsSophieBased
            { Consensus.sophieBasedGenesis = scConfig sophieGenesis
            , Consensus.sophieBasedInitialNonce = sophieOptimumNonce sophieGenesis
            , Consensus.sophieBasedLeaderCredentials = []
            }
          Consensus.ProtocolParamsSophie
            { Consensus.sophieProtVer = sophieProtVer dnc
            , Consensus.sophieMaxTxCapacityOverrides = TxLimits.mkOverrides TxLimits.noOverridesMeasure
            }
          Consensus.ProtocolParamsEvie
            { Consensus.evieProtVer = sophieProtVer dnc
            , Consensus.evieMaxTxCapacityOverrides = TxLimits.mkOverrides TxLimits.noOverridesMeasure
            }
          Consensus.ProtocolParamsJen
            { Consensus.jenProtVer = sophieProtVer dnc
            , Consensus.jenMaxTxCapacityOverrides = TxLimits.mkOverrides TxLimits.noOverridesMeasure
            }
          Consensus.ProtocolParamsAurum
            { Consensus.aurumProtVer = sophieProtVer dnc
            , Consensus.aurumMaxTxCapacityOverrides  = TxLimits.mkOverrides TxLimits.noOverridesMeasure
            }
          (ncColeToSophie dnc)
          (ncSophieToEvie dnc)
          (ncEvieToJen dnc)
          (Consensus.ProtocolTransitionParamsSophieBased aurumGenesis (ncJenToAurum dnc))

sophieOptimumNonce :: SophieConfig -> Sophie.Spec.Nonce
sophieOptimumNonce sCfg = Sophie.Spec.Nonce (Bcc.Crypto.Hash.Class.castHash . unGenesisHashSophie $ scGenesisHash sCfg)

sophieProtVer :: NodeConfig -> Sophie.Spec.ProtVer
sophieProtVer dnc =
  let bver = ncColeProtocolVersion dnc in
  Sophie.Spec.ProtVer
    (fromIntegral $ Bcc.Chain.Update.pvMajor bver)
    (fromIntegral $ Bcc.Chain.Update.pvSentry bver)

readBccGenesisConfig
        :: NodeConfig
        -> ExceptT GenesisConfigError IO GenesisConfig
readBccGenesisConfig enc =
  GenesisBcc enc
    <$> readColeGenesisConfig enc
    <*> readSophieGenesisConfig enc
    <*> readAurumGenesisConfig enc

data GenesisConfigError
  = NEError !Text
  | NEColeConfig !FilePath !Bcc.Chain.Genesis.ConfigurationError
  | NESophieConfig !FilePath !Text
  | NEAurumConfig !FilePath !Text
  | NEBccConfig !Text

renderGenesisConfigError :: GenesisConfigError -> Text
renderGenesisConfigError ne =
  case ne of
    NEError t -> "Error: " <> t
    NEColeConfig fp ce ->
      mconcat
        [ "Failed reading Cole genesis file ", textShow fp, ": ", textShow ce
        ]
    NESophieConfig fp txt ->
      mconcat
        [ "Failed reading Sophie genesis file ", textShow fp, ": ", txt
        ]
    NEAurumConfig fp txt ->
      mconcat
        [ "Failed reading Aurum genesis file ", textShow fp, ": ", txt
        ]
    NEBccConfig err ->
      mconcat
        [ "With Bcc protocol, Cole/Sophie config mismatch:\n"
        , "   ", err
        ]

data LookupFail
  = DbLookupBlockHash !ByteString
  | DbLookupBlockId !Word64
  | DbLookupMessage !Text
  | DbLookupTxHash !ByteString
  | DbLookupTxOutPair !ByteString !Word16
  | DbLookupEpochNo !Word64
  | DbLookupSlotNo !Word64
  | DbMetaEmpty
  | DbMetaMultipleRows
  deriving (Eq, Show)

readColeGenesisConfig
        :: NodeConfig
        -> ExceptT GenesisConfigError IO Bcc.Chain.Genesis.Config
readColeGenesisConfig enc = do
  let file = unGenesisFile $ ncColeGenesisFile enc
  genHash <- firstExceptT NEError
                . hoistEither
                $ Bcc.Crypto.Hashing.decodeAbstractHash (unGenesisHashCole $ ncColeGenesisHash enc)
  firstExceptT (NEColeConfig file)
                $ Bcc.Chain.Genesis.mkConfigFromFile (ncRequiresNetworkMagic enc) file genHash

readSophieGenesisConfig
    :: NodeConfig
    -> ExceptT GenesisConfigError IO SophieConfig
readSophieGenesisConfig enc = do
  let file = unGenesisFile $ ncSophieGenesisFile enc
  firstExceptT (NESophieConfig file . renderSophieGenesisError)
    $ readSophieGenesis (GenesisFile file) (ncSophieGenesisHash enc)

readAurumGenesisConfig
    :: NodeConfig
    -> ExceptT GenesisConfigError IO AurumGenesis
readAurumGenesisConfig enc = do
  let file = unGenesisFile $ ncAurumGenesisFile enc
  firstExceptT (NEAurumConfig file . renderAurumGenesisError)
    $ readAurumGenesis (GenesisFile file) (ncAurumGenesisHash enc)

textShow :: Show a => a -> Text
textShow = Text.pack . show

readSophieGenesis
    :: GenesisFile -> GenesisHashSophie
    -> ExceptT SophieGenesisError IO SophieConfig
readSophieGenesis (GenesisFile file) expectedGenesisHash = do
    content <- handleIOExceptT (SophieGenesisReadError file . textShow) $ BS.readFile file
    let genesisHash = GenesisHashSophie (Bcc.Crypto.Hash.Class.hashWith id content)
    checkExpectedGenesisHash genesisHash
    genesis <- firstExceptT (SophieGenesisDecodeError file . Text.pack)
                  . hoistEither
                  $ Aeson.eitherDecodeStrict' content
    pure $ SophieConfig genesis genesisHash
  where
    checkExpectedGenesisHash :: GenesisHashSophie -> ExceptT SophieGenesisError IO ()
    checkExpectedGenesisHash actual =
      if actual /= expectedGenesisHash
        then left (SophieGenesisHashMismatch actual expectedGenesisHash)
        else pure ()

data SophieGenesisError
     = SophieGenesisReadError !FilePath !Text
     | SophieGenesisHashMismatch !GenesisHashSophie !GenesisHashSophie -- actual, expected
     | SophieGenesisDecodeError !FilePath !Text
     deriving Show

renderSophieGenesisError :: SophieGenesisError -> Text
renderSophieGenesisError sge =
    case sge of
      SophieGenesisReadError fp err ->
        mconcat
          [ "There was an error reading the genesis file: ", Text.pack fp
          , " Error: ", err
          ]

      SophieGenesisHashMismatch actual expected ->
        mconcat
          [ "Wrong Sophie genesis file: the actual hash is ", renderHash (unGenesisHashSophie actual)
          , ", but the expected Sophie genesis hash given in the node "
          , "configuration file is ", renderHash (unGenesisHashSophie expected), "."
          ]

      SophieGenesisDecodeError fp err ->
        mconcat
          [ "There was an error parsing the genesis file: ", Text.pack fp
          , " Error: ", err
          ]

readAurumGenesis
    :: GenesisFile -> GenesisHashAurum
    -> ExceptT AurumGenesisError IO AurumGenesis
readAurumGenesis (GenesisFile file) expectedGenesisHash = do
    content <- handleIOExceptT (AurumGenesisReadError file . textShow) $ BS.readFile file
    let genesisHash = GenesisHashAurum (Bcc.Crypto.Hash.Class.hashWith id content)
    checkExpectedGenesisHash genesisHash
    firstExceptT (AurumGenesisDecodeError file . Text.pack)
                  . hoistEither
                  $ Aeson.eitherDecodeStrict' content
  where
    checkExpectedGenesisHash :: GenesisHashAurum -> ExceptT AurumGenesisError IO ()
    checkExpectedGenesisHash actual =
      if actual /= expectedGenesisHash
        then left (AurumGenesisHashMismatch actual expectedGenesisHash)
        else pure ()

data AurumGenesisError
     = AurumGenesisReadError !FilePath !Text
     | AurumGenesisHashMismatch !GenesisHashAurum !GenesisHashAurum -- actual, expected
     | AurumGenesisDecodeError !FilePath !Text
     deriving Show

renderAurumGenesisError :: AurumGenesisError -> Text
renderAurumGenesisError sge =
    case sge of
      AurumGenesisReadError fp err ->
        mconcat
          [ "There was an error reading the genesis file: ", Text.pack fp
          , " Error: ", err
          ]

      AurumGenesisHashMismatch actual expected ->
        mconcat
          [ "Wrong Aurum genesis file: the actual hash is ", renderHash (unGenesisHashAurum actual)
          , ", but the expected Aurum genesis hash given in the node "
          , "configuration file is ", renderHash (unGenesisHashAurum expected), "."
          ]

      AurumGenesisDecodeError fp err ->
        mconcat
          [ "There was an error parsing the genesis file: ", Text.pack fp
          , " Error: ", err
          ]

renderHash :: Bcc.Crypto.Hash.Class.Hash Bcc.Crypto.Hash.Blake2b.Blake2b_256 ByteString -> Text
renderHash h = Text.decodeUtf8 $ Base16.encode (Bcc.Crypto.Hash.Class.hashToBytes h)

newtype StakeCred
  = StakeCred { _unStakeCred :: Sophie.Spec.Credential 'Sophie.Spec.Staking Consensus.StandardCrypto }
  deriving (Eq, Ord)

data Env = Env
  { envLedgerConfig :: HFC.HardForkLedgerConfig (Consensus.BccEras Sophie.StandardCrypto)
  , envProtocolConfig :: Sophie.ConsensusConfig (HFC.HardForkProtocol (Consensus.BccEras Sophie.StandardCrypto))
  }

envSecurityParam :: Env -> Word64
envSecurityParam env = k
  where
    Consensus.SecurityParam k
      = HFC.hardForkConsensusConfigK
      $ envProtocolConfig env

-- | How to do validation when applying a block to a ledger state.
data ValidationMode
  -- | Do all validation implied by the ledger layer's 'applyBlock`.
  = FullValidation
  -- | Only check that the previous hash from the block matches the head hash of
  -- the ledger state.
  | QuickValidation

-- The function 'tickThenReapply' does zero validation, so add minimal
-- validation ('blockPrevHash' matches the tip hash of the 'LedgerState'). This
-- was originally for debugging but the check is cheap enough to keep.
applyBlock'
  :: Env
  -> LedgerState
  -> ValidationMode
  ->  HFC.HardForkBlock
            (Consensus.BccEras Consensus.StandardCrypto)
  -> Either Text LedgerState
applyBlock' env oldState validationMode block = do
  let config = envLedgerConfig env
      stateOld = clsState oldState
  stateNew <- case validationMode of
    FullValidation -> tickThenApply config block stateOld
    QuickValidation -> tickThenReapplyCheckHash config block stateOld
  return oldState { clsState = lseState stateNew }

applyBlockWithEvents
  :: Env
  -> LedgerState
  -> Bool
  -- ^ True to validate
  ->  HFC.HardForkBlock
            (Consensus.BccEras Consensus.StandardCrypto)
  -> Either Text LedgerStateEvents
applyBlockWithEvents env oldState enableValidation block = do
  let config = envLedgerConfig env
      stateOld = clsState oldState
  if enableValidation
    then tickThenApply config block stateOld
    else tickThenReapplyCheckHash config block stateOld

-- Like 'Consensus.tickThenReapply' but also checks that the previous hash from
-- the block matches the head hash of the ledger state.
tickThenReapplyCheckHash
    :: HFC.HardForkLedgerConfig
        (Consensus.BccEras Sophie.StandardCrypto)
    -> Consensus.BccBlock Consensus.StandardCrypto
    -> Sophie.LedgerState
        (HFC.HardForkBlock
            (Consensus.BccEras Sophie.StandardCrypto))
    -> Either Text LedgerStateEvents
tickThenReapplyCheckHash cfg block lsb =
  if Consensus.blockPrevHash block == Ledger.ledgerTipHash lsb
    then Right . toLedgerStateEvents
          $ Ledger.tickThenReapplyLedgerResult cfg block lsb
    else Left $ mconcat
                  [ "Ledger state hash mismatch. Ledger head is slot "
                  , textShow
                      $ Slot.unSlotNo
                      $ Slot.fromWithOrigin
                          (Slot.SlotNo 0)
                          (Ledger.ledgerTipSlot lsb)
                  , " hash "
                  , renderByteArray
                      $ unChainHash
                      $ Ledger.ledgerTipHash lsb
                  , " but block previous hash is "
                  , renderByteArray (unChainHash $ Consensus.blockPrevHash block)
                  , " and block current hash is "
                  , renderByteArray
                      $ BSS.fromShort
                      $ HFC.getOneEraHash
                      $ Shardagnostic.Network.Block.blockHash block
                  , "."
                  ]

-- Like 'Consensus.tickThenReapply' but also checks that the previous hash from
-- the block matches the head hash of the ledger state.
tickThenApply
    :: HFC.HardForkLedgerConfig
        (Consensus.BccEras Sophie.StandardCrypto)
    -> Consensus.BccBlock Consensus.StandardCrypto
    -> Sophie.LedgerState
        (HFC.HardForkBlock
            (Consensus.BccEras Sophie.StandardCrypto))
    -> Either Text LedgerStateEvents
tickThenApply cfg block lsb
  = either (Left . Text.pack . show) (Right . toLedgerStateEvents)
  $ runExcept
  $ Ledger.tickThenApplyLedgerResult cfg block lsb

renderByteArray :: ByteArrayAccess bin => bin -> Text
renderByteArray =
  Text.decodeUtf8 . Base16.encode . Data.ByteArray.convert

unChainHash :: Shardagnostic.Network.Block.ChainHash (Consensus.BccBlock era) -> ByteString
unChainHash ch =
  case ch of
    Shardagnostic.Network.Block.GenesisHash -> "genesis"
    Shardagnostic.Network.Block.BlockHash bh -> BSS.fromShort (HFC.getOneEraHash bh)
