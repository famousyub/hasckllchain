{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

import           Bcc.Api
import           Bcc.Api.ChainSync.Client
import qualified Bcc.Chain.Slotting as Cole (EpochSlots (..))
import           Bcc.Slotting.Slot
import qualified Shardagnostic.Consensus.Bcc.Block as C
import qualified Shardagnostic.Consensus.HardFork.Combinator.Basics as C
import qualified Shardagnostic.Consensus.HardFork.Combinator.State.Types as C
import qualified Shardagnostic.Consensus.HardFork.Combinator.Util.Telescope as TSP

import           Control.Monad (when)
import           Control.Monad.Trans.Except
import           Data.Kind
import           Data.Proxy
import qualified Data.SOP as SOP
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           Data.Word (Word32)
import qualified GHC.TypeLits as GHC
import           System.Environment (getArgs)
import           System.FilePath ((</>))

-- | Connects to a local bcc node, requests the blocks and prints out some
-- information from each ledger state. To run this, you must first start a local
-- node e.g.:
--
--     $ cabal run bcc-node:bcc-node -- run \
--        --config        configuration/bcc/mainnet-config.json \
--        --topology      configuration/bcc/mainnet-topology.json \
--        --database-path db \
--        --socket-path   db/node.sock \
--        --host-addr     127.0.0.1 \
--        --port          3001 \
--
-- Then run this with the path to the config file and the node.sock:
--
--     $ cabal run bcc-client-demo:chain-sync-client-with-ledger-state -- \
--          configuration/bcc/mainnet-config.json \
--          db/node.sock
--
main :: IO ()
main = do
  -- Get config and socket path from CLI argument.
  configFilePath : socketPath : xs <- getArgs
  coleSlotLength <- case xs of
        coleSlotLengthStr : _ -> return (read coleSlotLengthStr)
        _ -> do
          let l = 21600
          putStrLn $ "Using default cole slots per epoch: " <> show l
          return l

  -- Use 'chainSyncClientWithLedgerState' to support ledger state.
  Right (env, initialLedgerState) <- runExceptT $ initialLedgerState configFilePath
  let client = chainSyncClientWithLedgerState
        env
        initialLedgerState
        FullValidation
        chainSyncClient

      protocols :: LocalNodeClientProtocolsInMode BccMode
      protocols =
          LocalNodeClientProtocols {
            localChainSyncClient    = LocalChainSyncClient client,
            localTxSubmissionClient = Nothing,
            localStateQueryClient   = Nothing
          }

  -- Connect to the node.
  putStrLn $ "Connecting to socket: " <> socketPath
  connectToLocalNode
    (connectInfo socketPath)
    protocols
  where
  connectInfo :: FilePath -> LocalNodeConnectInfo BccMode
  connectInfo socketPath =
      LocalNodeConnectInfo {
        localConsensusModeParams = BccModeParams (Cole.EpochSlots 21600),
        localNodeNetworkId       = Mainnet,
        localNodeSocketPath      = socketPath
      }

-- | Defines the client side of the chain sync protocol.
chainSyncClient
  :: ChainSyncClient
        (BlockInMode BccMode, Either Text LedgerState)
        ChainPoint
        ChainTip
        IO
        ()
chainSyncClient = ChainSyncClient $ do
  startTime <- getCurrentTime
  let
    clientStIdle :: ClientStIdle (BlockInMode BccMode, Either Text LedgerState)
                                 ChainPoint ChainTip IO ()
    clientStIdle = SendMsgRequestNext
        clientStNext
        (pure clientStNext)

    clientStNext :: ClientStNext (BlockInMode BccMode, Either Text LedgerState)
                                 ChainPoint ChainTip IO ()
    clientStNext =
      ClientStNext {
          recvMsgRollForward =
            \( BlockInMode block@(Block (BlockHeader _ _ (BlockNo blockNo)) _) _
             , ledgerStateE
             )
             _tip ->
              ChainSyncClient $ case ledgerStateE of
                Left err -> do
                  putStrLn $ "Ledger state error: " <> T.unpack err
                  return (SendMsgDone ())
                Right (LedgerState (C.HardForkLedgerState (C.HardForkState ledgerState))) -> do
                  when (blockNo `mod` 1000 == 0) $ do
                    printLedgerState ledgerState
                    now <- getCurrentTime
                    let elapsedTime = realToFrac (now `diffUTCTime` startTime) :: Float
                        rate = fromIntegral blockNo / elapsedTime
                    putStrLn $ "Rate = " ++ show rate ++ " blocks/second"
                  return clientStIdle
        , recvMsgRollBackward = \_ _ -> ChainSyncClient $ do
            putStrLn "Rollback!"
            return clientStIdle
        }

    printLedgerState :: TSP.Telescope (SOP.K C.Past) (C.Current C.LedgerState) xs -> IO ()
    printLedgerState ls = case ls of
      TSP.TZ (C.Current bound _) -> putStrLn $ "curren't era bounds: " <> show bound
      TSP.TS _ ls' -> printLedgerState ls'
  return clientStIdle

