{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -freduction-depth=0 #-}

import           Bcc.Api
import           Bcc.Api.ChainSync.ClientPipelined
import qualified Bcc.Chain.Slotting as Cole (EpochSlots (..))
import           Bcc.Slotting.Slot

import           Control.Monad (when)
import           Data.Kind
import           Data.Proxy
import           Data.Time
import           Data.Word (Word32)
import qualified GHC.TypeLits as GHC
import           System.Environment (getArgs)
import           System.FilePath ((</>))

-- | Connects to a local bcc node, requests the blocks and prints out the
-- number of transactions. To run this, you must first start a local node e.g.:
--
--     $ cabal run bcc-node:bcc-node -- run \
--        --config        configuration/bcc/mainnet-config.json \
--        --topology      configuration/bcc/mainnet-topology.json \
--        --database-path db \
--        --socket-path   db/node.sock \
--        --host-addr     127.0.0.1 \
--        --port          3001 \
--
-- Then run this with the path to the directory containing node.sock:
--
--     $ cabal run bcc-client-demo:scan-blocks-pipelined -- db
--
main :: IO ()
main = do
  -- Get cocket path from CLI argument.
  socketDir:_ <- getArgs
  let socketPath = socketDir </> "node.sock"

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

  protocols :: LocalNodeClientProtocolsInMode BccMode
  protocols =
      LocalNodeClientProtocols {
        localChainSyncClient    = LocalChainSyncClientPipelined (chainSyncClient 50),
        localTxSubmissionClient = Nothing,
        localStateQueryClient   = Nothing
      }


-- | Defines the pipelined client side of the chain sync protocol.
chainSyncClient
  :: Word32
  -- ^ The maximum number of concurrent requests.
  -> ChainSyncClientPipelined
        (BlockInMode BccMode)
        ChainPoint
        ChainTip
        IO
        ()
chainSyncClient pipelineSize = ChainSyncClientPipelined $ do
  startTime <- getCurrentTime
  let
    clientIdle_RequestMoreN :: WithOrigin BlockNo -> WithOrigin BlockNo
                            -> Nat n -> ClientPipelinedStIdle n (BlockInMode BccMode)
                                 ChainPoint ChainTip IO ()
    clientIdle_RequestMoreN clientTip serverTip n = case pipelineDecisionMax pipelineSize n clientTip serverTip  of
      Collect -> case n of
        Succ predN -> CollectResponse Nothing (clientNextN predN)
      _ -> SendMsgRequestNextPipelined (clientIdle_RequestMoreN clientTip serverTip (Succ n))

    clientNextN :: Nat n -> ClientStNext n (BlockInMode BccMode)
                                 ChainPoint ChainTip IO ()
    clientNextN n =
      ClientStNext {
          recvMsgRollForward = \(BlockInMode block@(Block (BlockHeader _ _ currBlockNo@(BlockNo blockNo)) _) _) serverChainTip -> do
            let newClientTip = At currBlockNo
                newServerTip = fromChainTip serverChainTip
            when (blockNo `mod` 1000 == 0) $ do
              printBlock block
              now <- getCurrentTime
              let elapsedTime = realToFrac (now `diffUTCTime` startTime) :: Double
                  rate = fromIntegral blockNo / elapsedTime
              putStrLn $ "Rate = " ++ show rate ++ " blocks/second"
            if newClientTip == newServerTip
              then  clientIdle_DoneN n
              else return (clientIdle_RequestMoreN newClientTip newServerTip n)
        , recvMsgRollBackward = \_ serverChainTip -> do
            putStrLn "Rollback"
            let newClientTip = Origin -- We don't actually keep track of blocks so we temporarily "forget" the tip.
                newServerTip = fromChainTip serverChainTip
            return (clientIdle_RequestMoreN newClientTip newServerTip n)
        }

    clientIdle_DoneN :: Nat n -> IO (ClientPipelinedStIdle n (BlockInMode BccMode)
                                 ChainPoint ChainTip IO ())
    clientIdle_DoneN n = case n of
      Succ predN -> do
        putStrLn "Chain Sync: done! (Ignoring remaining responses)"
        return $ CollectResponse Nothing (clientNext_DoneN predN) -- Ignore remaining message responses
      Zero -> do
        putStrLn "Chain Sync: done!"
        return $ SendMsgDone ()

    clientNext_DoneN :: Nat n -> ClientStNext n (BlockInMode BccMode)
                                 ChainPoint ChainTip IO ()
    clientNext_DoneN n =
      ClientStNext {
          recvMsgRollForward = \_ _ -> clientIdle_DoneN n
        , recvMsgRollBackward = \_ _ -> clientIdle_DoneN n
        }

    printBlock :: Block era -> IO ()
    printBlock (Block (BlockHeader _ _ currBlockNo) transactions)
      = putStrLn $ show currBlockNo ++ " transactions: " ++ show (length transactions)

    fromChainTip :: ChainTip -> WithOrigin BlockNo
    fromChainTip ct = case ct of
      ChainTipAtGenesis -> Origin
      ChainTip _ _ bno -> At bno

  return (clientIdle_RequestMoreN Origin Origin Zero)
