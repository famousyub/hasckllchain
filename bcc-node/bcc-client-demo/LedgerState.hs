{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -freduction-depth=0 #-}

import           Bcc.Api
import           Bcc.Slotting.Slot (WithOrigin (At, Origin))
import           Control.Monad (when)
import           Control.Monad.Trans.Except
import           Data.Foldable
import           Data.IORef
import           Data.Maybe (fromMaybe)
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import           Data.Word
import           Network.TypedProtocol.Pipelined (Nat (..))
import qualified Shardagnostic.Consensus.Sophie.Ledger as Sophie
import           Shardagnostic.Network.Protocol.ChainSync.ClientPipelined
                   (ChainSyncClientPipelined (ChainSyncClientPipelined),
                   ClientPipelinedStIdle (CollectResponse, SendMsgDone, SendMsgRequestNextPipelined),
                   ClientStNext (..))
import           Shardagnostic.Network.Protocol.ChainSync.PipelineDecision
import           System.Environment (getArgs)


main :: IO ()
main = do
  -- Get socket path from CLI argument.
  configFilePath : socketPath : xs <- getArgs
  coleSlotLength <- case xs of
        coleSlotLengthStr : _ -> return (read coleSlotLengthStr)
        _ -> do
          let l = 21600
          putStrLn $ "Using default cole slots per epoch: " <> show l
          return l
  blockCount <- fmap (either (error . T.unpack . renderFoldBlocksError) id) $ runExceptT $ foldBlocks
    configFilePath
    (BccModeParams (EpochSlots coleSlotLength))
    socketPath
    FullValidation
    (0 :: Int) -- We just use a count of the blocks as the current state
    (\_env
      !ledgerState
      (BlockInMode (Block (BlockHeader _slotNo _blockHeaderHash (BlockNo blockNoI)) _transactions) _era)
      blockCount -> do
        case ledgerState of
            LedgerStateSophie (Sophie.SophieLedgerState sophieTipWO _ _) -> case sophieTipWO of
              Origin -> putStrLn "."
              At (Sophie.SophieTip _ _ hash) -> print hash
            _ -> when (blockNoI `mod` 100 == 0) (print blockNoI)
        return (blockCount + 1)
    )

  putStrLn $ "Processed " ++ show blockCount ++ " blocks"
  return ()
