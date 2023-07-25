{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
module Bcc.Tracing.Kernel
  ( NodeKernelData (..)
  , mkNodeKernelData
  , setNodeKernel
  , mapNodeKernelDataIO
  , nkQueryLedger
  , nkQueryChain
  -- * Re-exports
  , NodeKernel (..)
  , LocalConnectionId
  , RemoteConnectionId
  , StrictMaybe(..)
  , fromSMaybe
  ) where

import           Bcc.Prelude

import           Bcc.Ledger.BaseTypes (StrictMaybe (..), fromSMaybe)

import           Data.IORef (IORef, newIORef, readIORef, writeIORef)

import           Shardagnostic.Consensus.Block (Header)
import           Shardagnostic.Consensus.Ledger.Abstract (IsLedger, LedgerState)
import           Shardagnostic.Consensus.Ledger.Extended (ExtLedgerState)
import           Shardagnostic.Consensus.Node (NodeKernel (..))
import qualified Shardagnostic.Consensus.Storage.ChainDB as ChainDB
import           Shardagnostic.Consensus.Util.Orphans ()

import qualified Shardagnostic.Network.AnchoredFragment as AF
import           Shardagnostic.Network.NodeToClient (LocalConnectionId)
import           Shardagnostic.Network.NodeToNode (RemoteConnectionId)


newtype NodeKernelData blk =
  NodeKernelData
  { unNodeKernelData :: IORef (StrictMaybe (NodeKernel IO RemoteConnectionId LocalConnectionId blk))
  }

mkNodeKernelData :: IO (NodeKernelData blk)
mkNodeKernelData = NodeKernelData <$> newIORef SNothing

setNodeKernel :: NodeKernelData blk
              -> NodeKernel IO RemoteConnectionId LocalConnectionId blk
              -> IO ()
setNodeKernel (NodeKernelData ref) nodeKern =
  writeIORef ref $ SJust nodeKern

mapNodeKernelDataIO ::
  (NodeKernel IO RemoteConnectionId LocalConnectionId blk -> IO a)
  -> NodeKernelData blk
  -> IO (StrictMaybe a)
mapNodeKernelDataIO f (NodeKernelData ref) =
  readIORef ref >>= traverse f

nkQueryLedger ::
     IsLedger (LedgerState blk)
  => (ExtLedgerState blk -> a)
  -> NodeKernel IO RemoteConnectionId LocalConnectionId blk
  -> IO a
nkQueryLedger f NodeKernel{getChainDB} =
  f <$> atomically (ChainDB.getCurrentLedger getChainDB)

nkQueryChain ::
     (AF.AnchoredFragment (Header blk) -> a)
  -> NodeKernel IO RemoteConnectionId LocalConnectionId blk
  -> IO a
nkQueryChain f NodeKernel{getChainDB} =
  f <$> atomically (ChainDB.getCurrentChain getChainDB)
