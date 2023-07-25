{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Bcc.Tracing.ConvertTxId
  ( ConvertTxId (..)
  ) where

import           Bcc.Prelude hiding (All)

import           Data.SOP.Strict

import qualified Bcc.Crypto.Hash as Crypto
import qualified Bcc.Crypto.Hashing as Cole.Crypto
import qualified Bcc.Ledger.SafeHash as Ledger
import           Shardagnostic.Consensus.Cole.Ledger.Block (ColeBlock)
import           Shardagnostic.Consensus.Cole.Ledger.Mempool (TxId (..))
import           Shardagnostic.Consensus.HardFork.Combinator
import           Shardagnostic.Consensus.Sophie.Ledger.Block (SophieBlock)
import           Shardagnostic.Consensus.Sophie.Ledger.Mempool (TxId (..))
import           Shardagnostic.Consensus.TypeFamilyWrappers
import qualified Sophie.Spec.Ledger.TxBody as Sophie

-- | Convert a transaction ID to raw bytes.
class ConvertTxId blk where
  txIdToRawBytes :: TxId (GenTx blk) -> ByteString

instance ConvertTxId ColeBlock where
  txIdToRawBytes (ColeTxId txId) = Cole.Crypto.abstractHashToBytes txId
  txIdToRawBytes (ColeDlgId dlgId) = Cole.Crypto.abstractHashToBytes dlgId
  txIdToRawBytes (ColeUpdateProposalId upId) =
    Cole.Crypto.abstractHashToBytes upId
  txIdToRawBytes (ColeUpdateVoteId voteId) =
    Cole.Crypto.abstractHashToBytes voteId

instance ConvertTxId (SophieBlock c) where
  txIdToRawBytes (SophieTxId txId) =
    Crypto.hashToBytes . Ledger.extractHash . Sophie._unTxId $ txId

instance All ConvertTxId xs
      => ConvertTxId (HardForkBlock xs) where
  txIdToRawBytes =
    hcollapse
      . hcmap (Proxy @ ConvertTxId) (K . txIdToRawBytes . unwrapGenTxId)
      . getOneEraGenTxId
      . getHardForkGenTxId
