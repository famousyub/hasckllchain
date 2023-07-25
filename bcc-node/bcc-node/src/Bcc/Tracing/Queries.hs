{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Bcc.Tracing.Queries
  (LedgerQueries(..))
where

import           Prelude (Int, (.))

import qualified Data.Map.Strict as Map

import           Shardagnostic.Consensus.HardFork.Combinator
import           Shardagnostic.Consensus.HardFork.Combinator.Embed.Unary

import qualified Bcc.Chain.Block as Cole
import qualified Bcc.Chain.UTxO as Cole
import qualified Shardagnostic.Consensus.Cole.Ledger.Block as Cole
import qualified Shardagnostic.Consensus.Cole.Ledger.Ledger as Cole

import qualified Shardagnostic.Consensus.Sophie.Ledger as Sophie
import qualified Sophie.Spec.Ledger.LedgerState as Sophie
import qualified Sophie.Spec.Ledger.UTxO as Sophie

import qualified Shardagnostic.Consensus.Bcc as Bcc
import qualified Shardagnostic.Consensus.Bcc.Block as Bcc


class LedgerQueries blk where
  ledgerUtxoSize     :: LedgerState blk -> Int
  ledgerDelegMapSize :: LedgerState blk -> Int

instance LedgerQueries Cole.ColeBlock where
  ledgerUtxoSize = Map.size . Cole.unUTxO . Cole.cvsUtxo . Cole.coleLedgerState
  ledgerDelegMapSize _ = 0

instance LedgerQueries (Sophie.SophieBlock era) where
  ledgerUtxoSize =
      (\(Sophie.UTxO xs)-> Map.size xs)
    . Sophie._utxo
    . Sophie._utxoState
    . Sophie.esLState
    . Sophie.nesEs
    . Sophie.sophieLedgerState
  ledgerDelegMapSize =
      Map.size
    . Sophie._delegations
    . Sophie._dstate
    . Sophie._delegationState
    . Sophie.esLState
    . Sophie.nesEs
    . Sophie.sophieLedgerState

instance (LedgerQueries x, NoHardForks x)
      => LedgerQueries (HardForkBlock '[x]) where
  ledgerUtxoSize = ledgerUtxoSize . project
  ledgerDelegMapSize = ledgerDelegMapSize . project

instance LedgerQueries (Bcc.BccBlock c) where
  ledgerUtxoSize = \case
    Bcc.LedgerStateCole   ledgerCole   -> ledgerUtxoSize ledgerCole
    Bcc.LedgerStateSophie ledgerSophie -> ledgerUtxoSize ledgerSophie
    Bcc.LedgerStateEvie ledgerEvie -> ledgerUtxoSize ledgerEvie
    Bcc.LedgerStateJen    ledgerJen    -> ledgerUtxoSize ledgerJen
    Bcc.LedgerStateAurum  ledgerAurum  -> ledgerUtxoSize ledgerAurum
  ledgerDelegMapSize = \case
    Bcc.LedgerStateCole   ledgerCole   -> ledgerDelegMapSize ledgerCole
    Bcc.LedgerStateSophie ledgerSophie -> ledgerDelegMapSize ledgerSophie
    Bcc.LedgerStateEvie ledgerEvie -> ledgerDelegMapSize ledgerEvie
    Bcc.LedgerStateJen    ledgerJen    -> ledgerDelegMapSize ledgerJen
    Bcc.LedgerStateAurum  ledgerAurum  -> ledgerDelegMapSize ledgerAurum
