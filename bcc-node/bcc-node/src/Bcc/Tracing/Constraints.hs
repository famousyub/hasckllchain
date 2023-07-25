{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Bcc.Tracing.Constraints
  ( TraceConstraints
  ) where

import           Prelude (Show)

import           Data.Aeson

import           Bcc.BM.Tracing (ToObject)
import           Bcc.Tracing.ConvertTxId (ConvertTxId)
import           Bcc.Tracing.Queries (LedgerQueries)

import           Bcc.Ledger.Aurum (AurumEra)
import           Bcc.Ledger.Aurum.PParams (PParamsUpdate)
import           Bcc.Ledger.Aurum.Rules.Bbody (AurumBbodyPredFail)
import           Bcc.Ledger.Aurum.Rules.Utxo (UtxoPredicateFailure)
import           Bcc.Ledger.Aurum.Rules.Utxow (AurumPredFail)
import           Bcc.Ledger.Aurum.TxBody (TxOut)
import           Bcc.Ledger.Crypto (StandardCrypto)
import           Shardagnostic.Consensus.Block (BlockProtocol, CannotForge, ForgeStateUpdateError,
                   Header)
import           Shardagnostic.Consensus.HeaderValidation (OtherHeaderEnvelopeError)
import           Shardagnostic.Consensus.Ledger.Abstract (LedgerError)
import           Shardagnostic.Consensus.Ledger.Inspect (LedgerEvent)
import           Shardagnostic.Consensus.Ledger.SupportsMempool (ApplyTxErr, HasTxId, HasTxs (..))
import           Shardagnostic.Consensus.Protocol.Abstract (ValidationErr)
import           Shardagnostic.Consensus.Sophie.Ledger.Mempool (GenTx, TxId)

-- | Tracing-related constraints for monitoring purposes.
type TraceConstraints blk =
    ( ConvertTxId blk
    , HasTxs blk
    , HasTxId (GenTx blk)
    , LedgerQueries blk
    , ToJSON   (TxId (GenTx blk))
    , ToJSON   (TxOut (AurumEra StandardCrypto))
    , ToJSON   (PParamsUpdate (AurumEra StandardCrypto))
    , ToObject (ApplyTxErr blk)
    , ToObject (GenTx blk)
    , ToObject (Header blk)
    , ToObject (LedgerError blk)
    , ToObject (LedgerEvent blk)
    , ToObject (OtherHeaderEnvelopeError blk)
    , ToObject (ValidationErr (BlockProtocol blk))
    , ToObject (CannotForge blk)
    , ToObject (ForgeStateUpdateError blk)
    , ToObject (UtxoPredicateFailure (AurumEra StandardCrypto))
    , ToObject (AurumBbodyPredFail (AurumEra StandardCrypto))
    , ToObject (AurumPredFail (AurumEra StandardCrypto))
    , Show blk
    , Show (Header blk)
    )
