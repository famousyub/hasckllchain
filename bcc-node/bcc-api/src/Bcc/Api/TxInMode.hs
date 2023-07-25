{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Transactions in the context of a consensus mode, and other types used in
-- the transaction submission protocol.
--
module Bcc.Api.TxInMode (

    -- * Transaction in a consensus mode
    TxInMode(..),
    toConsensusGenTx,

    -- * Transaction validation errors
    TxValidationError(..),
    TxValidationErrorInMode(..),
    fromConsensusApplyTxErr,
  ) where

import           Prelude

import           Data.SOP.Strict (NS (S, Z))

import qualified Shardagnostic.Consensus.Cole.Ledger as Consensus
import qualified Shardagnostic.Consensus.Bcc.Block as Consensus
import qualified Shardagnostic.Consensus.HardFork.Combinator as Consensus
import           Shardagnostic.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)
import qualified Shardagnostic.Consensus.HardFork.Combinator.Degenerate as Consensus
import qualified Shardagnostic.Consensus.Ledger.SupportsMempool as Consensus
import qualified Shardagnostic.Consensus.Sophie.Ledger as Consensus

import           Bcc.Api.Eras
import           Bcc.Api.Modes
import           Bcc.Api.Tx


-- ----------------------------------------------------------------------------
-- Transactions in the context of a consensus mode
--

-- | A 'Tx' in one of the eras supported by a given protocol mode.
--
-- For multi-era modes such as the 'BccMode' this type is a sum of the
-- different transaction types for all the eras. It is used in the
-- LocalTxSubmission protocol.
--
data TxInMode mode where

     -- | Everything we consider a normal transaction.
     --
     TxInMode :: Tx era -> EraInMode era mode -> TxInMode mode

     -- | Cole has various things we can post to the chain which are not
     -- actually transactions. This covers: update proposals, votes and
     -- delegation certs.
     --
     TxInColeSpecial :: Consensus.GenTx Consensus.ColeBlock
                      -> EraInMode ColeEra mode -> TxInMode mode

deriving instance Show (TxInMode mode)


toConsensusGenTx :: ConsensusBlockForMode mode ~ block
                 => TxInMode mode
                 -> Consensus.GenTx block
toConsensusGenTx (TxInMode (ColeTx tx) ColeEraInColeMode) =
    Consensus.HardForkGenTx (Consensus.OneEraGenTx (Z tx'))
  where
    tx' = Consensus.ColeTx (Consensus.coleIdTx tx) tx

toConsensusGenTx (TxInMode (ColeTx tx) ColeEraInBccMode) =
    Consensus.HardForkGenTx (Consensus.OneEraGenTx (Z tx'))
  where
    tx' = Consensus.ColeTx (Consensus.coleIdTx tx) tx
    --TODO: add the above as mkColeTx to the consensus code,
    -- matching mkSophieTx below

toConsensusGenTx (TxInColeSpecial gtx ColeEraInColeMode) =
    Consensus.HardForkGenTx (Consensus.OneEraGenTx (Z gtx))

toConsensusGenTx (TxInColeSpecial gtx ColeEraInBccMode) =
    Consensus.HardForkGenTx (Consensus.OneEraGenTx (Z gtx))

toConsensusGenTx (TxInMode (SophieTx _ tx) SophieEraInSophieMode) =
    Consensus.HardForkGenTx (Consensus.OneEraGenTx (Z tx'))
  where
    tx' = Consensus.mkSophieTx tx

toConsensusGenTx (TxInMode (SophieTx _ tx) SophieEraInBccMode) =
    Consensus.HardForkGenTx (Consensus.OneEraGenTx (S (Z tx')))
  where
    tx' = Consensus.mkSophieTx tx

toConsensusGenTx (TxInMode (SophieTx _ tx) EvieEraInBccMode) =
    Consensus.HardForkGenTx (Consensus.OneEraGenTx (S (S (Z tx'))))
  where
    tx' = Consensus.mkSophieTx tx

toConsensusGenTx (TxInMode (SophieTx _ tx) JenEraInBccMode) =
    Consensus.HardForkGenTx (Consensus.OneEraGenTx (S (S (S (Z tx')))))
  where
    tx' = Consensus.mkSophieTx tx

toConsensusGenTx (TxInMode (SophieTx _ tx) AurumEraInBccMode) =
    Consensus.HardForkGenTx (Consensus.OneEraGenTx (S (S (S (S (Z tx'))))))
  where
    tx' = Consensus.mkSophieTx tx



-- ----------------------------------------------------------------------------
-- Transaction validation errors in the context of eras and consensus modes
--

-- | The transaction validations errors that can occur from trying to submit a
-- transaction to a local node. The errors are specific to an era.
--
data TxValidationError era where

     ColeTxValidationError
       :: Consensus.ApplyTxErr Consensus.ColeBlock
       -> TxValidationError ColeEra

     SophieTxValidationError
       :: SophieBasedEra era
       -> Consensus.ApplyTxErr (Consensus.SophieBlock (SophieLedgerEra era))
       -> TxValidationError era

-- The GADT in the SophieTxValidationError case requires a custom instance
instance Show (TxValidationError era) where
    showsPrec p (ColeTxValidationError err) =
      showParen (p >= 11)
        ( showString "ColeTxValidationError "
        . showsPrec 11 err
        )

    showsPrec p (SophieTxValidationError SophieBasedEraSophie err) =
      showParen (p >= 11)
        ( showString "SophieTxValidationError SophieBasedEraSophie "
        . showsPrec 11 err
        )

    showsPrec p (SophieTxValidationError SophieBasedEraEvie err) =
      showParen (p >= 11)
        ( showString "SophieTxValidationError SophieBasedEraEvie "
        . showsPrec 11 err
        )

    showsPrec p (SophieTxValidationError SophieBasedEraJen err) =
      showParen (p >= 11)
        ( showString "SophieTxValidationError SophieBasedEraJen "
        . showsPrec 11 err
        )

    showsPrec p (SophieTxValidationError SophieBasedEraAurum err) =
      showParen (p >= 11)
        ( showString "SophieTxValidationError SophieBasedEraAurum "
        . showsPrec 11 err
        )


-- | A 'TxValidationError' in one of the eras supported by a given protocol
-- mode.
--
-- This is used in the LocalStateQuery protocol.
--
data TxValidationErrorInMode mode where
     TxValidationErrorInMode :: TxValidationError era
                             -> EraInMode era mode
                             -> TxValidationErrorInMode mode

     TxValidationEraMismatch :: EraMismatch
                             -> TxValidationErrorInMode mode

deriving instance Show (TxValidationErrorInMode mode)


fromConsensusApplyTxErr :: ConsensusBlockForMode mode ~ block
                        => ConsensusMode mode
                        -> Consensus.ApplyTxErr block
                        -> TxValidationErrorInMode mode
fromConsensusApplyTxErr ColeMode (Consensus.DegenApplyTxErr err) =
    TxValidationErrorInMode
      (ColeTxValidationError err)
      ColeEraInColeMode

fromConsensusApplyTxErr SophieMode (Consensus.DegenApplyTxErr err) =
    TxValidationErrorInMode
      (SophieTxValidationError SophieBasedEraSophie err)
      SophieEraInSophieMode

fromConsensusApplyTxErr BccMode (Consensus.ApplyTxErrCole err) =
    TxValidationErrorInMode
      (ColeTxValidationError err)
      ColeEraInBccMode

fromConsensusApplyTxErr BccMode (Consensus.ApplyTxErrSophie err) =
    TxValidationErrorInMode
      (SophieTxValidationError SophieBasedEraSophie err)
      SophieEraInBccMode

fromConsensusApplyTxErr BccMode (Consensus.ApplyTxErrEvie err) =
    TxValidationErrorInMode
      (SophieTxValidationError SophieBasedEraEvie err)
      EvieEraInBccMode

fromConsensusApplyTxErr BccMode (Consensus.ApplyTxErrJen err) =
    TxValidationErrorInMode
      (SophieTxValidationError SophieBasedEraJen err)
      JenEraInBccMode

fromConsensusApplyTxErr BccMode (Consensus.ApplyTxErrAurum err) =
    TxValidationErrorInMode
      (SophieTxValidationError SophieBasedEraAurum err)
      AurumEraInBccMode

fromConsensusApplyTxErr BccMode (Consensus.ApplyTxErrWrongEra err) =
    TxValidationEraMismatch err

