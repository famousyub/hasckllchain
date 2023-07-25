{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Bcc.Tracing.OrphanInstances.Sophie () where

import           Bcc.Prelude

import           Data.Aeson (Value (..), object)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.HashMap.Strict as HMS
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Bcc.Api as Api
import           Bcc.Api.Orphans ()
import qualified Bcc.Api.Sophie as Api
import           Bcc.Ledger.Crypto (StandardCrypto)

import           Bcc.Slotting.Block (BlockNo (..))
import           Bcc.Tracing.OrphanInstances.Common
import           Bcc.Tracing.OrphanInstances.Consensus ()

import           Shardagnostic.Consensus.Ledger.SupportsMempool (txId)
import qualified Shardagnostic.Consensus.Ledger.SupportsMempool as SupportsMempool
import           Shardagnostic.Consensus.Util.Condense (condense)
import           Shardagnostic.Network.Block (SlotNo (..), blockHash, blockNo, blockSlot)
import           Shardagnostic.Network.Point (WithOrigin, withOriginToMaybe)

import           Shardagnostic.Consensus.Sophie.Ledger hiding (TxId)
import           Shardagnostic.Consensus.Sophie.Ledger.Inspect
import           Shardagnostic.Consensus.Sophie.Protocol (TOptimumCannotForge (..))
import qualified Shardagnostic.Consensus.Sophie.Protocol.HotKey as HotKey

import qualified Bcc.Crypto.Hash.Class as Crypto
import           Bcc.Ledger.Aurum as Aurum
import qualified Bcc.Ledger.Aurum.ZerepochScriptApi as Aurum
import           Bcc.Ledger.Aurum.Rules.Bbody (AurumBbodyPredFail)
import qualified Bcc.Ledger.Aurum.Rules.Utxo as Aurum
import qualified Bcc.Ledger.Aurum.Rules.Utxos as Aurum
import           Bcc.Ledger.Aurum.Rules.Utxow (AurumPredFail (..))
import qualified Bcc.Ledger.Aurum.Tx as Aurum
import qualified Bcc.Ledger.Aurum.TxInfo as Aurum
import qualified Bcc.Ledger.AuxiliaryData as Core
import           Bcc.Ledger.BaseTypes (strictMaybeToMaybe)
import qualified Bcc.Ledger.Core as Core
import qualified Bcc.Ledger.Core as Ledger
import qualified Bcc.Ledger.Crypto as Core
import qualified Bcc.Ledger.Era as Ledger
import qualified Bcc.Ledger.SafeHash as SafeHash
import qualified Bcc.Ledger.SophieMA.Rules.Utxo as MA
import qualified Bcc.Ledger.SophieMA.Timelocks as MA
import           Bcc.Protocol.TOptimum.BHeader (LastAppliedBlock, labBlockNo)
import           Bcc.Protocol.TOptimum.Rules.OCert
import           Bcc.Protocol.TOptimum.Rules.Overlay
import           Bcc.Protocol.TOptimum.Rules.Updn

-- TODO: this should be exposed via Bcc.Api
import           Sophie.Spec.Ledger.API hiding (SophieBasedEra)

import           Sophie.Spec.Ledger.STS.Bbody
import           Sophie.Spec.Ledger.STS.Chain
import           Sophie.Spec.Ledger.STS.Deleg
import           Sophie.Spec.Ledger.STS.Delegs
import           Sophie.Spec.Ledger.STS.Delpl
import           Sophie.Spec.Ledger.STS.Epoch
import           Sophie.Spec.Ledger.STS.Ledger
import           Sophie.Spec.Ledger.STS.Ledgers
import           Sophie.Spec.Ledger.STS.Mir
import           Sophie.Spec.Ledger.STS.NewEpoch
import           Sophie.Spec.Ledger.STS.Newpp
import           Sophie.Spec.Ledger.STS.Pool
import           Sophie.Spec.Ledger.STS.PoolReap
import           Sophie.Spec.Ledger.STS.Ppup
import           Sophie.Spec.Ledger.STS.Rupd
import           Sophie.Spec.Ledger.STS.Snap
import           Sophie.Spec.Ledger.STS.Tick
import           Sophie.Spec.Ledger.STS.Upec
import           Sophie.Spec.Ledger.STS.Utxo
import           Sophie.Spec.Ledger.STS.Utxow

{- HLINT ignore "Use :" -}

--
-- | instances of @ToObject@
--
-- NOTE: this list is sorted in roughly topological order.

instance SophieBasedEra era => ToObject (GenTx (SophieBlock era)) where
  toObject verb tx =
    mkObject $
        [ "txid" .= txId tx ]
     ++ [ "tx"   .= condense tx | verb == MaximalVerbosity ]

instance ToJSON (SupportsMempool.TxId (GenTx (SophieBlock era))) where
  toJSON i = toJSON (condense i)

instance SophieBasedEra era => ToObject (Header (SophieBlock era)) where
  toObject _verb b = mkObject
        [ "kind" .= String "SophieBlock"
        , "hash" .= condense (blockHash b)
        , "slotNo" .= condense (blockSlot b)
        , "blockNo" .= condense (blockNo b)
--      , "delegate" .= condense (headerSignerVk h)
        ]

instance ( SophieBasedEra era
         , ToObject (PredicateFailure (UTXO era))
         , ToObject (PredicateFailure (UTXOW era))
         , ToObject (PredicateFailure (Core.EraRule "LEDGER" era))
         ) => ToObject (ApplyTxError era) where
  toObject verb (ApplyTxError predicateFailures) =
    HMS.unions $ map (toObject verb) predicateFailures

instance Core.Crypto crypto => ToObject (TOptimumCannotForge crypto) where
  toObject _verb (TOptimumCannotForgeKeyNotUsableYet wallClockPeriod keyStartPeriod) =
    mkObject
      [ "kind" .= String "TOptimumCannotForgeKeyNotUsableYet"
      , "keyStart" .= keyStartPeriod
      , "wallClock" .= wallClockPeriod
      ]
  toObject _verb (TOptimumCannotForgeWrongVRF genDlgVRFHash coreNodeVRFHash) =
    mkObject
      [ "kind" .= String "TOptimumCannotLeadWrongVRF"
      , "expected" .= genDlgVRFHash
      , "actual" .= coreNodeVRFHash
      ]

deriving newtype instance ToJSON KESPeriod

instance ToObject HotKey.KESInfo where
  toObject _verb HotKey.KESInfo { kesStartPeriod, kesEndPeriod, kesEvolution } =
    mkObject
      [ "kind" .= String "KESInfo"
      , "startPeriod" .= kesStartPeriod
      , "endPeriod" .= kesEndPeriod
      , "evolution" .= kesEvolution
      ]

instance ToObject HotKey.KESEvolutionError where
  toObject verb (HotKey.KESCouldNotEvolve kesInfo targetPeriod) =
    mkObject
      [ "kind" .= String "KESCouldNotEvolve"
      , "kesInfo" .= toObject verb kesInfo
      , "targetPeriod" .= targetPeriod
      ]
  toObject verb (HotKey.KESKeyAlreadyPoisoned kesInfo targetPeriod) =
    mkObject
      [ "kind" .= String "KESKeyAlreadyPoisoned"
      , "kesInfo" .= toObject verb kesInfo
      , "targetPeriod" .= targetPeriod
      ]

instance ( SophieBasedEra era
         , ToObject (PredicateFailure (UTXO era))
         , ToObject (PredicateFailure (UTXOW era))
         , ToObject (PredicateFailure (Core.EraRule "BBODY" era))
         ) => ToObject (SophieLedgerError era) where
  toObject verb (BBodyError (BlockTransitionError fs)) =
    mkObject [ "kind" .= String "BBodyError"
             , "failures" .= map (toObject verb) fs
             ]

instance ( SophieBasedEra era
         , ToJSON (Ledger.PParamsDelta era)
         ) => ToObject (SophieLedgerUpdate era) where
  toObject verb (SophieUpdatedProtocolUpdates updates) =
    mkObject [ "kind" .= String "SophieUpdatedProtocolUpdates"
             , "updates" .= map (toObject verb) updates
             ]

instance (Ledger.Era era, ToJSON (Ledger.PParamsDelta era))
         => ToObject (ProtocolUpdate era) where
  toObject verb ProtocolUpdate{protocolUpdateProposal, protocolUpdateState} =
    mkObject [ "proposal" .= toObject verb protocolUpdateProposal
             , "state"    .= toObject verb protocolUpdateState
             ]

instance ToJSON (Ledger.PParamsDelta era)
         => ToObject (UpdateProposal era) where
  toObject _verb UpdateProposal{proposalParams, proposalVersion, proposalEpoch} =
    mkObject [ "params"  .= proposalParams
             , "version" .= proposalVersion
             , "epoch"   .= proposalEpoch
             ]

instance Core.Crypto crypto => ToObject (UpdateState crypto) where
  toObject _verb UpdateState{proposalVotes, proposalReachedQuorum} =
    mkObject [ "proposal"      .= proposalVotes
             , "reachedQuorum" .= proposalReachedQuorum
             ]

instance Core.Crypto crypto => ToObject (ChainTransitionError crypto) where
  toObject verb (ChainTransitionError fs) =
    mkObject [ "kind" .= String "ChainTransitionError"
             , "failures" .= map (toObject verb) fs
             ]

instance ( SophieBasedEra era
         , ToObject (PredicateFailure (Core.EraRule "UTXOW" era))
         , ToObject (PredicateFailure (Core.EraRule "BBODY" era))
         , ToObject (PredicateFailure (Core.EraRule "TICK" era))
         , ToObject (PredicateFailure (Core.EraRule "TICKN" era))
         ) => ToObject (ChainPredicateFailure era) where
  toObject _verb (HeaderSizeTooLargeCHAIN hdrSz maxHdrSz) =
    mkObject [ "kind" .= String "HeaderSizeTooLarge"
             , "headerSize" .= hdrSz
             , "maxHeaderSize" .= maxHdrSz
             ]
  toObject _verb (BlockSizeTooLargeCHAIN blkSz maxBlkSz) =
    mkObject [ "kind" .= String "BlockSizeTooLarge"
             , "blockSize" .= blkSz
             , "maxBlockSize" .= maxBlkSz
             ]
  toObject _verb (ObsoleteNodeCHAIN currentPtcl supportedPtcl) =
    mkObject [ "kind" .= String "ObsoleteNode"
             , "explanation" .= String explanation
             , "currentProtocol" .= currentPtcl
             , "supportedProtocol" .= supportedPtcl ]
      where
        explanation = "A scheduled major protocol version change (hard fork) \
                      \has taken place on the chain, but this node does not \
                      \understand the new major protocol version. This node \
                      \must be upgraded before it can continue with the new \
                      \protocol version."
  toObject verb (BbodyFailure f) = toObject verb f
  toObject verb (TickFailure  f) = toObject verb f
  toObject verb (TicknFailure  f) = toObject verb f
  toObject verb (PrtclFailure f) = toObject verb f
  toObject verb (PrtclSeqFailure f) = toObject verb f

instance ToObject (PrtlSeqFailure crypto) where
  toObject _verb (WrongSlotIntervalPrtclSeq (SlotNo lastSlot) (SlotNo currSlot)) =
    mkObject [ "kind" .= String "WrongSlotInterval"
             , "lastSlot" .= lastSlot
             , "currentSlot" .= currSlot
             ]
  toObject _verb (WrongBlockNoPrtclSeq lab currentBlockNo) =
    mkObject [ "kind" .= String "WrongBlockNo"
             , "lastAppliedBlockNo" .= showLastAppBlockNo lab
             , "currentBlockNo" .= (String . textShow $ unBlockNo currentBlockNo)
             ]
  toObject _verb (WrongBlockSequencePrtclSeq lastAppliedHash currentHash) =
    mkObject [ "kind" .= String "WrongBlockSequence"
             , "lastAppliedBlockHash" .= String (textShow lastAppliedHash)
             , "currentBlockHash" .= String (textShow currentHash)
             ]

instance ( SophieBasedEra era
         , ToObject (PredicateFailure (UTXO era))
         , ToObject (PredicateFailure (UTXOW era))
         , ToObject (PredicateFailure (Core.EraRule "LEDGER" era))
         , ToObject (PredicateFailure (Core.EraRule "LEDGERS" era))
         ) => ToObject (BbodyPredicateFailure era) where
  toObject _verb (WrongBlockBodySizeBBODY actualBodySz claimedBodySz) =
    mkObject [ "kind" .= String "WrongBlockBodySizeBBODY"
             , "actualBlockBodySize" .= actualBodySz
             , "claimedBlockBodySize" .= claimedBodySz
             ]
  toObject _verb (InvalidBodyHashBBODY actualHash claimedHash) =
    mkObject [ "kind" .= String "InvalidBodyHashBBODY"
             , "actualBodyHash" .= textShow actualHash
             , "claimedBodyHash" .= textShow claimedHash
             ]
  toObject verb (LedgersFailure f) = toObject verb f


instance ( SophieBasedEra era
         , ToObject (PredicateFailure (UTXO era))
         , ToObject (PredicateFailure (UTXOW era))
         , ToObject (PredicateFailure (Core.EraRule "LEDGER" era))
         ) => ToObject (LedgersPredicateFailure era) where
  toObject verb (LedgerFailure f) = toObject verb f


instance ( SophieBasedEra era
         , ToObject (PredicateFailure (UTXO era))
         , ToObject (PredicateFailure (UTXOW era))
         , ToObject (PredicateFailure (Core.EraRule "DELEGS" era))
         , ToObject (PredicateFailure (Core.EraRule "UTXOW" era))
         ) => ToObject (LedgerPredicateFailure era) where
  toObject verb (UtxowFailure f) = toObject verb f
  toObject verb (DelegsFailure f) = toObject verb f

instance ToObject (AurumPredFail (Aurum.AurumEra StandardCrypto)) where
  toObject v (WrappedSophieEraFailure utxoPredFail) =
    toObject v utxoPredFail
  toObject _ (MissingRedeemers scripts) =
    mkObject [ "kind" .= String "MissingRedeemers"
             , "scripts" .= renderMissingRedeemers scripts
             ]
  toObject _ (MissingRequiredDatums required received) =
    mkObject [ "kind" .= String "MissingRequiredDatums"
             , "required" .= map (Crypto.hashToTextAsHex . SafeHash.extractHash)
                                 (Set.toList required)
             , "received" .= map (Crypto.hashToTextAsHex . SafeHash.extractHash)
                                 (Set.toList received)
             ]
  toObject _ (PPViewHashesDontMatch ppHashInTxBody ppHashFromPParams) =
    mkObject [ "kind" .= String "PPViewHashesDontMatch"
             , "fromTxBody" .= renderScriptIntegrityHash (strictMaybeToMaybe ppHashInTxBody)
             , "fromPParams" .= renderScriptIntegrityHash (strictMaybeToMaybe ppHashFromPParams)
             ]
  toObject _ (MissingRequiredSigners missingKeyWitnesses) =
    mkObject [ "kind" .= String "MissingRequiredSigners"
             , "witnesses" .= Set.toList missingKeyWitnesses
             ]
  toObject _ (UnspendableUTxONoDatumHash txins) =
    mkObject [ "kind" .= String "MissingRequiredSigners"
             , "txins" .= Set.toList txins
             ]
  toObject _ (NonOutputSupplimentaryDatums disallowed acceptable) =
    mkObject [ "kind" .= String "NonOutputSupplimentaryDatums"
             , "disallowed" .= Set.toList disallowed
             , "acceptable" .= Set.toList acceptable
             ]
  toObject _ (ExtraRedeemers rdmrs) =
    mkObject [ "kind" .= String "ExtraRedeemers"
             , "rdmrs" .= map (Api.renderScriptWitnessIndex . Api.fromAurumRdmrPtr) rdmrs
             ]

renderScriptIntegrityHash :: Maybe (Aurum.ScriptIntegrityHash StandardCrypto) -> Aeson.Value
renderScriptIntegrityHash (Just witPPDataHash) =
  Aeson.String . Crypto.hashToTextAsHex $ SafeHash.extractHash witPPDataHash
renderScriptIntegrityHash Nothing = Aeson.Null

renderScriptHash :: ScriptHash StandardCrypto -> Text
renderScriptHash = Api.serialiseToRawBytesHexText . Api.fromSophieScriptHash

renderMissingRedeemers :: [(Aurum.ScriptPurpose StandardCrypto, ScriptHash StandardCrypto)] -> Aeson.Value
renderMissingRedeemers scripts = Aeson.object $ map renderTuple  scripts
 where
  renderTuple :: (Aurum.ScriptPurpose StandardCrypto, ScriptHash StandardCrypto) -> Aeson.Pair
  renderTuple (scriptPurpose, sHash) =  renderScriptHash sHash .= renderScriptPurpose scriptPurpose

renderScriptPurpose :: Aurum.ScriptPurpose StandardCrypto -> Aeson.Value
renderScriptPurpose (Aurum.Minting pid) =
  Aeson.object [ "minting" .= toJSON pid]
renderScriptPurpose (Aurum.Spending txin) =
  Aeson.object [ "spending" .= Api.fromSophieTxIn txin]
renderScriptPurpose (Aurum.Rewarding rwdAcct) =
  Aeson.object [ "rewarding" .= Aeson.String (Api.serialiseAddress $ Api.fromSophieStakeAddr rwdAcct)]
renderScriptPurpose (Aurum.Certifying cert) =
  Aeson.object [ "certifying" .= toJSON (Api.textEnvelopeDefaultDescr $ Api.fromSophieCertificate cert)]

instance ( SophieBasedEra era
         , ToObject (PredicateFailure (UTXO era))
         , ToObject (PredicateFailure (Core.EraRule "UTXO" era))
         ) => ToObject (UtxowPredicateFailure era) where
  toObject _verb (InvalidWitnessesUTXOW wits') =
    mkObject [ "kind" .= String "InvalidWitnessesUTXOW"
             , "invalidWitnesses" .= map textShow wits'
             ]
  toObject _verb (MissingVKeyWitnessesUTXOW (WitHashes wits')) =
    mkObject [ "kind" .= String "MissingVKeyWitnessesUTXOW"
             , "missingWitnesses" .= wits'
             ]
  toObject _verb (MissingScriptWitnessesUTXOW missingScripts) =
    mkObject [ "kind" .= String "MissingScriptWitnessesUTXOW"
             , "missingScripts" .= missingScripts
             ]
  toObject _verb (ScriptWitnessNotValidatingUTXOW failedScripts) =
    mkObject [ "kind" .= String "ScriptWitnessNotValidatingUTXOW"
             , "failedScripts" .= failedScripts
             ]
  toObject verb (UtxoFailure f) = toObject verb f
  toObject _verb (MIRInsufficientGenesisSigsUTXOW genesisSigs) =
    mkObject [ "kind" .= String "MIRInsufficientGenesisSigsUTXOW"
             , "genesisSigs" .= genesisSigs
             ]
  toObject _verb (MissingTxBodyMetadataHash metadataHash) =
    mkObject [ "kind" .= String "MissingTxBodyMetadataHash"
             , "metadataHash" .= metadataHash
             ]
  toObject _verb (MissingTxMetadata txBodyMetadataHash) =
    mkObject [ "kind" .= String "MissingTxMetadata"
             , "txBodyMetadataHash" .= txBodyMetadataHash
             ]
  toObject _verb (ConflictingMetadataHash txBodyMetadataHash fullMetadataHash) =
    mkObject [ "kind" .= String "ConflictingMetadataHash"
             , "txBodyMetadataHash" .= txBodyMetadataHash
             , "fullMetadataHash" .= fullMetadataHash
             ]
  toObject _verb InvalidMetadata =
    mkObject [ "kind" .= String "InvalidMetadata"
             ]

instance ( SophieBasedEra era
         , ToJSON (Core.Value era)
         , ToJSON (Core.TxOut era)
         , ToObject (PredicateFailure (Core.EraRule "PPUP" era))
         )
      => ToObject (UtxoPredicateFailure era) where
  toObject _verb (BadInputsUTxO badInputs) =
    mkObject [ "kind" .= String "BadInputsUTxO"
             , "badInputs" .= badInputs
             , "error" .= renderBadInputsUTxOErr badInputs
             ]
  toObject _verb (ExpiredUTxO ttl slot) =
    mkObject [ "kind" .= String "ExpiredUTxO"
             , "ttl"  .= ttl
             , "slot" .= slot ]
  toObject _verb (MaxTxSizeUTxO txsize maxtxsize) =
    mkObject [ "kind" .= String "MaxTxSizeUTxO"
             , "size" .= txsize
             , "maxSize" .= maxtxsize ]
  -- TODO: Add the minimum allowed UTxO value to OutputTooSmallUTxO
  toObject _verb (OutputTooSmallUTxO badOutputs) =
    mkObject [ "kind" .= String "OutputTooSmallUTxO"
             , "outputs" .= badOutputs
             , "error" .= String "The output is smaller than the allow minimum \
                                 \UTxO value defined in the protocol parameters"
             ]
  toObject _verb (OutputBootAddrAttrsTooBig badOutputs) =
    mkObject [ "kind" .= String "OutputBootAddrAttrsTooBig"
             , "outputs" .= badOutputs
             , "error" .= String "The Cole address attributes are too big"
             ]
  toObject _verb InputSetEmptyUTxO =
    mkObject [ "kind" .= String "InputSetEmptyUTxO" ]
  toObject _verb (FeeTooSmallUTxO minfee txfee) =
    mkObject [ "kind" .= String "FeeTooSmallUTxO"
             , "minimum" .= minfee
             , "fee" .= txfee ]
  toObject _verb (ValueNotConservedUTxO consumed produced) =
    mkObject [ "kind" .= String "ValueNotConservedUTxO"
             , "consumed" .= consumed
             , "produced" .= produced
             , "error" .= renderValueNotConservedErr consumed produced
             ]
  toObject verb (UpdateFailure f) = toObject verb f

  toObject _verb (WrongNetwork network addrs) =
    mkObject [ "kind" .= String "WrongNetwork"
             , "network" .= network
             , "addrs"   .= addrs
             ]
  toObject _verb (WrongNetworkWithdrawal network addrs) =
    mkObject [ "kind" .= String "WrongNetworkWithdrawal"
             , "network" .= network
             , "addrs"   .= addrs
             ]

instance ToJSON MA.ValidityInterval where
  toJSON vi =
    Aeson.object $
        [ "invalidBefore"    .= x | x <- mbfield (MA.invalidBefore    vi) ]
     ++ [ "invalidHereafter" .= x | x <- mbfield (MA.invalidHereafter vi) ]
    where
      mbfield SNothing  = []
      mbfield (SJust x) = [x]

instance ( SophieBasedEra era
         , ToJSON (Core.Value era)
         , ToJSON (Core.TxOut era)
         , ToObject (PredicateFailure (Core.EraRule "PPUP" era))
         ) => ToObject (MA.UtxoPredicateFailure era) where
  toObject _verb (MA.BadInputsUTxO badInputs) =
    mkObject [ "kind" .= String "BadInputsUTxO"
             , "badInputs" .= badInputs
             , "error" .= renderBadInputsUTxOErr badInputs
             ]
  toObject _verb (MA.OutsideValidityIntervalUTxO validityInterval slot) =
    mkObject [ "kind" .= String "ExpiredUTxO"
             , "validityInterval" .= validityInterval
             , "slot" .= slot ]
  toObject _verb (MA.MaxTxSizeUTxO txsize maxtxsize) =
    mkObject [ "kind" .= String "MaxTxSizeUTxO"
             , "size" .= txsize
             , "maxSize" .= maxtxsize ]
  toObject _verb MA.InputSetEmptyUTxO =
    mkObject [ "kind" .= String "InputSetEmptyUTxO" ]
  toObject _verb (MA.FeeTooSmallUTxO minfee txfee) =
    mkObject [ "kind" .= String "FeeTooSmallUTxO"
             , "minimum" .= minfee
             , "fee" .= txfee ]
  toObject _verb (MA.ValueNotConservedUTxO consumed produced) =
    mkObject [ "kind" .= String "ValueNotConservedUTxO"
             , "consumed" .= consumed
             , "produced" .= produced
             , "error" .= renderValueNotConservedErr consumed produced
             ]
  toObject _verb (MA.WrongNetwork network addrs) =
    mkObject [ "kind" .= String "WrongNetwork"
             , "network" .= network
             , "addrs"   .= addrs
             ]
  toObject _verb (MA.WrongNetworkWithdrawal network addrs) =
    mkObject [ "kind" .= String "WrongNetworkWithdrawal"
             , "network" .= network
             , "addrs"   .= addrs
             ]
  -- TODO: Add the minimum allowed UTxO value to OutputTooSmallUTxO
  toObject _verb (MA.OutputTooSmallUTxO badOutputs) =
    mkObject [ "kind" .= String "OutputTooSmallUTxO"
             , "outputs" .= badOutputs
             , "error" .= String "The output is smaller than the allow minimum \
                                 \UTxO value defined in the protocol parameters"
             ]
  toObject verb (MA.UpdateFailure f) = toObject verb f
  toObject _verb (MA.OutputBootAddrAttrsTooBig badOutputs) =
    mkObject [ "kind" .= String "OutputBootAddrAttrsTooBig"
             , "outputs" .= badOutputs
             , "error" .= String "The Cole address attributes are too big"
             ]
  toObject _verb MA.TriesToForgeBCC =
    mkObject [ "kind" .= String "TriesToForgeBCC" ]
  toObject _verb (MA.OutputTooBigUTxO badOutputs) =
    mkObject [ "kind" .= String "OutputTooBigUTxO"
             , "outputs" .= badOutputs
             , "error" .= String "Too many asset ids in the tx output"
             ]

renderBadInputsUTxOErr ::  Set (TxIn era) -> Aeson.Value
renderBadInputsUTxOErr txIns
  | Set.null txIns = String "The transaction contains no inputs."
  | otherwise = String "The transaction contains inputs that do not exist in the UTxO set."

renderValueNotConservedErr :: Show val => val -> val -> Aeson.Value
renderValueNotConservedErr consumed produced = String $
    "This transaction consumed " <> show consumed <> " but produced " <> show produced

instance Ledger.Era era => ToObject (PpupPredicateFailure era) where
  toObject _verb (NonGenesisUpdatePPUP proposalKeys genesisKeys) =
    mkObject [ "kind" .= String "NonGenesisUpdatePPUP"
             , "keys" .= proposalKeys Set.\\ genesisKeys ]
  toObject _verb (PPUpdateWrongEpoch currEpoch intendedEpoch votingPeriod) =
    mkObject [ "kind" .= String "PPUpdateWrongEpoch"
             , "currentEpoch" .= currEpoch
             , "intendedEpoch" .= intendedEpoch
             , "votingPeriod"  .= String (show votingPeriod)
             ]
  toObject _verb (PVCannotFollowPPUP badPv) =
    mkObject [ "kind" .= String "PVCannotFollowPPUP"
             , "badProtocolVersion" .= badPv
             ]


instance ( SophieBasedEra era
         , ToObject (PredicateFailure (Core.EraRule "DELPL" era))
         ) => ToObject (DelegsPredicateFailure era) where
  toObject _verb (DelegateeNotRegisteredDELEG targetPool) =
    mkObject [ "kind" .= String "DelegateeNotRegisteredDELEG"
             , "targetPool" .= targetPool
             ]
  toObject _verb (WithdrawalsNotInRewardsDELEGS incorrectWithdrawals) =
    mkObject [ "kind" .= String "WithdrawalsNotInRewardsDELEGS"
             , "incorrectWithdrawals" .= incorrectWithdrawals
             ]
  toObject verb (DelplFailure f) = toObject verb f


instance ( ToObject (PredicateFailure (Core.EraRule "POOL" era))
         , ToObject (PredicateFailure (Core.EraRule "DELEG" era))
         ) => ToObject (DelplPredicateFailure era) where
  toObject verb (PoolFailure f) = toObject verb f
  toObject verb (DelegFailure f) = toObject verb f

instance Ledger.Era era => ToObject (DelegPredicateFailure era) where
  toObject _verb (StakeKeyAlreadyRegisteredDELEG alreadyRegistered) =
    mkObject [ "kind" .= String "StakeKeyAlreadyRegisteredDELEG"
             , "credential" .= String (textShow alreadyRegistered)
             , "error" .= String "Staking credential already registered"
             ]
  toObject _verb (StakeKeyInRewardsDELEG alreadyRegistered) =
    mkObject [ "kind" .= String "StakeKeyInRewardsDELEG"
             , "credential" .= String (textShow alreadyRegistered)
             , "error" .= String "Staking credential registered in rewards map"
             ]
  toObject _verb (StakeKeyNotRegisteredDELEG notRegistered) =
    mkObject [ "kind" .= String "StakeKeyNotRegisteredDELEG"
             , "credential" .= String (textShow notRegistered)
             , "error" .= String "Staking credential not registered"
             ]
  toObject _verb (StakeKeyNonZeroAccountBalanceDELEG remBalance) =
    mkObject [ "kind" .= String "StakeKeyNonZeroAccountBalanceDELEG"
             , "remainingBalance" .= remBalance
             ]
  toObject _verb (StakeDelegationImpossibleDELEG unregistered) =
    mkObject [ "kind" .= String "StakeDelegationImpossibleDELEG"
             , "credential" .= String (textShow unregistered)
             , "error" .= String "Cannot delegate this stake credential because it is not registered"
             ]
  toObject _verb WrongCertificateTypeDELEG =
    mkObject [ "kind" .= String "WrongCertificateTypeDELEG" ]
  toObject _verb (GenesisKeyNotInMappingDELEG (KeyHash genesisKeyHash)) =
    mkObject [ "kind" .= String "GenesisKeyNotInMappingDELEG"
             , "unknownKeyHash" .= String (textShow genesisKeyHash)
             , "error" .= String "This genesis key is not in the delegation mapping"
             ]
  toObject _verb (VestedKeyNotInMappingDELEG (KeyHash vestedKeyHash)) =
    mkObject [ "kind" .= String "VestedKeyNotInMappingDELEG"
             , "unknownKeyHash" .= String (textShow vestedKeyHash)
             , "error" .= String "This vested key is not in the delegation mapping"
             ]
  toObject _verb (DuplicateGenesisDelegateDELEG (KeyHash genesisKeyHash)) =
    mkObject [ "kind" .= String "DuplicateGenesisDelegateDELEG"
             , "duplicateKeyHash" .= String (textShow genesisKeyHash)
             , "error" .= String "This genesis key has already been delegated to"
             ]
  toObject _verb (DuplicateVestedDelegateDELEG (KeyHash vestedKeyHash)) =
    mkObject [ "kind" .= String "DuplicateVestedDelegateDELEG"
             , "duplicateKeyHash" .= String (textShow vestedKeyHash)
             , "error" .= String "This vested key has already been delegated to"
             ]
  toObject _verb (InsufficientForInstantaneousRewardsDELEG mirpot neededMirAmount reserves) =
    mkObject [ "kind" .= String "InsufficientForInstantaneousRewardsDELEG"
             , "pot" .= String (case mirpot of
                                  ReservesMIR -> "Reserves"
                                  TreasuryMIR -> "Treasury")
             , "neededAmount" .= neededMirAmount
             , "reserves" .= reserves
             ]
  toObject _verb (MIRCertificateTooLateinEpochDELEG currSlot boundSlotNo) =
    mkObject [ "kind" .= String "MIRCertificateTooLateinEpochDELEG"
             , "currentSlotNo" .= currSlot
             , "mustBeSubmittedBeforeSlotNo" .= boundSlotNo
             ]
  toObject _verb (DuplicateGenesisVRFDELEG vrfKeyHash) =
    mkObject [ "kind" .= String "DuplicateGenesisVRFDELEG"
             , "keyHash" .= vrfKeyHash
             ]
  toObject _verb (DuplicateVestedVRFDELEG vrfKeyHash) =
    mkObject [ "kind" .= String "DuplicateVestedVRFDELEG"
             , "keyHash" .= vrfKeyHash
             ]
  toObject _verb MIRTransferNotCurrentlyAllowed =
    mkObject [ "kind" .= String "MIRTransferNotCurrentlyAllowed"
             ]
  toObject _verb MIRNegativesNotCurrentlyAllowed =
    mkObject [ "kind" .= String "MIRNegativesNotCurrentlyAllowed"
             ]
  toObject _verb (InsufficientForTransferDELEG mirpot attempted available) =
    mkObject [ "kind" .= String "DuplicateGenesisVRFDELEG"
             , "pot" .= String (case mirpot of
                                  ReservesMIR -> "Reserves"
                                  TreasuryMIR -> "Treasury")
             , "attempted" .= attempted
             , "available" .= available
             ]
  toObject _verb MIRProducesNegativeUpdate =
    mkObject [ "kind" .= String "MIRProducesNegativeUpdate"
             ]


instance ToObject (PoolPredicateFailure era) where
  toObject _verb (StakePoolNotRegisteredOnKeyPOOL (KeyHash unregStakePool)) =
    mkObject [ "kind" .= String "StakePoolNotRegisteredOnKeyPOOL"
             , "unregisteredKeyHash" .= String (textShow unregStakePool)
             , "error" .= String "This stake pool key hash is unregistered"
             ]
  toObject _verb (StakePoolRetirementWrongEpochPOOL currentEpoch intendedRetireEpoch maxRetireEpoch) =
    mkObject [ "kind" .= String "StakePoolRetirementWrongEpochPOOL"
             , "currentEpoch" .= String (textShow currentEpoch)
             , "intendedRetirementEpoch" .= String (textShow intendedRetireEpoch)
             , "maxEpochForRetirement" .= String (textShow maxRetireEpoch)
             ]
  toObject _verb (StakePoolCostTooLowPOOL certCost protCost) =
    mkObject [ "kind" .= String "StakePoolCostTooLowPOOL"
             , "certificateCost" .= String (textShow certCost)
             , "protocolParCost" .= String (textShow protCost)
             , "error" .= String "The stake pool cost is too low"
             ]
  toObject _verb (PoolMedataHashTooBig poolID hashSize) =
    mkObject [ "kind" .= String "PoolMedataHashTooBig"
             , "poolID" .= String (textShow poolID)
             , "hashSize" .= String (textShow hashSize)
             , "error" .= String "The stake pool metadata hash is too large"
             ]

-- Apparently this should never happen according to the Sophie exec spec
  toObject _verb (WrongCertificateTypePOOL index) =
    case index of
      0 -> mkObject [ "kind" .= String "WrongCertificateTypePOOL"
                    , "error" .= String "Wrong certificate type: Delegation certificate"
                    ]
      1 -> mkObject [ "kind" .= String "WrongCertificateTypePOOL"
                    , "error" .= String "Wrong certificate type: MIR certificate"
                    ]
      2 -> mkObject [ "kind" .= String "WrongCertificateTypePOOL"
                    , "error" .= String "Wrong certificate type: Genesis certificate"
                    ]
      k -> mkObject [ "kind" .= String "WrongCertificateTypePOOL"
                    , "certificateType" .= k
                    , "error" .= String "Wrong certificate type: Unknown certificate type"
                    ]

  toObject _verb (WrongNetworkPOOL networkId listedNetworkId poolId) =
    mkObject [ "kind" .= String "WrongNetworkPOOL"
             , "networkId" .= String (textShow networkId)
             , "listedNetworkId" .= String (textShow listedNetworkId)
             , "poolId" .= String (textShow poolId)
             , "error" .= String "Wrong network ID in pool registration certificate"
             ]

instance ( ToObject (PredicateFailure (Core.EraRule "NEWEPOCH" era))
         , ToObject (PredicateFailure (Core.EraRule "RUPD" era))
         ) => ToObject (TickPredicateFailure era) where
  toObject verb (NewEpochFailure f) = toObject verb f
  toObject verb (RupdFailure f) = toObject verb f

instance ToObject TicknPredicateFailure where
  toObject _verb x = case x of {} -- no constructors

instance ( ToObject (PredicateFailure (Core.EraRule "EPOCH" era))
         , ToObject (PredicateFailure (Core.EraRule "MIR" era))
         ) => ToObject (NewEpochPredicateFailure era) where
  toObject verb (EpochFailure f) = toObject verb f
  toObject verb (MirFailure f) = toObject verb f
  toObject _verb (CorruptRewardUpdate update) =
    mkObject [ "kind" .= String "CorruptRewardUpdate"
             , "update" .= String (show update) ]


instance ( ToObject (PredicateFailure (Core.EraRule "POOLREAP" era))
         , ToObject (PredicateFailure (Core.EraRule "SNAP" era))
         , ToObject (PredicateFailure (Core.EraRule "UPEC" era))
         ) => ToObject (EpochPredicateFailure era) where
  toObject verb (PoolReapFailure f) = toObject verb f
  toObject verb (SnapFailure f) = toObject verb f
  toObject verb (UpecFailure f) = toObject verb f


instance ToObject (PoolreapPredicateFailure era) where
  toObject _verb x = case x of {} -- no constructors


instance ToObject (SnapPredicateFailure era) where
  toObject _verb x = case x of {} -- no constructors

-- TODO: Need to elaborate more on this error
instance ToObject (NewppPredicateFailure era) where
  toObject _verb (UnexpectedDepositPot outstandingDeposits depositPot) =
    mkObject [ "kind" .= String "UnexpectedDepositPot"
             , "outstandingDeposits" .= String (textShow outstandingDeposits)
             , "depositPot" .= String (textShow depositPot)
             ]


instance ToObject (MirPredicateFailure era) where
  toObject _verb x = case x of {} -- no constructors


instance ToObject (RupdPredicateFailure era) where
  toObject _verb x = case x of {} -- no constructors


instance Core.Crypto crypto => ToObject (PrtclPredicateFailure crypto) where
  toObject  verb (OverlayFailure f) = toObject verb f
  toObject  verb (UpdnFailure f) = toObject verb f


instance Core.Crypto crypto => ToObject (OverlayPredicateFailure crypto) where
  toObject _verb (UnknownGenesisVestedKeyOVERLAY (KeyHash genKeyHash)) =
    mkObject [ "kind" .= String "UnknownGenesisVestedKeyOVERLAY"
             , "unknownKeyHash" .= String (textShow genKeyHash)
             ]
  toObject _verb (VRFKeyBadLeaderValue seedNonce (SlotNo currSlotNo) prevHashNonce leaderElecVal) =
    mkObject [ "kind" .= String "VRFKeyBadLeaderValueOVERLAY"
             , "seedNonce" .= String (textShow seedNonce)
             , "currentSlot" .= String (textShow currSlotNo)
             , "previousHashAsNonce" .= String (textShow prevHashNonce)
             , "leaderElectionValue" .= String (textShow leaderElecVal)
             ]
  toObject _verb (VRFKeyBadNonce seedNonce (SlotNo currSlotNo) prevHashNonce blockNonce) =
    mkObject [ "kind" .= String "VRFKeyBadNonceOVERLAY"
             , "seedNonce" .= String (textShow seedNonce)
             , "currentSlot" .= String (textShow currSlotNo)
             , "previousHashAsNonce" .= String (textShow prevHashNonce)
             , "blockNonce" .= String (textShow blockNonce)
             ]
  toObject _verb (VRFKeyWrongVRFKey issuerHash regVRFKeyHash unregVRFKeyHash) =
    mkObject [ "kind" .= String "VRFKeyWrongVRFKeyOVERLAY"
             , "poolHash" .= textShow issuerHash
             , "registeredVRFKeHash" .= textShow regVRFKeyHash
             , "unregisteredVRFKeyHash" .= textShow unregVRFKeyHash
             ]
  --TODO: Pipe slot number with VRFKeyUnknown
  toObject _verb (VRFKeyUnknown (KeyHash kHash)) =
    mkObject [ "kind" .= String "VRFKeyUnknownOVERLAY"
             , "keyHash" .= String (textShow kHash)
             ]
  toObject _verb (VRFLeaderValueTooBig leadElecVal weightOfDelegPool actSlotCoefff) =
    mkObject [ "kind" .= String "VRFLeaderValueTooBigOVERLAY"
             , "leaderElectionValue" .= String (textShow leadElecVal)
             , "delegationPoolWeight" .= String (textShow weightOfDelegPool)
             , "activeSlotCoefficient" .= String (textShow actSlotCoefff)
             ]
  toObject _verb (NotActiveSlotOVERLAY notActiveSlotNo) =
    -- TODO: Elaborate on NotActiveSlot error
    mkObject [ "kind" .= String "NotActiveSlotOVERLAY"
             , "slot" .= String (textShow notActiveSlotNo)
             ]
  toObject _verb (WrongGenesisVestedColdKeyOVERLAY actual expected) =
    mkObject [ "kind" .= String "WrongGenesisVestedColdKeyOVERLAY"
             , "actual" .= actual
             , "expected" .= expected ]
  toObject _verb (WrongGenesisVestedVRFKeyOVERLAY issuer actual expected) =
    mkObject [ "kind" .= String "WrongGenesisVestedVRFKeyOVERLAY"
             , "issuer" .= issuer
             , "actual" .= actual
             , "expected" .= expected ]
  toObject verb (OcertFailure f) = toObject verb f


instance ToObject (OcertPredicateFailure crypto) where
  toObject _verb (KESBeforeStartOCERT (KESPeriod oCertstart) (KESPeriod current)) =
    mkObject [ "kind" .= String "KESBeforeStartOCERT"
             , "opCertKESStartPeriod" .= String (textShow oCertstart)
             , "currentKESPeriod" .= String (textShow current)
             , "error" .= String "Your operational certificate's KES start period \
                                 \is before the KES current period."
             ]
  toObject _verb (KESAfterEndOCERT (KESPeriod current) (KESPeriod oCertstart) maxKESEvolutions) =
    mkObject [ "kind" .= String "KESAfterEndOCERT"
             , "currentKESPeriod" .= String (textShow current)
             , "opCertKESStartPeriod" .= String (textShow oCertstart)
             , "maxKESEvolutions" .= String  (textShow maxKESEvolutions)
             , "error" .= String "The operational certificate's KES start period is \
                                 \greater than the max number of KES + the KES current period"
             ]
  toObject _verb (CounterTooSmallOCERT lastKEScounterUsed currentKESCounter) =
    mkObject [ "kind" .= String "CounterTooSmallOCert"
             , "currentKESCounter" .= String (textShow currentKESCounter)
             , "lastKESCounter" .= String (textShow lastKEScounterUsed)
             , "error" .= String "The operational certificate's last KES counter is greater \
                                 \than the current KES counter."
             ]
  toObject _verb (InvalidSignatureOCERT oCertCounter oCertKESStartPeriod) =
    mkObject [ "kind" .= String "InvalidSignatureOCERT"
             , "opCertKESStartPeriod" .= String (textShow oCertKESStartPeriod)
             , "opCertCounter" .= String (textShow oCertCounter)
             ]
  toObject _verb (InvalidKesSignatureOCERT currKESPeriod startKESPeriod expectedKESEvolutions err) =
    mkObject [ "kind" .= String "InvalidKesSignatureOCERT"
             , "opCertKESStartPeriod" .= String (textShow startKESPeriod)
             , "opCertKESCurrentPeriod" .= String (textShow currKESPeriod)
             , "opCertExpectedKESEvolutions" .= String (textShow expectedKESEvolutions)
             , "error" .= err ]
  toObject _verb (NoCounterForKeyHashOCERT (KeyHash stakePoolKeyHash)) =
    mkObject [ "kind" .= String "NoCounterForKeyHashOCERT"
             , "stakePoolKeyHash" .= String (textShow stakePoolKeyHash)
             , "error" .= String "A counter was not found for this stake pool key hash"
             ]



instance ToObject (UpdnPredicateFailure crypto) where
  toObject _verb x = case x of {} -- no constructors

instance ToObject (UpecPredicateFailure era) where
  toObject _verb (NewPpFailure (UnexpectedDepositPot totalOutstanding depositPot)) =
    mkObject [ "kind" .= String "UnexpectedDepositPot"
             , "totalOutstanding" .=  String (textShow totalOutstanding)
             , "depositPot" .= String (textShow depositPot)
             ]


--------------------------------------------------------------------------------
-- Aurum related
--------------------------------------------------------------------------------


instance ToObject (Aurum.UtxoPredicateFailure (Aurum.AurumEra StandardCrypto)) where
  toObject _verb (Aurum.BadInputsUTxO badInputs) =
    mkObject [ "kind" .= String "BadInputsUTxO"
             , "badInputs" .= badInputs
             , "error" .= renderBadInputsUTxOErr badInputs
             ]
  toObject _verb (Aurum.OutsideValidityIntervalUTxO validtyInterval slot) =
    mkObject [ "kind" .= String "ExpiredUTxO"
             , "validityInterval" .= validtyInterval
             , "slot" .= slot
             ]
  toObject _verb (Aurum.MaxTxSizeUTxO txsize maxtxsize) =
    mkObject [ "kind" .= String "MaxTxSizeUTxO"
             , "size" .= txsize
             , "maxSize" .= maxtxsize
             ]
  toObject _verb Aurum.InputSetEmptyUTxO =
    mkObject [ "kind" .= String "InputSetEmptyUTxO" ]
  toObject _verb (Aurum.FeeTooSmallUTxO minfee currentFee) =
    mkObject [ "kind" .= String "FeeTooSmallUTxO"
             , "minimum" .= minfee
             , "fee" .= currentFee
             ]
  toObject _verb (Aurum.ValueNotConservedUTxO consumed produced) =
    mkObject [ "kind" .= String "ValueNotConservedUTxO"
             , "consumed" .= consumed
             , "produced" .= produced
             , "error" .= renderValueNotConservedErr consumed produced
             ]
  toObject _verb (Aurum.WrongNetwork network addrs) =
    mkObject [ "kind" .= String "WrongNetwork"
             , "network" .= network
             , "addrs"   .= addrs
             ]
  toObject _verb (Aurum.WrongNetworkWithdrawal network addrs) =
    mkObject [ "kind" .= String "WrongNetworkWithdrawal"
             , "network" .= network
             , "addrs"   .= addrs
             ]
  toObject _verb (Aurum.OutputTooSmallUTxO badOutputs) =
    mkObject [ "kind" .= String "OutputTooSmallUTxO"
             , "outputs" .= badOutputs
             , "error" .= String "The output is smaller than the allow minimum \
                                 \UTxO value defined in the protocol parameters"
             ]
  toObject verb (Aurum.UtxosFailure predFailure) =
    toObject verb predFailure
  toObject _verb (Aurum.OutputBootAddrAttrsTooBig txouts) =
    mkObject [ "kind" .= String "OutputBootAddrAttrsTooBig"
             , "outputs" .= txouts
             , "error" .= String "The Cole address attributes are too big"
             ]
  toObject _verb Aurum.TriesToForgeBCC =
    mkObject [ "kind" .= String "TriesToForgeBCC" ]
  toObject _verb (Aurum.OutputTooBigUTxO badOutputs) =
    mkObject [ "kind" .= String "OutputTooBigUTxO"
             , "outputs" .= badOutputs
             , "error" .= String "Too many asset ids in the tx output"
             ]
  toObject _verb (Aurum.InsufficientCollateral computedBalance suppliedFee) =
    mkObject [ "kind" .= String "InsufficientCollateral"
             , "balance" .= computedBalance
             , "txfee" .= suppliedFee
             ]
  toObject _verb (Aurum.ScriptsNotPaidUTxO utxos) =
    mkObject [ "kind" .= String "ScriptsNotPaidUTxO"
             , "utxos" .= utxos
             ]
  toObject _verb (Aurum.ExUnitsTooBigUTxO pParamsMaxExUnits suppliedExUnits) =
    mkObject [ "kind" .= String "ExUnitsTooBigUTxO"
             , "maxexunits" .= pParamsMaxExUnits
             , "exunits" .= suppliedExUnits
             ]
  toObject _verb (Aurum.CollateralContainsNonBCC inputs) =
    mkObject [ "kind" .= String "CollateralContainsNonBCC"
             , "inputs" .= inputs
             ]
  toObject _verb (Aurum.WrongNetworkInTxBody actualNetworkId netIdInTxBody) =
    mkObject [ "kind" .= String "WrongNetworkInTxBody"
             , "networkid" .= actualNetworkId
             , "txbodyNetworkId" .= netIdInTxBody
             ]
  toObject _verb (Aurum.OutsideForecast slotNum) =
    mkObject [ "kind" .= String "OutsideForecast"
             , "slot" .= slotNum
             ]
  toObject _verb (Aurum.TooManyCollateralInputs maxCollateralInputs numberCollateralInputs) =
    mkObject [ "kind" .= String "TooManyCollateralInputs"
             , "max" .= maxCollateralInputs
             , "inputs" .= numberCollateralInputs
             ]
  toObject _verb Aurum.NoCollateralInputs =
    mkObject [ "kind" .= String "NoCollateralInputs" ]

instance ToObject (Aurum.UtxosPredicateFailure (AurumEra StandardCrypto)) where
  toObject _ (Aurum.ValidationTagMismatch isValidating reason) =
    mkObject [ "kind" .= String "ValidationTagMismatch"
             , "isvalidating" .= isValidating
             , "reason" .= reason
             ]
  toObject _ (Aurum.CollectErrors errors) =
    mkObject [ "kind" .= String "CollectErrors"
             , "errors" .= errors
             ]
  toObject verb (Aurum.UpdateFailure pFailure) =
    toObject verb pFailure

deriving newtype instance ToJSON Aurum.IsValid

instance ToJSON (Aurum.CollectError StandardCrypto) where
  toJSON cError =
    case cError of
      Aurum.NoRedeemer sPurpose ->
        object
          [ "kind" .= String "CollectError"
          , "error" .= String "NoRedeemer"
          , "scriptpurpose" .= renderScriptPurpose sPurpose
          ]
      Aurum.NoWitness sHash ->
        object
          [ "kind" .= String "CollectError"
          , "error" .= String "NoWitness"
          , "scripthash" .= toJSON sHash
          ]
      Aurum.NoCostModel lang ->
        object
          [ "kind" .= String "CollectError"
          , "error" .= String "NoCostModel"
          , "language" .= toJSON lang
          ]

instance ToJSON Aurum.TagMismatchDescription where
  toJSON tmd = case tmd of
    Aurum.PassedUnexpectedly ->
      object
        [ "kind" .= String "TagMismatchDescription"
        , "error" .= String "PassedUnexpectedly"
        ]
    Aurum.FailedUnexpectedly forReasons ->
      object
        [ "kind" .= String "TagMismatchDescription"
        , "error" .= String "FailedUnexpectedly"
        , "reconstruction" .= forReasons
        ]

instance ToJSON Aurum.FailureDescription where
  toJSON f = case f of
    Aurum.OnePhaseFailure t ->
      object
        [ "kind" .= String "FailureDescription"
        , "error" .= String "OnePhaseFailure"
        , "description" .= t
        ]
    Aurum.ZerepochFailure t bs ->
      object
        [ "kind" .= String "FailureDescription"
        , "error" .= String "ZerepochFailure"
        , "description" .= t
        , "reconstructionDetail" .= bs
        ]

instance ToObject (AurumBbodyPredFail (Aurum.AurumEra StandardCrypto)) where
  toObject _ err = mkObject [ "kind" .= String "AurumBbodyPredFail"
                            , "error" .= String (show err)
                            ]

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

textShow :: Show a => a -> Text
textShow = Text.pack . show

showLastAppBlockNo :: WithOrigin (LastAppliedBlock crypto) -> Text
showLastAppBlockNo wOblk =  case withOriginToMaybe wOblk of
                     Nothing -> "Genesis Block"
                     Just blk -> textShow . unBlockNo $ labBlockNo blk

-- Common to bcc-cli

deriving newtype instance Core.Crypto crypto => ToJSON (Core.AuxiliaryDataHash crypto)

deriving newtype instance Core.Crypto crypto => ToJSON (TxId crypto)
