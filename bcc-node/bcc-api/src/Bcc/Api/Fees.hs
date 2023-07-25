{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

-- | Fee calculation
--
module Bcc.Api.Fees (

    -- * Transaction fees
    transactionFee,
    estimateTransactionFee,
    evaluateTransactionFee,
    estimateTransactionKeyWitnessCount,

    -- * Script execution units
    evaluateTransactionExecutionUnits,
    ScriptExecutionError(..),
    TransactionValidityIntervalError(..),

    -- * Transaction balance
    evaluateTransactionBalance,

    -- * Automated transaction building
    makeTransactionBodyAutoBalance,
    BalancedTxBody(..),
    TxBodyErrorAutoBalance(..),

    -- * Minimum UTxO calculation
    calculateMinimumUTxO,
    MinimumUTxOError(..),
  ) where

import           Prelude

import qualified Data.Array as Array
import           Data.Bifunctor (bimap, first)
import qualified Data.ByteString as BS
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Sequence.Strict (StrictSeq (..))
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import           GHC.Records (HasField (..))
import           Numeric.Natural

import           Control.Monad.Trans.Except
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.String as PP

import qualified Bcc.Binary as CBOR
import           Bcc.Slotting.EpochInfo (EpochInfo, hoistEpochInfo)

import qualified Bcc.Chain.Common as Cole

import qualified Bcc.Ledger.Aurum.Rules.Utxo as Aurum
import qualified Bcc.Ledger.Coin as Ledger
import qualified Bcc.Ledger.Core as Ledger
import qualified Bcc.Ledger.Crypto as Ledger
import qualified Bcc.Ledger.Era as Ledger.Era (Crypto)
import qualified Bcc.Ledger.Keys as Ledger
import qualified Sophie.Spec.Ledger.API as Ledger (CLI, DCert, TxIn, Wdrl)
import qualified Sophie.Spec.Ledger.API.Wallet as Ledger (evaluateTransactionBalance,
                   evaluateTransactionFee)

import           Sophie.Spec.Ledger.PParams (PParams' (..))

import qualified Bcc.Ledger.Jen.Value as Jen

import qualified Bcc.Ledger.Aurum as Aurum
import qualified Bcc.Ledger.Aurum.Language as Aurum
import           Bcc.Ledger.Aurum.PParams (PParams' (..))
import qualified Bcc.Ledger.Aurum.Scripts as Aurum
import qualified Bcc.Ledger.Aurum.Tools as Aurum
import qualified Bcc.Ledger.Aurum.TxWitness as Aurum

import qualified Zerepoch.V1.Ledger.Api as Zerepoch

import qualified Shardagnostic.Consensus.HardFork.History as Consensus

import           Bcc.Api.Address
import           Bcc.Api.Certificate
import           Bcc.Api.Eras
import           Bcc.Api.Error
import           Bcc.Api.Modes
import           Bcc.Api.NetworkId
import           Bcc.Api.ProtocolParameters
import           Bcc.Api.Query
import           Bcc.Api.Script
import           Bcc.Api.Tx
import           Bcc.Api.TxBody
import           Bcc.Api.Value

{- HLINT ignore "Redundant return" -}

-- ----------------------------------------------------------------------------
-- Transaction fees
--

-- | For a concrete fully-constructed transaction, determine the minimum fee
-- that it needs to pay.
--
-- This function is simple, but if you are doing input selection then you
-- probably want to consider estimateTransactionFee.
--
transactionFee :: forall era.
                  IsSophieBasedEra era
               => Natural -- ^ The fixed tx fee
               -> Natural -- ^ The tx fee per byte
               -> Tx era
               -> Entropic
transactionFee txFeeFixed txFeePerByte tx =
  let a = toInteger txFeePerByte
      b = toInteger txFeeFixed
  in case tx of
       SophieTx _ tx' -> let x = obtainHasField sophieBasedEra $ getField @"txsize" tx'
                          in Entropic (a * x + b)
       --TODO: This can be made to work for Cole txs too. Do that: fill in this case
       -- and remove the IsSophieBasedEra constraint.
       ColeTx _ -> case sophieBasedEra :: SophieBasedEra ColeEra of {}
 where
  obtainHasField
    :: SophieLedgerEra era ~ ledgerera
    => SophieBasedEra era
    -> ( HasField "txsize" (Ledger.Tx (SophieLedgerEra era)) Integer
        => a)
    -> a
  obtainHasField SophieBasedEraSophie f = f
  obtainHasField SophieBasedEraEvie f = f
  obtainHasField SophieBasedEraJen    f = f
  obtainHasField SophieBasedEraAurum  f = f

{-# DEPRECATED transactionFee "Use 'evaluateTransactionFee' instead" #-}


--TODO: in the Cole case the per-byte is non-integral, would need different
-- parameters. e.g. a new data type for fee params, Cole vs Sophie

-- | This can estimate what the transaction fee will be, based on a starting
-- base transaction, plus the numbers of the additional components of the
-- transaction that may be added.
--
-- So for example with wallet coin selection, the base transaction should
-- contain all the things not subject to coin selection (such as script inputs,
-- metadata, withdrawals, certs etc)
--
estimateTransactionFee :: forall era.
                          IsSophieBasedEra era
                       => NetworkId
                       -> Natural -- ^ The fixed tx fee
                       -> Natural -- ^ The tx fee per byte
                       -> Tx era
                       -> Int -- ^ The number of extra UTxO transaction inputs
                       -> Int -- ^ The number of extra transaction outputs
                       -> Int -- ^ The number of extra Sophie key witnesses
                       -> Int -- ^ The number of extra Cole key witnesses
                       -> Entropic
estimateTransactionFee nw txFeeFixed txFeePerByte (SophieTx era tx) =
    let Entropic baseFee = transactionFee txFeeFixed txFeePerByte (SophieTx era tx)
     in \nInputs nOutputs nSophieKeyWitnesses nColeKeyWitnesses ->

        --TODO: this is fragile. Move something like this to the ledger and
        -- make it robust, based on the txsize calculation.
        let extraBytes :: Int
            extraBytes = nInputs               * sizeInput
                       + nOutputs              * sizeOutput
                       + nColeKeyWitnesses    * sizeColeKeyWitnesses
                       + nSophieKeyWitnesses  * sizeSophieKeyWitnesses

         in Entropic (baseFee + toInteger txFeePerByte * toInteger extraBytes)
  where
    sizeInput               = smallArray + uint + hashObj
    sizeOutput              = smallArray + uint + address
    sizeColeKeyWitnesses   = smallArray + keyObj + sigObj + ccodeObj + attrsObj
    sizeSophieKeyWitnesses = smallArray + keyObj + sigObj

    smallArray  = 1
    uint        = 5

    hashObj     = 2 + hashLen
    hashLen     = 32

    keyObj      = 2 + keyLen
    keyLen      = 32

    sigObj      = 2 + sigLen
    sigLen      = 64

    ccodeObj    = 2 + ccodeLen
    ccodeLen    = 32

    address     = 2 + addrHeader + 2 * addrHashLen
    addrHeader  = 1
    addrHashLen = 28

    attrsObj    = 2 + BS.length attributes
    attributes  = CBOR.serialize' $
                    Cole.mkAttributes Cole.AddrAttributes {
                      Cole.aaVKDerivationPath = Nothing,
                      Cole.aaNetworkMagic     = toColeNetworkMagic nw
                    }

--TODO: This can be made to work for Cole txs too. Do that: fill in this case
-- and remove the IsSophieBasedEra constraint.
estimateTransactionFee _ _ _ (ColeTx _) =
    case sophieBasedEra :: SophieBasedEra era of {}

--TODO: also deprecate estimateTransactionFee:
--{-# DEPRECATED estimateTransactionFee "Use 'evaluateTransactionFee' instead" #-}


-- | Compute the transaction fee for a proposed transaction, with the
-- assumption that there will be the given number of key witnesses (i.e.
-- signatures).
--
-- TODO: we need separate args for Sophie vs Cole key sigs
--
evaluateTransactionFee :: forall era.
                          IsSophieBasedEra era
                       => ProtocolParameters
                       -> TxBody era
                       -> Word  -- ^ The number of Sophie key witnesses
                       -> Word  -- ^ The number of Cole key witnesses
                       -> Entropic
evaluateTransactionFee _ _ _ colewitcount | colewitcount > 0 =
  error "evaluateTransactionFee: TODO support Cole key witnesses"

evaluateTransactionFee pparams txbody keywitcount _colewitcount =
    case makeSignedTransaction [] txbody of
      ColeTx{} -> case sophieBasedEra :: SophieBasedEra era of {}
      --TODO: we could actually support Cole here, it'd be different but simpler

      SophieTx era tx -> withLedgerConstraints era (evalSophieBasedEra era tx)
  where
    evalSophieBasedEra :: forall ledgerera.
                           SophieLedgerEra era ~ ledgerera
                        => Ledger.CLI ledgerera
                        => SophieBasedEra era
                        -> Ledger.Tx ledgerera
                        -> Entropic
    evalSophieBasedEra era tx =
      fromSophieEntropic $
        Ledger.evaluateTransactionFee
          (toLedgerPParams era pparams)
          tx
          keywitcount

    -- Conjur up all the necessary class instances and evidence
    withLedgerConstraints
      :: SophieLedgerEra era ~ ledgerera
      => SophieBasedEra era
      -> (   Ledger.CLI ledgerera
          => a)
      -> a
    withLedgerConstraints SophieBasedEraSophie f = f
    withLedgerConstraints SophieBasedEraEvie f = f
    withLedgerConstraints SophieBasedEraJen    f = f
    withLedgerConstraints SophieBasedEraAurum  f = f

-- | Give an approximate count of the number of key witnesses (i.e. signatures)
-- a transaction will need.
--
-- This is an estimate not a precise count in that it can over-estimate: it
-- makes conservative assumptions such as all inputs are from distinct
-- addresses, but in principle multiple inputs can use the same address and we
-- only need a witness per address.
--
-- Similarly there can be overlap between the regular and collateral inputs,
-- but we conservatively assume they are distinct.
--
-- TODO: it is worth us considering a more precise count that relies on the
-- UTxO to resolve which inputs are for distinct addresses, and also to count
-- the number of Sophie vs Cole style witnesses.
--
estimateTransactionKeyWitnessCount :: TxBodyContent BuildTx era -> Word
estimateTransactionKeyWitnessCount TxBodyContent {
                                     txIns,
                                     txInsCollateral,
                                     txExtraKeyWits,
                                     txWithdrawals,
                                     txCertificates,
                                     txUpdateProposal
                                   } =
  fromIntegral $
    length [ () | (_txin, BuildTxWith KeyWitness{}) <- txIns ]

  + case txInsCollateral of
      TxInsCollateral _ txins
        -> length txins
      _ -> 0

  + case txExtraKeyWits of
      TxExtraKeyWitnesses _ khs
        -> length khs
      _ -> 0

  + case txWithdrawals of
      TxWithdrawals _ withdrawals
        -> length [ () | (_, _, BuildTxWith KeyWitness{}) <- withdrawals ]
      _ -> 0

  + case txCertificates of
      TxCertificates _ _ (BuildTxWith witnesses)
        -> length [ () | KeyWitness{} <- Map.elems witnesses ]
      _ -> 0

  + case txUpdateProposal of
      TxUpdateProposal _ (UpdateProposal updatePerGenesisKey _)
        -> Map.size updatePerGenesisKey
      _ -> 0


-- ----------------------------------------------------------------------------
-- Script execution units
--

-- | The different possible reasons that executing a script can fail,
-- as reported by 'evaluateTransactionExecutionUnits'.
--
-- The first three of these are about failures before we even get to execute
-- the script, and two are the result of execution.
--
data ScriptExecutionError =

       -- | The script depends on a 'TxIn' that has not been provided in the
       -- given 'UTxO' subset. The given 'UTxO' must cover all the inputs
       -- the transaction references.
       ScriptErrorMissingTxIn TxIn

       -- | The 'TxIn' the script is spending does not have a 'ScriptDatum'.
       -- All inputs guarded by Zerepoch scripts need to have been created with
       -- a 'ScriptDatum'.
     | ScriptErrorTxInWithoutDatum TxIn

       -- | The 'ScriptDatum' provided does not match the one from the 'UTxO'.
       -- This means the wrong 'ScriptDatum' value has been provided.
       --
     | ScriptErrorWrongDatum (Hash ScriptData)

       -- | The script evaluation failed. This usually means it evaluated to an
       -- error value. This is not a case of running out of execution units
       -- (which is not possible for 'evaluateTransactionExecutionUnits' since
       -- the whole point of it is to discover how many execution units are
       -- needed).
       --
     | ScriptErrorEvaluationFailed Zerepoch.EvaluationError

       -- | The execution units overflowed a 64bit word. Congratulations if
       -- you encounter this error. With the current style of cost model this
       -- would need a script to run for over 7 months, which is somewhat more
       -- than the expected maximum of a few milliseconds.
       --
     | ScriptErrorExecutionUnitsOverflow

       -- | An attempt was made to spend a key witnessed tx input
       -- with a script witness.
     | ScriptErrorNotZerepochWitnessedTxIn ScriptWitnessIndex
  deriving Show

instance Error ScriptExecutionError where
  displayError (ScriptErrorMissingTxIn txin) =
      "The supplied UTxO is missing the txin " ++ Text.unpack (renderTxIn txin)

  displayError (ScriptErrorTxInWithoutDatum txin) =
      "The Zerepoch script witness for the txin does not have a script datum "
   ++ "(according to the UTxO). The txin in question is "
   ++ Text.unpack (renderTxIn txin)

  displayError (ScriptErrorWrongDatum dh) =
      "The Zerepoch script witness has the wrong datum (according to the UTxO). "
   ++ "The expected datum value has hash " ++ show dh

  displayError (ScriptErrorEvaluationFailed evalErr) =
      "The Zerepoch script evaluation failed: " ++ pp evalErr
    where
      pp :: PP.Pretty p => p -> String
      pp = PP.renderString
         . PP.layoutPretty PP.defaultLayoutOptions
         . PP.pretty

  displayError ScriptErrorExecutionUnitsOverflow =
      "The execution units required by this Zerepoch script overflows a 64bit "
   ++ "word. In a properly configured chain this should be practically "
   ++ "impossible. So this probably indicates a chain configuration problem, "
   ++ "perhaps with the values in the cost model."

  displayError (ScriptErrorNotZerepochWitnessedTxIn scriptWitness) =
      renderScriptWitnessIndex scriptWitness <> " is not a Zerepoch script \
      \witnessed tx input and cannot be spent using a Zerepoch script witness."

-- | The transaction validity interval is too far into the future.
--
-- Transactions with Zerepoch scripts need to have a validity interval that is
-- not so far in the future that we cannot reliably determine the UTC time
-- corresponding to the validity interval expressed in slot numbers.
--
-- This is because the Zerepoch scripts get given the transaction validity
-- interval in UTC time, so that they are not sensitive to slot lengths.
--
-- If either end of the validity interval is beyond the so called \"time
-- horizon\" then the consensus algorithm is not able to reliably determine
-- the relationship between slots and time. This is this situation in which
-- this error is reported. For the Bcc mainnet the time horizon is 36
-- hours beyond the current time. This effectively means we cannot submit
-- check or submit transactions that use Zerepoch scripts that have the end
-- of their validity interval more than 36 hours into the future.
--
newtype TransactionValidityIntervalError =
          TransactionValidityIntervalError Consensus.PastHorizonException
  deriving Show

instance Error TransactionValidityIntervalError where
  displayError (TransactionValidityIntervalError pastTimeHorizon) =
      "The transaction validity interval is too far in the future. "
   ++ "For this network it must not be more than "
   ++ show (timeHorizonSlots pastTimeHorizon)
   ++ "slots ahead of the current time slot. "
   ++ "(Transactions with Zerepoch scripts must have validity intervals that "
   ++ "are close enough in the future that we can reliably turn the slot "
   ++ "numbers into UTC wall clock times.)"
    where
      timeHorizonSlots :: Consensus.PastHorizonException -> Word
      timeHorizonSlots Consensus.PastHorizon{Consensus.pastHorizonSummary}
        | eraSummaries@(_:_) <- pastHorizonSummary
        , Consensus.StandardSafeZone slots <-
            (Consensus.eraSafeZone . Consensus.eraParams . last) eraSummaries
        = fromIntegral slots

        | otherwise
        = 0 -- This should be impossible.



-- | Compute the 'ExecutionUnits' needed for each script in the transaction.
--
-- This works by running all the scripts and counting how many execution units
-- are actually used.
--
evaluateTransactionExecutionUnits
  :: forall era mode.
     EraInMode era mode
  -> SystemStart
  -> EraHistory mode
  -> ProtocolParameters
  -> UTxO era
  -> TxBody era
  -> Either TransactionValidityIntervalError
            (Map ScriptWitnessIndex (Either ScriptExecutionError ExecutionUnits))
evaluateTransactionExecutionUnits _eraInMode systemstart history pparams utxo txbody =
    case makeSignedTransaction [] txbody of
      ColeTx {}                 -> evalPreAurum
      SophieTx era tx' ->
        case era of
          SophieBasedEraSophie -> evalPreAurum
          SophieBasedEraEvie -> evalPreAurum
          SophieBasedEraJen    -> evalPreAurum
          SophieBasedEraAurum  -> evalAurum era tx'
  where
    -- Pre-Aurum eras do not support languages with execution unit accounting.
    evalPreAurum :: Either TransactionValidityIntervalError
                            (Map ScriptWitnessIndex
                                 (Either ScriptExecutionError ExecutionUnits))
    evalPreAurum = Right Map.empty

    evalAurum :: forall ledgerera.
                  SophieLedgerEra era ~ ledgerera
               => ledgerera ~ Aurum.AurumEra Ledger.StandardCrypto
               => LedgerEraConstraints ledgerera
               => SophieBasedEra era
               -> Ledger.Tx ledgerera
               -> Either TransactionValidityIntervalError
                         (Map ScriptWitnessIndex
                              (Either ScriptExecutionError ExecutionUnits))
    evalAurum era tx =
      case Aurum.evaluateTransactionExecutionUnits
             (toLedgerPParams era pparams)
             tx
             (toLedgerUTxO era utxo)
             (toLedgerEpochInfo history)
             systemstart
             (toAurumCostModels (protocolParamCostModels pparams))
        of Left  err   -> Left err
           Right exmap -> Right (fromLedgerScriptExUnitsMap exmap)

    toLedgerEpochInfo :: EraHistory mode
                      -> EpochInfo (Either TransactionValidityIntervalError)
    toLedgerEpochInfo (EraHistory _ interpreter) =
        hoistEpochInfo (first TransactionValidityIntervalError . runExcept) $
          Consensus.interpreterToEpochInfo interpreter

    toAurumCostModels :: Map AnyZerepochScriptVersion CostModel
                       -> Array.Array Aurum.Language Aurum.CostModel
    toAurumCostModels costmodels =
      Array.array
        (minBound, maxBound)
        [ (toAurumLanguage lang, toAurumCostModel costmodel)
        | (lang, costmodel) <- Map.toList costmodels ]

    fromLedgerScriptExUnitsMap
      :: Map Aurum.RdmrPtr (Either (Aurum.ScriptFailure Ledger.StandardCrypto)
                                    Aurum.ExUnits)
      -> Map ScriptWitnessIndex (Either ScriptExecutionError ExecutionUnits)
    fromLedgerScriptExUnitsMap exmap =
      Map.fromList
        [ (fromAurumRdmrPtr rdmrptr,
           bimap fromAurumScriptExecutionError fromAurumExUnits exunitsOrFailure)
        | (rdmrptr, exunitsOrFailure) <- Map.toList exmap ]

    fromAurumScriptExecutionError :: Aurum.ScriptFailure Ledger.StandardCrypto
                                   -> ScriptExecutionError
    fromAurumScriptExecutionError failure =
      case failure of
        Aurum.UnknownTxIn     txin -> ScriptErrorMissingTxIn txin'
                                         where txin' = fromSophieTxIn txin
        Aurum.InvalidTxIn     txin -> ScriptErrorTxInWithoutDatum txin'
                                         where txin' = fromSophieTxIn txin
        Aurum.MissingDatum      dh -> ScriptErrorWrongDatum (ScriptDataHash dh)
        Aurum.ValidationFailed err -> ScriptErrorEvaluationFailed err
        Aurum.IncompatibleBudget _ -> ScriptErrorExecutionUnitsOverflow

        -- This is only possible for spending scripts and occurs when
        -- we attempt to spend a key witnessed tx input with a Zerepoch
        -- script witness.
        Aurum.RedeemerNotNeeded rdmrPtr ->
          ScriptErrorNotZerepochWitnessedTxIn $ fromAurumRdmrPtr rdmrPtr
        -- Some of the errors are impossible by construction, given the way we
        -- build transactions in the API:
        Aurum.MissingScript rdmrPtr ->
          impossible ("MissingScript " ++ show (fromAurumRdmrPtr rdmrPtr))

    impossible detail = error $ "evaluateTransactionExecutionUnits: "
                             ++ "the impossible happened: " ++ detail


-- ----------------------------------------------------------------------------
-- Transaction balance
--

-- | Compute the total balance of the proposed transaction. Ultimately a valid
-- transaction must be fully balanced: that is have a total value of zero.
--
-- Finding the (non-zero) balance of partially constructed transaction is
-- useful for adjusting a transaction to be fully balanced.
--
evaluateTransactionBalance :: forall era.
                              IsSophieBasedEra era
                           => ProtocolParameters
                           -> Set PoolId
                           -> UTxO era
                           -> TxBody era
                           -> TxOutValue era
evaluateTransactionBalance _ _ _ (ColeTxBody _) =
    case sophieBasedEra :: SophieBasedEra era of {}
    --TODO: we could actually support Cole here, it'd be different but simpler

evaluateTransactionBalance pparams poolids utxo
                           (SophieTxBody era txbody _ _ _ _) =
    withLedgerConstraints era evalBccOnly evalMultiAsset
  where
    isNewPool :: Ledger.KeyHash Ledger.StakePool Ledger.StandardCrypto -> Bool
    isNewPool kh = StakePoolKeyHash kh `Set.notMember` poolids

    evalMultiAsset :: forall ledgerera.
                      SophieLedgerEra era ~ ledgerera
                   => LedgerEraConstraints ledgerera
                   => LedgerMultiAssetConstraints ledgerera
                   => MultiAssetSupportedInEra era
                   -> TxOutValue era
    evalMultiAsset evidence =
      TxOutValue evidence . fromJenValue $
         Ledger.evaluateTransactionBalance
           (toLedgerPParams era pparams)
           (toLedgerUTxO era utxo)
           isNewPool
           txbody

    evalBccOnly :: forall ledgerera.
                   SophieLedgerEra era ~ ledgerera
                => LedgerEraConstraints ledgerera
                => LedgerBccOnlyConstraints ledgerera
                => OnlyBccSupportedInEra era
                -> TxOutValue era
    evalBccOnly evidence =
     TxOutBccOnly evidence . fromSophieEntropic
       $ Ledger.evaluateTransactionBalance
           (toLedgerPParams era pparams)
           (toLedgerUTxO era utxo)
           isNewPool
           txbody

    -- Conjur up all the necessary class instances and evidence
    withLedgerConstraints
      :: SophieLedgerEra era ~ ledgerera
      => SophieBasedEra era
      -> (   LedgerEraConstraints ledgerera
          => LedgerBccOnlyConstraints ledgerera
          => LedgerPParamsConstraints ledgerera
          => LedgerTxBodyConstraints ledgerera
          => OnlyBccSupportedInEra era
          -> a)
      -> (   LedgerEraConstraints ledgerera
          => LedgerMultiAssetConstraints ledgerera
          => LedgerPParamsConstraints ledgerera
          => LedgerTxBodyConstraints ledgerera
          => MultiAssetSupportedInEra era
          -> a)
      -> a
    withLedgerConstraints SophieBasedEraSophie f _ = f BccOnlyInSophieEra
    withLedgerConstraints SophieBasedEraEvie f _ = f BccOnlyInEvieEra
    withLedgerConstraints SophieBasedEraJen    _ f = f MultiAssetInJenEra
    withLedgerConstraints SophieBasedEraAurum  _ f = f MultiAssetInAurumEra

type LedgerEraConstraints ledgerera =
       ( Ledger.Era.Crypto ledgerera ~ Ledger.StandardCrypto
       , Ledger.CLI ledgerera
       )

type LedgerBccOnlyConstraints ledgerera =
         Ledger.Value ledgerera ~ Ledger.Coin

type LedgerMultiAssetConstraints ledgerera =
       ( Ledger.Value ledgerera ~ Jen.Value Ledger.StandardCrypto
       , HasField "mint" (Ledger.TxBody ledgerera) (Ledger.Value ledgerera)
       )

type LedgerPParamsConstraints ledgerera =
       ( HasField "_minfeeA"     (Ledger.PParams ledgerera) Natural
       , HasField "_minfeeB"     (Ledger.PParams ledgerera) Natural
       , HasField "_keyDeposit"  (Ledger.PParams ledgerera) Ledger.Coin
       , HasField "_poolDeposit" (Ledger.PParams ledgerera) Ledger.Coin
       )

type LedgerTxBodyConstraints ledgerera =
       ( HasField "certs" (Ledger.TxBody ledgerera)
                          (StrictSeq (Ledger.DCert Ledger.StandardCrypto))
       , HasField "inputs" (Ledger.TxBody ledgerera)
                           (Set (Ledger.TxIn Ledger.StandardCrypto))
       , HasField "wdrls" (Ledger.TxBody ledgerera) (Ledger.Wdrl Ledger.StandardCrypto)
       )


-- ----------------------------------------------------------------------------
-- Automated transaction building
--

-- | The possible errors that can arise from 'makeTransactionBodyAutoBalance'.
--
data TxBodyErrorAutoBalance =

       -- | The same errors that can arise from 'makeTransactionBody'.
       TxBodyError TxBodyError

       -- | One or more of the scripts fails to execute correctly.
     | TxBodyScriptExecutionError [(ScriptWitnessIndex, ScriptExecutionError)]

       -- | One or more of the scripts were expected to fail validation, but none did.
     | TxBodyScriptBadScriptValidity

       -- | The balance of the non-bcc assets is not zero. The 'Value' here is
       -- that residual non-zero balance. The 'makeTransactionBodyAutoBalance'
       -- function only automatically balances bcc, not other assets.
     | TxBodyErrorAssetBalanceWrong Value

       -- | There is not enough bcc to cover both the outputs and the fees.
       -- The transaction should be changed to provide more input bcc, or
       -- otherwise adjusted to need less (e.g. outputs, script etc).
       --
     | TxBodyErrorBccBalanceNegative Entropic

       -- | There is enough bcc to cover both the outputs and the fees, but the
       -- resulting change is too small: it is under the minimum value for
       -- new UTxO entries. The transaction should be changed to provide more
       -- input bcc.
       --
     | TxBodyErrorBccBalanceTooSmall
         -- ^ Offending TxOut
         TxOutInAnyEra
         -- ^ Minimum UTxO
         Entropic
         -- ^ Tx balance
         Entropic

       -- | 'makeTransactionBodyAutoBalance' does not yet support the Cole era.
     | TxBodyErrorColeEraNotSupported

       -- | The 'ProtocolParameters' must provide the value for the min utxo
       -- parameter, for eras that use this parameter.
     | TxBodyErrorMissingParamMinUTxO

       -- | The 'ProtocolParameters' must provide the value for the cost per
       -- word parameter, for eras that use this parameter.
     | TxBodyErrorMissingParamCostPerWord

       -- | The transaction validity interval is too far into the future.
       -- See 'TransactionValidityIntervalError' for details.
     | TxBodyErrorValidityInterval TransactionValidityIntervalError

       -- | The minimum spendable UTxO threshold has not been met.
     | TxBodyErrorMinUTxONotMet
         -- ^ Offending TxOut
         TxOutInAnyEra
         -- ^ Minimum UTxO
         Entropic
     | TxBodyErrorMinUTxOMissingPParams MinimumUTxOError
     | TxBodyErrorNonBccAssetsUnbalanced Value
  deriving Show


instance Error TxBodyErrorAutoBalance where
  displayError (TxBodyError err) = displayError err

  displayError (TxBodyScriptExecutionError failures) =
      "The following scripts have execution failures:\n"
   ++ unlines [ "the script for " ++ renderScriptWitnessIndex index
                ++ " failed with: " ++ "\n" ++ displayError failure
              | (index, failure) <- failures ]

  displayError TxBodyScriptBadScriptValidity =
      "One or more of the scripts were expected to fail validation, but none did."

  displayError (TxBodyErrorAssetBalanceWrong _value) =
      "The transaction does not correctly balance in its non-bcc assets. "
   ++ "The balance between inputs and outputs should sum to zero. "
   ++ "The actual balance is: "
   ++ "TODO: move the Value renderer and parser from the CLI into the API and use them here"
   -- TODO: do this ^^

  displayError (TxBodyErrorBccBalanceNegative entropic) =
      "The transaction does not balance in its use of bcc. The net balance "
   ++ "of the transaction is negative: " ++ show entropic ++ " entropic. "
   ++ "The usual solution is to provide more inputs, or inputs with more bcc."

  displayError (TxBodyErrorBccBalanceTooSmall changeOutput minUTxO balance) =
      "The transaction does balance in its use of bcc, however the net "
   ++ "balance does not meet the minimum UTxO threshold. \n"
   ++ "Balance: " ++ show balance ++ "\n"
   ++ "Offending output (change output): " ++ Text.unpack (prettyRenderTxOut changeOutput) ++ "\n"
   ++ "Minimum UTxO threshold: " ++ show minUTxO ++ "\n"
   ++ "The usual solution is to provide more inputs, or inputs with more bcc to \
      \meet the minimum UTxO threshold"

  displayError TxBodyErrorColeEraNotSupported =
      "The Cole era is not yet supported by makeTransactionBodyAutoBalance"

  displayError TxBodyErrorMissingParamMinUTxO =
      "The minUTxOValue protocol parameter is required but missing"

  displayError TxBodyErrorMissingParamCostPerWord =
      "The utxoCostPerWord protocol parameter is required but missing"

  displayError (TxBodyErrorValidityInterval err) =
      displayError err

  displayError (TxBodyErrorMinUTxONotMet txout minUTxO) =
      "Minimum UTxO threshold not met for tx output: " <> Text.unpack (prettyRenderTxOut txout) <> "\n"
   <> "Minimum required UTxO: " <> show minUTxO

  displayError (TxBodyErrorNonBccAssetsUnbalanced val) =
      "Non-Bcc assets are unbalanced: " <> Text.unpack (renderValue val)

  displayError (TxBodyErrorMinUTxOMissingPParams err) = displayError err

handleExUnitsErrors ::
     ScriptValidity -- ^ Mark script as expected to pass or fail validation
  -> Map ScriptWitnessIndex ScriptExecutionError
  -> Map ScriptWitnessIndex ExecutionUnits
  -> Either TxBodyErrorAutoBalance (Map ScriptWitnessIndex ExecutionUnits)
handleExUnitsErrors ScriptValid failuresMap exUnitsMap =
    if null failures
      then Right exUnitsMap
      else Left (TxBodyScriptExecutionError failures)
  where failures :: [(ScriptWitnessIndex, ScriptExecutionError)]
        failures = Map.toList failuresMap
handleExUnitsErrors ScriptInvalid failuresMap exUnitsMap
  | null scriptFailures = Left TxBodyScriptBadScriptValidity
  | null nonScriptFailures = Right exUnitsMap
  | otherwise = Left (TxBodyScriptExecutionError nonScriptFailures)
  where nonScriptFailures :: [(ScriptWitnessIndex, ScriptExecutionError)]
        nonScriptFailures = filter (not . isScriptErrorEvaluationFailed) (Map.toList failuresMap)
        scriptFailures :: [(ScriptWitnessIndex, ScriptExecutionError)]
        scriptFailures = filter isScriptErrorEvaluationFailed (Map.toList failuresMap)
        isScriptErrorEvaluationFailed :: (ScriptWitnessIndex, ScriptExecutionError) -> Bool
        isScriptErrorEvaluationFailed (_, e) = case e of
            ScriptErrorEvaluationFailed _ -> True
            _ -> True

data BalancedTxBody era
  = BalancedTxBody
      (TxBody era)
      (TxOut era) -- ^ Transaction balance (change output)
      Entropic    -- ^ Estimated transaction fee

-- | This is much like 'makeTransactionBody' but with greater automation to
-- calculate suitable values for several things.
--
-- In particular:
--
-- * It calculates the correct script 'ExecutionUnits' (ignoring the provided
--   values, which can thus be zero).
--
-- * It calculates the transaction fees, based on the script 'ExecutionUnits',
--   the current 'ProtocolParameters', and an estimate of the number of
--   key witnesses (i.e. signatures). There is an override for the number of
--   key witnesses.
--
-- * It accepts a change address, calculates the balance of the transaction
--   and puts the excess change into the change output.
--
-- * It also checks that the balance is positive and the change is above the
--   minimum threshold.
--
-- To do this it needs more information than 'makeTransactionBody', all of
-- which can be queried from a local node.
--
makeTransactionBodyAutoBalance
  :: forall era mode.
     IsSophieBasedEra era
  => EraInMode era mode
  -> SystemStart
  -> EraHistory mode
  -> ProtocolParameters
  -> Set PoolId       -- ^ The set of registered stake pools
  -> UTxO era         -- ^ Just the transaction inputs, not the entire 'UTxO'.
  -> TxBodyContent BuildTx era
  -> AddressInEra era -- ^ Change address
  -> Maybe Word       -- ^ Override key witnesses
  -> Either TxBodyErrorAutoBalance (BalancedTxBody era)
makeTransactionBodyAutoBalance eraInMode systemstart history pparams
                            poolids utxo txbodycontent changeaddr mnkeys = do

    -- Our strategy is to:
    -- 1. evaluate all the scripts to get the exec units, update with ex units
    -- 2. figure out the overall min fees
    -- 3. update tx with fees
    -- 4. balance the transaction and update tx change output

    txbody0 <-
      first TxBodyError $ makeTransactionBody txbodycontent
        { txOuts =
              TxOut changeaddr (entropicToTxOutValue 0) TxOutDatumHashNone
            : txOuts txbodycontent
            --TODO: think about the size of the change output
            -- 1,2,4 or 8 bytes?
        }

    exUnitsMap <- first TxBodyErrorValidityInterval $
                    evaluateTransactionExecutionUnits
                      eraInMode
                      systemstart history
                      pparams
                      utxo
                      txbody0

    exUnitsMap' <-
      case Map.mapEither id exUnitsMap of
        (failures, exUnitsMap') ->
          handleExUnitsErrors
            (txScriptValidityToScriptValidity (txScriptValidity txbodycontent))
            failures
            exUnitsMap'

    let txbodycontent1 = substituteExecutionUnits exUnitsMap' txbodycontent

    explicitTxFees <- first (const TxBodyErrorColeEraNotSupported) $
                        txFeesExplicitInEra era'

    -- Make a txbody that we will use for calculating the fees. For the purpose
    -- of fees we just need to make a txbody of the right size in bytes. We do
    -- not need the right values for the fee or change output. We use
    -- "big enough" values for the change output and set so that the CBOR
    -- encoding size of the tx will be big enough to cover the size of the final
    -- output and fee. Yes this means this current code will only work for
    -- final fee of less than around 4000 bcc (2^32-1 entropic) and change output
    -- of less than around 18 trillion bcc  (2^64-1 entropic).
    txbody1 <- first TxBodyError $ -- TODO: impossible to fail now
               makeTransactionBody txbodycontent1 {
                 txFee  = TxFeeExplicit explicitTxFees $ Entropic (2^(32 :: Integer) - 1),
                 txOuts = TxOut changeaddr
                                (entropicToTxOutValue $ Entropic (2^(64 :: Integer)) - 1)
                                TxOutDatumHashNone
                        : txOuts txbodycontent
               }

    let nkeys = fromMaybe (estimateTransactionKeyWitnessCount txbodycontent1)
                          mnkeys
        fee   = evaluateTransactionFee pparams txbody1 nkeys 0 --TODO: cole keys

    -- Make a txbody for calculating the balance. For this the size of the tx
    -- does not matter, instead it's just the values of the fee and outputs.
    -- Here we do not want to start with any change output, since that's what
    -- we need to calculate.
    txbody2 <- first TxBodyError $ -- TODO: impossible to fail now
               makeTransactionBody txbodycontent1 {
                 txFee = TxFeeExplicit explicitTxFees fee
               }

    let balance = evaluateTransactionBalance pparams poolids utxo txbody2

    mapM_ (`checkMinUTxOValue` pparams) $ txOuts txbodycontent1

    -- check if the balance is positive or negative
    -- in one case we can produce change, in the other the inputs are insufficient
    case balance of
      TxOutBccOnly _ _ -> balanceCheck balance
      TxOutValue _ v   ->
        case valueToEntropic v of
          Nothing -> Left $ TxBodyErrorNonBccAssetsUnbalanced v
          Just _ -> balanceCheck balance

    --TODO: we could add the extra fee for the CBOR encoding of the change,
    -- now that we know the magnitude of the change: i.e. 1-8 bytes extra.

    -- The txbody with the final fee and change output. This should work
    -- provided that the fee and change are less than 2^32-1, and so will
    -- fit within the encoding size we picked above when calculating the fee.
    -- Yes this could be an over-estimate by a few bytes if the fee or change
    -- would fit within 2^16-1. That's a possible optimisation.
    txbody3 <-
      first TxBodyError $ -- TODO: impossible to fail now
        makeTransactionBody txbodycontent1 {
          txFee  = TxFeeExplicit explicitTxFees fee,
          txOuts = accountForNoChange
                     (TxOut changeaddr balance TxOutDatumHashNone)
                     (txOuts txbodycontent)
        }
    return (BalancedTxBody txbody3 (TxOut changeaddr balance TxOutDatumHashNone) fee)
 where
   era :: SophieBasedEra era
   era = sophieBasedEra

   era' :: BccEra era
   era' = bccEra

   -- In the event of spending the exact amount of entropic in
   -- the specified input(s), this function excludes the change
   -- output. Note that this does not save any fees because by default
   -- the fee calculation includes a change address for simplicity and
   -- we make no attempt to recalculate the tx fee without a change address.
   accountForNoChange :: TxOut era -> [TxOut era] -> [TxOut era]
   accountForNoChange change@(TxOut _ balance _) rest =
     case txOutValueToEntropic balance of
       Entropic 0 -> rest
       _ -> change : rest

   balanceCheck :: TxOutValue era -> Either TxBodyErrorAutoBalance ()
   balanceCheck balance
    | txOutValueToEntropic balance == 0 = return ()
    | txOutValueToEntropic balance < 0 =
        Left . TxBodyErrorBccBalanceNegative $ txOutValueToEntropic balance
    | otherwise =
        case checkMinUTxOValue (TxOut changeaddr balance TxOutDatumHashNone) pparams of
          Left (TxBodyErrorMinUTxONotMet txOutAny minUTxO) ->
            Left $ TxBodyErrorBccBalanceTooSmall txOutAny minUTxO (txOutValueToEntropic balance)
          Left err -> Left err
          Right _ -> Right ()

   checkMinUTxOValue
     :: TxOut era
     -> ProtocolParameters
     -> Either TxBodyErrorAutoBalance ()
   checkMinUTxOValue txout@(TxOut _ v _) pparams' = do
     minUTxO  <- first TxBodyErrorMinUTxOMissingPParams
                   $ calculateMinimumUTxO era txout pparams'
     if txOutValueToEntropic v >= selectEntropic minUTxO
     then Right ()
     else Left $ TxBodyErrorMinUTxONotMet
                   (txOutInAnyEra txout)
                   (selectEntropic minUTxO)

substituteExecutionUnits :: Map ScriptWitnessIndex ExecutionUnits
                         -> TxBodyContent BuildTx era
                         -> TxBodyContent BuildTx era
substituteExecutionUnits exUnitsMap =
    mapTxScriptWitnesses f
  where
    f :: ScriptWitnessIndex
      -> ScriptWitness witctx era
      -> ScriptWitness witctx era
    f _   wit@SimpleScriptWitness{} = wit
    f idx wit@(ZerepochScriptWitness langInEra version script datum redeemer _) =
      case Map.lookup idx exUnitsMap of
        Nothing      -> wit
        Just exunits -> ZerepochScriptWitness langInEra version script
                                            datum redeemer exunits

calculateMinimumUTxO
  :: SophieBasedEra era
  -> TxOut era
  -> ProtocolParameters
  -> Either MinimumUTxOError Value
calculateMinimumUTxO era txout@(TxOut _ v _) pparams' =
  case era of
    SophieBasedEraSophie -> entropicToValue <$> getMinUTxOPreAurum pparams'
    SophieBasedEraEvie -> calcMinUTxOEvieJen
    SophieBasedEraJen -> calcMinUTxOEvieJen
    SophieBasedEraAurum ->
      case protocolParamUTxOCostPerWord pparams' of
        Just (Entropic costPerWord) -> do
          Right . entropicToValue
            $ Entropic (Aurum.utxoEntrySize (toSophieTxOut era txout) * costPerWord)
        Nothing -> Left PParamsUTxOCostPerWordMissing
 where
   calcMinUTxOEvieJen :: Either MinimumUTxOError Value
   calcMinUTxOEvieJen = do
     let val = txOutValueToValue v
     minUTxO <- getMinUTxOPreAurum pparams'
     Right . entropicToValue $ calcMinimumDeposit val minUTxO

   getMinUTxOPreAurum
     :: ProtocolParameters -> Either MinimumUTxOError Entropic
   getMinUTxOPreAurum =
     maybe (Left PParamsMinUTxOMissing) Right . protocolParamMinUTxOValue

data MinimumUTxOError =
    PParamsMinUTxOMissing
  | PParamsUTxOCostPerWordMissing
  deriving Show

instance Error MinimumUTxOError where
  displayError PParamsMinUTxOMissing =
    "\"minUtxoValue\" field not present in protocol parameters when \
    \trying to calculate minimum UTxO value."
  displayError PParamsUTxOCostPerWordMissing =
    "\"utxoCostPerWord\" field not present in protocol parameters when \
    \trying to calculate minimum UTxO value."
