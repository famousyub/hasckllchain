{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Bcc.CLI.Sophie.Run.Transaction
  ( SophieTxCmdError
  , renderSophieTxCmdError
  , runTransactionCmd
  ) where

import           Bcc.Prelude hiding (All, Any)
import           Prelude (String, error)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.List (intersect, (\\))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Data.Type.Equality (TestEquality (..))

import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither,
                   hoistMaybe, left, newExceptT)

import           Bcc.Api
import           Bcc.Api.Cole hiding (SomeColeSigningKey (..))
import           Bcc.Api.Sophie
import           Shardagnostic.Consensus.Sophie.Eras (StandardEvie, StandardJen, StandardSophie)

--TODO: do this nicely via the API too:
import qualified Bcc.Binary as CBOR

--TODO: following import needed for orphan Eq Script instance
import           Bcc.Ledger.SophieMA.TxBody ()
import           Sophie.Spec.Ledger.Scripts ()

import           Bcc.CLI.Environment (EnvSocketError, readEnvSocketPath, renderEnvSocketError)
import           Bcc.CLI.Run.Friendly (friendlyTxBodyBS)
import           Bcc.CLI.Sophie.Key (InputDecodeError, readSigningKeyFileAnyOf)
import           Bcc.CLI.Sophie.Parsers
import           Bcc.CLI.Sophie.Run.Genesis (SophieGenesisCmdError (..), readSophieGenesis,
                   renderSophieGenesisCmdError)
import           Bcc.CLI.Sophie.Run.Query (SophieQueryCmdLocalStateQueryError (..),
                   renderLocalStateQueryError)
import           Bcc.CLI.Sophie.Script
import           Bcc.CLI.Types
import           Shardagnostic.Consensus.Cole.Ledger (ColeBlock)
import           Shardagnostic.Consensus.Bcc.Block (EraMismatch (..))
import           Shardagnostic.Consensus.Ledger.SupportsMempool (ApplyTxErr)
import           Shardagnostic.Consensus.Sophie.Ledger (SophieBlock)
import           Shardagnostic.Network.Protocol.LocalStateQuery.Type (AcquireFailure (..))
import qualified Shardagnostic.Network.Protocol.LocalTxSubmission.Client as Net.Tx

import qualified System.IO as IO

{- HLINT ignore "Use let" -}

data SophieTxCmdError
  = SophieTxCmdAesonDecodeProtocolParamsError !FilePath !Text
  | SophieTxCmdReadFileError !(FileError ())
  | SophieTxCmdScriptFileError (FileError ScriptDecodeError)
  | SophieTxCmdReadTextViewFileError !(FileError TextEnvelopeError)
  | SophieTxCmdReadWitnessSigningDataError !ReadWitnessSigningDataError
  | SophieTxCmdWriteFileError !(FileError ())
  | SophieTxCmdEraConsensusModeMismatch
      !(Maybe FilePath)
      !AnyConsensusMode
      !AnyBccEra
      -- ^ Era
  | SophieTxCmdMetadataJsonParseError !FilePath !String
  | SophieTxCmdMetadataConversionError !FilePath !TxMetadataJsonError
  | SophieTxCmdMetaValidationError !FilePath ![(Word64, TxMetadataRangeError)]
  | SophieTxCmdScriptDataJsonParseError  !FilePath !String
  | SophieTxCmdScriptDataConversionError !FilePath !ScriptDataJsonError
  | SophieTxCmdScriptDataValidationError !FilePath !ScriptDataRangeError
  | SophieTxCmdMetaDecodeError !FilePath !CBOR.DecoderError
  | SophieTxCmdBootstrapWitnessError !SophieBootstrapWitnessError
  | SophieTxCmdSocketEnvError !EnvSocketError
  | SophieTxCmdTxSubmitError !Text
  | SophieTxCmdTxSubmitErrorCole !(ApplyTxErr ColeBlock)
  | SophieTxCmdTxSubmitErrorSophie !(ApplyTxErr (SophieBlock StandardSophie))
  | SophieTxCmdTxSubmitErrorEvie !(ApplyTxErr (SophieBlock StandardEvie))
  | SophieTxCmdTxSubmitErrorJen !(ApplyTxErr (SophieBlock StandardJen))
  | SophieTxCmdTxSubmitErrorEraMismatch !EraMismatch
  | SophieTxCmdTxFeatureMismatch !AnyBccEra !TxFeature
  | SophieTxCmdTxBodyError !TxBodyError
  | SophieTxCmdNotImplemented !Text
  | SophieTxCmdWitnessEraMismatch !AnyBccEra !AnyBccEra !WitnessFile
  | SophieTxCmdScriptLanguageNotSupportedInEra !AnyScriptLanguage !AnyBccEra
  | SophieTxCmdScriptExpectedSimple !FilePath !AnyScriptLanguage
  | SophieTxCmdScriptExpectedZerepoch !FilePath !AnyScriptLanguage
  | SophieTxCmdGenesisCmdError !SophieGenesisCmdError
  | SophieTxCmdPolicyIdsMissing ![PolicyId]
  | SophieTxCmdPolicyIdsExcess  ![PolicyId]
  | SophieTxCmdAcquireFailure !AcquireFailure
  | SophieTxCmdUnsupportedMode !AnyConsensusMode
  | SophieTxCmdColeEra
  | SophieTxCmdEraConsensusModeMismatchTxBalance
      !TxBodyFile
      !AnyConsensusMode
      !AnyBccEra
  | SophieTxCmdBalanceTxBody !TxBodyErrorAutoBalance
  | SophieTxCmdEraConsensusModeMismatchQuery !AnyConsensusMode !AnyBccEra
  | SophieTxCmdColeEraQuery
  | SophieTxCmdLocalStateQueryError !SophieQueryCmdLocalStateQueryError
  | SophieTxCmdExpectedKeyLockedTxIn ![TxIn]
  | SophieTxCmdTxInsDoNotExist ![TxIn]
  | SophieTxCmdMinimumUTxOErr !MinimumUTxOError
  | SophieTxCmdPParamsErr !ProtocolParametersError
  deriving Show


renderSophieTxCmdError :: SophieTxCmdError -> Text
renderSophieTxCmdError err =
  case err of
    SophieTxCmdReadFileError fileErr -> Text.pack (displayError fileErr)
    SophieTxCmdReadTextViewFileError fileErr -> Text.pack (displayError fileErr)
    SophieTxCmdScriptFileError fileErr -> Text.pack (displayError fileErr)
    SophieTxCmdReadWitnessSigningDataError witSignDataErr ->
      renderReadWitnessSigningDataError witSignDataErr
    SophieTxCmdWriteFileError fileErr -> Text.pack (displayError fileErr)
    SophieTxCmdMetadataJsonParseError fp jsonErr ->
       "Invalid JSON format in file: " <> show fp
                <> "\nJSON parse error: " <> Text.pack jsonErr
    SophieTxCmdMetadataConversionError fp metadataErr ->
       "Error reading metadata at: " <> show fp
                             <> "\n" <> Text.pack (displayError metadataErr)
    SophieTxCmdMetaDecodeError fp metadataErr ->
       "Error decoding CBOR metadata at: " <> show fp
                             <> " Error: " <> show metadataErr
    SophieTxCmdMetaValidationError fp errs ->
      "Error validating transaction metadata at: " <> show fp <> "\n" <>
      Text.intercalate "\n"
        [ "key " <> show k <> ":" <> Text.pack (displayError valErr)
        | (k, valErr) <- errs ]

    SophieTxCmdScriptDataJsonParseError  fp jsonErr ->
       "Invalid JSON format in file: " <> show fp <>
       "\nJSON parse error: " <> Text.pack jsonErr
    SophieTxCmdScriptDataConversionError fp cerr ->
       "Error reading metadata at: " <> show fp
                             <> "\n" <> Text.pack (displayError cerr)
    SophieTxCmdScriptDataValidationError fp verr ->
      "Error validating script data at: " <> show fp <> ":\n" <>
      Text.pack (displayError verr)

    SophieTxCmdSocketEnvError envSockErr -> renderEnvSocketError envSockErr
    SophieTxCmdAesonDecodeProtocolParamsError fp decErr ->
      "Error while decoding the protocol parameters at: " <> show fp
                                            <> " Error: " <> show decErr
    SophieTxCmdTxSubmitError res -> "Error while submitting tx: " <> res
    SophieTxCmdTxSubmitErrorCole res ->
      "Error while submitting tx: " <> Text.pack (show res)
    SophieTxCmdTxSubmitErrorSophie res ->
      "Error while submitting tx: " <> Text.pack (show res)
    SophieTxCmdTxSubmitErrorEvie res ->
      "Error while submitting tx: " <> Text.pack (show res)
    SophieTxCmdTxSubmitErrorJen res ->
      "Error while submitting tx: " <> Text.pack (show res)
    SophieTxCmdTxSubmitErrorEraMismatch EraMismatch{ledgerEraName, otherEraName} ->
      "The era of the node and the tx do not match. " <>
      "The node is running in the " <> ledgerEraName <>
      " era, but the transaction is for the " <> otherEraName <> " era."
    SophieTxCmdBootstrapWitnessError sbwErr ->
      renderSophieBootstrapWitnessError sbwErr

    SophieTxCmdTxFeatureMismatch era TxFeatureImplicitFees ->
      "An explicit transaction fee must be specified for " <>
      renderEra era <> " era transactions."

    SophieTxCmdTxFeatureMismatch (AnyBccEra SophieEra)
                                  TxFeatureValidityNoUpperBound ->
      "A TTL must be specified for Sophie era transactions."

    SophieTxCmdTxFeatureMismatch era feature ->
      renderFeature feature <> " cannot be used for " <> renderEra era <>
      " era transactions."

    SophieTxCmdTxBodyError err' ->
      "Transaction validaton error: " <> Text.pack (displayError err')

    SophieTxCmdNotImplemented msg ->
      "Feature not yet implemented: " <> msg

    SophieTxCmdWitnessEraMismatch era era' (WitnessFile file) ->
      "The era of a witness does not match the era of the transaction. " <>
      "The transaction is for the " <> renderEra era <> " era, but the " <>
      "witness in " <> show file <> " is for the " <> renderEra era' <> " era."

    SophieTxCmdScriptLanguageNotSupportedInEra (AnyScriptLanguage lang) era ->
      "The script language " <> show lang <> " is not supported in the " <>
      renderEra era <> " era."

    SophieTxCmdScriptExpectedSimple file (AnyScriptLanguage lang) ->
      Text.pack file <> ": expected a script in the simple script language, " <>
      "but it is actually using " <> show lang <> ". Alternatively, to use " <>
      "a Zerepoch script, you must also specify the redeemer " <>
      "(datum if appropriate) and script execution units."

    SophieTxCmdScriptExpectedZerepoch file (AnyScriptLanguage lang) ->
      Text.pack file <> ": expected a script in the Zerepoch script language, " <>
      "but it is actually using " <> show lang <> "."

    SophieTxCmdEraConsensusModeMismatch fp mode era ->
       "Submitting " <> renderEra era <> " era transaction (" <> show fp <>
       ") is not supported in the " <> renderMode mode <> " consensus mode."
    SophieTxCmdGenesisCmdError e -> renderSophieGenesisCmdError e
    SophieTxCmdPolicyIdsMissing policyids ->
      "The \"--mint\" flag specifies an asset with a policy Id, but no \
      \corresponding monetary policy script has been provided as a witness \
      \(via the \"--minting-script-file\" flag). The policy Id in question is: "
      <> Text.intercalate ", " (map serialiseToRawBytesHexText policyids)

    SophieTxCmdPolicyIdsExcess policyids ->
      "A script provided to witness minting does not correspond to the policy \
      \id of any asset specified in the \"--mint\" field. The script hash is: "
      <> Text.intercalate ", " (map serialiseToRawBytesHexText policyids)
    SophieTxCmdAcquireFailure acquireFail -> Text.pack $ show acquireFail
    SophieTxCmdUnsupportedMode mode -> "Unsupported mode: " <> renderMode mode
    SophieTxCmdColeEra -> "This query cannot be used for the Cole era"
    SophieTxCmdEraConsensusModeMismatchTxBalance fp mode era ->
       "Cannot balance " <> renderEra era <> " era transaction body (" <> show fp <>
       ") because is not supported in the " <> renderMode mode <> " consensus mode."
    SophieTxCmdEraConsensusModeMismatchQuery (AnyConsensusMode cMode) (AnyBccEra era) ->
      "Consensus mode and era mismatch. Consensus mode: " <> show cMode <>
      " Era: " <> show era
    SophieTxCmdColeEraQuery -> "Query not available in Cole era"
    SophieTxCmdLocalStateQueryError err' -> renderLocalStateQueryError err'
    SophieTxCmdBalanceTxBody err' -> Text.pack $ displayError err'
    SophieTxCmdExpectedKeyLockedTxIn txins ->
      "Expected key witnessed collateral tx inputs but got script witnessed tx inputs: " <>
      Text.singleton '\n' <>
      Text.intercalate (Text.singleton '\n') (map renderTxIn txins)
    SophieTxCmdTxInsDoNotExist txins ->
      "The following tx input(s) were not present in the UTxO: " <>
      Text.singleton '\n' <>
      Text.intercalate (Text.singleton '\n') (map renderTxIn txins)
    SophieTxCmdMinimumUTxOErr err' -> Text.pack $ displayError err'
    SophieTxCmdPParamsErr err' -> Text.pack $ displayError err'

renderEra :: AnyBccEra -> Text
renderEra (AnyBccEra ColeEra)   = "Cole"
renderEra (AnyBccEra SophieEra) = "Sophie"
renderEra (AnyBccEra EvieEra) = "Evie"
renderEra (AnyBccEra JenEra)    = "Jen"
renderEra (AnyBccEra AurumEra)  = "Aurum"

renderFeature :: TxFeature -> Text
renderFeature TxFeatureSophieAddresses     = "Sophie addresses"
renderFeature TxFeatureExplicitFees         = "Explicit fees"
renderFeature TxFeatureImplicitFees         = "Implicit fees"
renderFeature TxFeatureValidityLowerBound   = "A validity lower bound"
renderFeature TxFeatureValidityUpperBound   = "A validity upper bound"
renderFeature TxFeatureValidityNoUpperBound = "An absent validity upper bound"
renderFeature TxFeatureTxMetadata           = "Transaction metadata"
renderFeature TxFeatureAuxScripts           = "Auxiliary scripts"
renderFeature TxFeatureWithdrawals          = "Reward account withdrawals"
renderFeature TxFeatureCertificates         = "Certificates"
renderFeature TxFeatureMintValue            = "Asset minting"
renderFeature TxFeatureMultiAssetOutputs    = "Multi-Asset outputs"
renderFeature TxFeatureScriptWitnesses      = "Script witnesses"
renderFeature TxFeatureSophieKeys          = "Sophie keys"
renderFeature TxFeatureCollateral           = "Collateral inputs"
renderFeature TxFeatureProtocolParameters   = "Protocol parameters"
renderFeature TxFeatureTxOutDatum           = "Transaction output datums"
renderFeature TxFeatureScriptValidity       = "Script validity"
renderFeature TxFeatureExtraKeyWits         = "Required signers"

runTransactionCmd :: TransactionCmd -> ExceptT SophieTxCmdError IO ()
runTransactionCmd cmd =
  case cmd of
    TxBuild era consensusModeParams nid mScriptValidity mOverrideWits txins reqSigners
            txinsc txouts changeAddr mValue mLowBound mUpperBound certs wdrls metadataSchema
            scriptFiles metadataFiles mpparams mUpProp out ->
      runTxBuild era consensusModeParams nid mScriptValidity txins txinsc txouts changeAddr mValue mLowBound
                 mUpperBound certs wdrls reqSigners metadataSchema scriptFiles
                 metadataFiles mpparams mUpProp out mOverrideWits
    TxBuildRaw era mScriptValidity txins txinsc reqSigners txouts mValue mLowBound mUpperBound
               fee certs wdrls metadataSchema scriptFiles
               metadataFiles mpparams mUpProp out ->
      runTxBuildRaw era mScriptValidity txins txinsc txouts mLowBound mUpperBound
                    fee mValue certs wdrls reqSigners metadataSchema
                    scriptFiles metadataFiles mpparams mUpProp out
    TxSign txinfile skfiles network txoutfile ->
      runTxSign txinfile skfiles network txoutfile
    TxSubmit anyConensusModeParams network txFp ->
      runTxSubmit anyConensusModeParams network txFp
    TxCalculateMinFee txbody mnw pGenesisOrParamsFile nInputs nOutputs
                      nSophieKeyWitnesses nColeKeyWitnesses ->
      runTxCalculateMinFee txbody mnw pGenesisOrParamsFile nInputs nOutputs
                           nSophieKeyWitnesses nColeKeyWitnesses
    TxCalculateMinRequiredUTxO era pParamSpec txOuts -> runTxCalculateMinRequiredUTxO era pParamSpec txOuts
    TxHashScriptData scriptDataOrFile -> runTxHashScriptData scriptDataOrFile
    TxGetTxId txinfile -> runTxGetTxId txinfile
    TxView txinfile -> runTxView txinfile
    TxMintedPolicyId sFile -> runTxCreatePolicyId sFile
    TxCreateWitness txBodyfile witSignData mbNw outFile ->
      runTxCreateWitness txBodyfile witSignData mbNw outFile
    TxAssembleTxBodyWitness txBodyFile witnessFile outFile ->
      runTxSignWitness txBodyFile witnessFile outFile

-- ----------------------------------------------------------------------------
-- Building transactions
--

runTxBuildRaw
  :: AnyBccEra
  -> Maybe ScriptValidity
  -- ^ Mark script as expected to pass or fail validation
  -> [(TxIn, Maybe (ScriptWitnessFiles WitCtxTxIn))]
  -- ^ TxIn with potential script witness
  -> [TxIn]
  -- ^ TxIn for collateral
  -> [TxOutAnyEra]
  -> Maybe SlotNo
  -- ^ Tx lower bound
  -> Maybe SlotNo
  -- ^ Tx upper bound
  -> Maybe Entropic
  -- ^ Tx fee
  -> Maybe (Value, [ScriptWitnessFiles WitCtxMint])
  -- ^ Multi-Asset value(s)
  -> [(CertificateFile, Maybe (ScriptWitnessFiles WitCtxStake))]
  -- ^ Certificate with potential script witness
  -> [(StakeAddress, Entropic, Maybe (ScriptWitnessFiles WitCtxStake))]
  -> [WitnessSigningData]
  -- ^ Required signers
  -> TxMetadataJsonSchema
  -> [ScriptFile]
  -> [MetadataFile]
  -> Maybe ProtocolParamsSourceSpec
  -> Maybe UpdateProposalFile
  -> TxBodyFile
  -> ExceptT SophieTxCmdError IO ()
runTxBuildRaw (AnyBccEra era)
              mScriptValidity inputsAndScripts inputsCollateral txouts
              mLowerBound mUpperBound
              mFee mValue
              certFiles withdrawals reqSigners
              metadataSchema scriptFiles
              metadataFiles mpparams mUpdatePropFile
              (TxBodyFile fpath) = do
    txBodyContent <-
      TxBodyContent
        <$> validateTxIns  era inputsAndScripts
        <*> validateTxInsCollateral
                           era inputsCollateral
        <*> validateTxOuts era txouts
        <*> validateTxFee  era mFee
        <*> ((,) <$> validateTxValidityLowerBound era mLowerBound
                 <*> validateTxValidityUpperBound era mUpperBound)
        <*> validateTxMetadataInEra  era metadataSchema metadataFiles
        <*> validateTxAuxScripts     era scriptFiles
        <*> pure (BuildTxWith TxExtraScriptDataNone) --TODO aurum: support this
        <*> validateRequiredSigners  era reqSigners
        <*> validateProtocolParameters era mpparams
        <*> validateTxWithdrawals    era withdrawals
        <*> validateTxCertificates   era certFiles
        <*> validateTxUpdateProposal era mUpdatePropFile
        <*> validateTxMintValue      era mValue
        <*> validateTxScriptValidity era mScriptValidity

    txBody <-
      firstExceptT SophieTxCmdTxBodyError . hoistEither $
        makeTransactionBody txBodyContent

    firstExceptT SophieTxCmdWriteFileError . newExceptT $
      writeFileTextEnvelope fpath Nothing txBody

runTxBuild
  :: AnyBccEra
  -> AnyConsensusModeParams
  -> NetworkId
  -> Maybe ScriptValidity
  -- ^ Mark script as expected to pass or fail validation
  -> [(TxIn, Maybe (ScriptWitnessFiles WitCtxTxIn))]
  -- ^ TxIn with potential script witness
  -> [TxIn]
  -- ^ TxIn for collateral
  -> [TxOutAnyEra]
  -- ^ Normal outputs
  -> TxOutChangeAddress
  -- ^ A change output
  -> Maybe (Value, [ScriptWitnessFiles WitCtxMint])
  -- ^ Multi-Asset value(s)
  -> Maybe SlotNo
  -- ^ Tx lower bound
  -> Maybe SlotNo
  -- ^ Tx upper bound
  -> [(CertificateFile, Maybe (ScriptWitnessFiles WitCtxStake))]
  -- ^ Certificate with potential script witness
  -> [(StakeAddress, Entropic, Maybe (ScriptWitnessFiles WitCtxStake))]
  -> [WitnessSigningData]
  -- ^ Required signers
  -> TxMetadataJsonSchema
  -> [ScriptFile]
  -> [MetadataFile]
  -> Maybe ProtocolParamsSourceSpec
  -> Maybe UpdateProposalFile
  -> TxBodyFile
  -> Maybe Word
  -> ExceptT SophieTxCmdError IO ()
runTxBuild (AnyBccEra era) (AnyConsensusModeParams cModeParams) networkId mScriptValidity txins txinsc txouts
           (TxOutChangeAddress changeAddr) mValue mLowerBound mUpperBound certFiles withdrawals reqSigners
           metadataSchema scriptFiles metadataFiles mpparams mUpdatePropFile outBody@(TxBodyFile fpath)
           mOverrideWits = do
  SocketPath sockPath <- firstExceptT SophieTxCmdSocketEnvError readEnvSocketPath

  let localNodeConnInfo = LocalNodeConnectInfo cModeParams networkId sockPath
      consensusMode = consensusModeOnly cModeParams
      dummyFee = Just $ Entropic 0
      onlyInputs = [input | (input,_) <- txins]

  case (consensusMode, bccEraStyle era) of
    (BccMode, SophieBasedEra sbe) -> do
      txBodyContent <-
        TxBodyContent
          <$> validateTxIns               era txins
          <*> validateTxInsCollateral     era txinsc
          <*> validateTxOuts              era txouts
          <*> validateTxFee               era dummyFee
          <*> ((,) <$> validateTxValidityLowerBound era mLowerBound
                   <*> validateTxValidityUpperBound era mUpperBound)
          <*> validateTxMetadataInEra     era metadataSchema metadataFiles
          <*> validateTxAuxScripts        era scriptFiles
          <*> pure (BuildTxWith TxExtraScriptDataNone) --TODO aurum: support this
          <*> validateRequiredSigners     era reqSigners
          <*> validateProtocolParameters  era mpparams
          <*> validateTxWithdrawals       era withdrawals
          <*> validateTxCertificates      era certFiles
          <*> validateTxUpdateProposal    era mUpdatePropFile
          <*> validateTxMintValue         era mValue
          <*> validateTxScriptValidity    era mScriptValidity

      eInMode <- case toEraInMode era BccMode of
                   Just result -> return result
                   Nothing ->
                     left (SophieTxCmdEraConsensusModeMismatchTxBalance outBody
                            (AnyConsensusMode BccMode) (AnyBccEra era))

      (utxo, pparams, eraHistory, systemStart, stakePools) <-
        newExceptT . fmap (join . first SophieTxCmdAcquireFailure) $
          executeLocalStateQueryExpr localNodeConnInfo Nothing $ \_ntcVersion -> runExceptT $ do
            unless (null txinsc) $ do
              collateralUtxo <- firstExceptT SophieTxCmdTxSubmitErrorEraMismatch . newExceptT . queryExpr
                $ QueryInEra eInMode
                $ QueryInSophieBasedEra sbe (QueryUTxO . QueryUTxOByTxIn $ Set.fromList txinsc)
              txinsExist txinsc collateralUtxo
              notScriptLockedTxIns collateralUtxo

            utxo <- firstExceptT SophieTxCmdTxSubmitErrorEraMismatch . newExceptT . queryExpr
              $ QueryInEra eInMode $ QueryInSophieBasedEra sbe
              $ QueryUTxO (QueryUTxOByTxIn (Set.fromList onlyInputs))

            txinsExist onlyInputs utxo

            pparams <- firstExceptT SophieTxCmdTxSubmitErrorEraMismatch . newExceptT . queryExpr
              $ QueryInEra eInMode $ QueryInSophieBasedEra sbe QueryProtocolParameters

            eraHistory <- lift . queryExpr $ QueryEraHistory BccModeIsMultiEra

            systemStart <- lift $ queryExpr QuerySystemStart


            stakePools <- firstExceptT SophieTxCmdTxSubmitErrorEraMismatch . ExceptT $
              queryExpr . QueryInEra eInMode . QueryInSophieBasedEra sbe $ QueryStakePools

            return (utxo, pparams, eraHistory, systemStart, stakePools)

      let cAddr = case anyAddressInEra era changeAddr of
                    Just addr -> addr
                    Nothing -> error $ "runTxBuild: Cole address used: " <> show changeAddr

      (BalancedTxBody balancedTxBody _ fee) <-
        firstExceptT SophieTxCmdBalanceTxBody
          . hoistEither
          $ makeTransactionBodyAutoBalance eInMode systemStart eraHistory
                                           pparams stakePools utxo txBodyContent
                                           cAddr mOverrideWits

      putStrLn $ "Estimated transaction fee: " <> (show fee :: String)

      firstExceptT SophieTxCmdWriteFileError . newExceptT
        $ writeFileTextEnvelope fpath Nothing balancedTxBody

    (BccMode, LegacyColeEra) -> left SophieTxCmdColeEra

    (wrongMode, _) -> left (SophieTxCmdUnsupportedMode (AnyConsensusMode wrongMode))
  where
    txinsExist :: Monad m => [TxIn] -> UTxO era -> ExceptT SophieTxCmdError m ()
    txinsExist ins (UTxO utxo)
      | null utxo = left $ SophieTxCmdTxInsDoNotExist ins
      | otherwise = do
          let utxoIns = Map.keys utxo
              occursInUtxo = [ txin | txin <- ins, txin `elem` utxoIns ]
          if length occursInUtxo == length ins
          then return ()
          else left . SophieTxCmdTxInsDoNotExist $ ins \\ ins `intersect` occursInUtxo

    notScriptLockedTxIns :: Monad m => UTxO era -> ExceptT SophieTxCmdError m ()
    notScriptLockedTxIns (UTxO utxo) = do
      let scriptLockedTxIns =
            filter (\(_, TxOut aInEra _ _) -> not $ isKeyAddress aInEra ) $ Map.assocs utxo
      if null scriptLockedTxIns
      then return ()
      else left . SophieTxCmdExpectedKeyLockedTxIn $ map fst scriptLockedTxIns

-- ----------------------------------------------------------------------------
-- Transaction body validation and conversion
--

-- | An enumeration of era-dependent features where we have to check that it
-- is permissible to use this feature in this era.
--
data TxFeature = TxFeatureSophieAddresses
               | TxFeatureExplicitFees
               | TxFeatureImplicitFees
               | TxFeatureValidityLowerBound
               | TxFeatureValidityUpperBound
               | TxFeatureValidityNoUpperBound
               | TxFeatureTxMetadata
               | TxFeatureAuxScripts
               | TxFeatureWithdrawals
               | TxFeatureCertificates
               | TxFeatureMintValue
               | TxFeatureMultiAssetOutputs
               | TxFeatureScriptWitnesses
               | TxFeatureSophieKeys
               | TxFeatureCollateral
               | TxFeatureProtocolParameters
               | TxFeatureTxOutDatum
               | TxFeatureScriptValidity
               | TxFeatureExtraKeyWits
  deriving Show

txFeatureMismatch :: BccEra era
                  -> TxFeature
                  -> ExceptT SophieTxCmdError IO a
txFeatureMismatch era feature =
    left (SophieTxCmdTxFeatureMismatch (anyBccEra era) feature)

validateTxIns
  :: forall era.
     BccEra era
  -> [(TxIn, Maybe (ScriptWitnessFiles WitCtxTxIn))]
  -> ExceptT SophieTxCmdError IO
             [(TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era))]
validateTxIns era = mapM convert
 where
   convert
     :: (TxIn, Maybe (ScriptWitnessFiles WitCtxTxIn))
     -> ExceptT SophieTxCmdError IO
                (TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era))
   convert (txin, mScriptWitnessFiles) =
     case mScriptWitnessFiles of
       Just scriptWitnessFiles -> do
         sWit <- createScriptWitness era scriptWitnessFiles
         return ( txin
                , BuildTxWith $ ScriptWitness ScriptWitnessForSpending sWit
                )
       Nothing -> return (txin, BuildTxWith $ KeyWitness KeyWitnessForSpending)


validateTxInsCollateral :: BccEra era
                        -> [TxIn]
                        -> ExceptT SophieTxCmdError IO (TxInsCollateral era)
validateTxInsCollateral _   []    = return TxInsCollateralNone
validateTxInsCollateral era txins =
    case collateralSupportedInEra era of
      Nothing -> txFeatureMismatch era TxFeatureCollateral
      Just supported -> return (TxInsCollateral supported txins)


validateTxOuts :: forall era.
                  BccEra era
               -> [TxOutAnyEra]
               -> ExceptT SophieTxCmdError IO [TxOut era]
validateTxOuts era = mapM (toTxOutInAnyEra era)

toAddressInAnyEra
  :: BccEra era
  -> AddressAny
  -> ExceptT SophieTxCmdError IO (AddressInEra era)
toAddressInAnyEra era addrAny =
  case addrAny of
    AddressCole   bAddr -> return (AddressInEra ColeAddressInAnyEra bAddr)
    AddressSophie sAddr ->
      case bccEraStyle era of
        LegacyColeEra -> txFeatureMismatch era TxFeatureSophieAddresses
        SophieBasedEra era' ->
          return (AddressInEra (SophieAddressInEra era') sAddr)

toTxOutValueInAnyEra
  :: BccEra era
  -> Value
  -> ExceptT SophieTxCmdError IO (TxOutValue era)
toTxOutValueInAnyEra era val =
  case multiAssetSupportedInEra era of
    Left adaOnlyInEra ->
      case valueToEntropic val of
        Just l  -> return (TxOutBccOnly adaOnlyInEra l)
        Nothing -> txFeatureMismatch era TxFeatureMultiAssetOutputs
    Right multiAssetInEra -> return (TxOutValue multiAssetInEra val)

toTxOutInAnyEra :: BccEra era
                -> TxOutAnyEra
                -> ExceptT SophieTxCmdError IO (TxOut era)
toTxOutInAnyEra era (TxOutAnyEra addr val mDatumHash) =
  case (scriptDataSupportedInEra era, mDatumHash) of
    (_, Nothing) ->
      TxOut <$> toAddressInAnyEra era addr
            <*> toTxOutValueInAnyEra era val
            <*> pure TxOutDatumHashNone
    (Just supported, Just dh) ->
      TxOut <$> toAddressInAnyEra era addr
            <*> toTxOutValueInAnyEra era val
            <*> pure (TxOutDatumHash supported dh)
    (Nothing, Just _) ->
      txFeatureMismatch era TxFeatureTxOutDatum

validateTxFee :: BccEra era
              -> Maybe Entropic
              -> ExceptT SophieTxCmdError IO (TxFee era)
validateTxFee era mfee =
    case (txFeesExplicitInEra era, mfee) of
      (Left  implicit, Nothing)  -> return (TxFeeImplicit implicit)
      (Right explicit, Just fee) -> return (TxFeeExplicit explicit fee)

      (Right _, Nothing) -> txFeatureMismatch era TxFeatureImplicitFees
      (Left  _, Just _)  -> txFeatureMismatch era TxFeatureExplicitFees


validateTxValidityLowerBound :: BccEra era
                             -> Maybe SlotNo
                             -> ExceptT SophieTxCmdError IO
                                        (TxValidityLowerBound era)
validateTxValidityLowerBound _ Nothing = return TxValidityNoLowerBound
validateTxValidityLowerBound era (Just slot) =
    case validityLowerBoundSupportedInEra era of
      Nothing -> txFeatureMismatch era TxFeatureValidityLowerBound
      Just supported -> return (TxValidityLowerBound supported slot)


validateTxValidityUpperBound :: BccEra era
                             -> Maybe SlotNo
                             -> ExceptT SophieTxCmdError IO
                                        (TxValidityUpperBound era)
validateTxValidityUpperBound era Nothing =
    case validityNoUpperBoundSupportedInEra era of
      Nothing -> txFeatureMismatch era TxFeatureValidityNoUpperBound
      Just supported -> return (TxValidityNoUpperBound supported)
validateTxValidityUpperBound era (Just slot) =
    case validityUpperBoundSupportedInEra era of
      Nothing -> txFeatureMismatch era TxFeatureValidityUpperBound
      Just supported -> return (TxValidityUpperBound supported slot)


validateTxMetadataInEra :: BccEra era
                        -> TxMetadataJsonSchema
                        -> [MetadataFile]
                        -> ExceptT SophieTxCmdError IO (TxMetadataInEra era)
validateTxMetadataInEra _ _ [] = return TxMetadataNone
validateTxMetadataInEra era schema files =
    case txMetadataSupportedInEra era of
      Nothing -> txFeatureMismatch era TxFeatureTxMetadata
      Just supported -> do
        metadata <- mconcat <$> mapM (readFileTxMetadata schema) files
        return (TxMetadataInEra supported metadata)


validateTxAuxScripts :: BccEra era
                     -> [ScriptFile]
                     -> ExceptT SophieTxCmdError IO (TxAuxScripts era)
validateTxAuxScripts _ [] = return TxAuxScriptsNone
validateTxAuxScripts era files =
  case auxScriptsSupportedInEra era of
    Nothing -> txFeatureMismatch era TxFeatureAuxScripts
    Just supported -> do
      scripts <- sequence
        [ do script <- firstExceptT SophieTxCmdScriptFileError $
                         readFileScriptInAnyLang file
             validateScriptSupportedInEra era script
        | ScriptFile file <- files ]
      return $ TxAuxScripts supported scripts

validateRequiredSigners :: BccEra era
                        -> [WitnessSigningData]
                        -> ExceptT SophieTxCmdError IO (TxExtraKeyWitnesses era)
validateRequiredSigners _ [] = return TxExtraKeyWitnessesNone
validateRequiredSigners era reqSigs =
  case extraKeyWitnessesSupportedInEra era of
    Nothing -> txFeatureMismatch era TxFeatureExtraKeyWits
    Just supported -> do
      keyWits <- firstExceptT SophieTxCmdReadWitnessSigningDataError
                   $ mapM readWitnessSigningData reqSigs
      let (_sksCole, sksSophie) = partitionSomeWitnesses $ map categoriseSomeWitness keyWits
          sophieSigningKeys = map toSophieSigningKey sksSophie
          paymentKeyHashes = map (verificationKeyHash . getVerificationKey) $ mapMaybe excludeExtendedKeys sophieSigningKeys
      return $ TxExtraKeyWitnesses supported paymentKeyHashes
 where
  excludeExtendedKeys :: SophieSigningKey -> Maybe (SigningKey PaymentKey)
  excludeExtendedKeys (SophieExtendedSigningKey _) = Nothing
  excludeExtendedKeys (SophieNormalSigningKey sk) = Just $ PaymentSigningKey sk

validateTxWithdrawals
  :: forall era.
     BccEra era
  -> [(StakeAddress, Entropic, Maybe (ScriptWitnessFiles WitCtxStake))]
  -> ExceptT SophieTxCmdError IO (TxWithdrawals BuildTx era)
validateTxWithdrawals _ [] = return TxWithdrawalsNone
validateTxWithdrawals era withdrawals =
  case withdrawalsSupportedInEra era of
    Nothing -> txFeatureMismatch era TxFeatureWithdrawals
    Just supported -> do
      convWithdrawals <- mapM convert withdrawals
      return (TxWithdrawals supported convWithdrawals)
 where
  convert
    :: (StakeAddress, Entropic, Maybe (ScriptWitnessFiles WitCtxStake))
    -> ExceptT SophieTxCmdError IO
              (StakeAddress,
               Entropic,
               BuildTxWith BuildTx (Witness WitCtxStake era))
  convert (sAddr, ll, mScriptWitnessFiles) =
    case mScriptWitnessFiles of
      Just scriptWitnessFiles -> do
        sWit <- createScriptWitness era scriptWitnessFiles
        return ( sAddr
               , ll
               , BuildTxWith $ ScriptWitness ScriptWitnessForStakeAddr sWit
               )
      Nothing -> return (sAddr,ll, BuildTxWith $ KeyWitness KeyWitnessForStakeAddr)

validateTxCertificates
  :: forall era.
     BccEra era
  -> [(CertificateFile, Maybe (ScriptWitnessFiles WitCtxStake))]
  -> ExceptT SophieTxCmdError IO (TxCertificates BuildTx era)
validateTxCertificates era certFiles =
  case certificatesSupportedInEra era of
    Nothing
      | null certFiles -> return TxCertificatesNone
      | otherwise      -> txFeatureMismatch era TxFeatureCertificates
    Just supported -> do
      certs <- sequence
                 [ firstExceptT SophieTxCmdReadTextViewFileError . newExceptT $
                     readFileTextEnvelope AsCertificate certFile
                 | CertificateFile certFile <- map fst certFiles ]
      reqWits <- Map.fromList . catMaybes  <$> mapM convert certFiles
      return $ TxCertificates supported certs $ BuildTxWith reqWits
  where
   -- We get the stake credential witness for a certificate that requires it.
   -- NB: Only stake address deregistration and delegation requires
   -- witnessing (witness can be script or key)
   deriveStakeCredentialWitness
     :: CertificateFile
     -> ExceptT SophieTxCmdError IO (Maybe StakeCredential)
   deriveStakeCredentialWitness (CertificateFile certFile) = do
     cert <- firstExceptT SophieTxCmdReadTextViewFileError . newExceptT
               $ readFileTextEnvelope AsCertificate certFile
     case cert of
       StakeAddressDeregistrationCertificate sCred -> return $ Just sCred
       StakeAddressDelegationCertificate sCred _ -> return $ Just sCred
       _ -> return Nothing

   convert
     :: (CertificateFile, Maybe (ScriptWitnessFiles WitCtxStake))
     -> ExceptT SophieTxCmdError IO
                (Maybe (StakeCredential, Witness WitCtxStake era))
   convert (cert, mScriptWitnessFiles) = do
     mStakeCred <- deriveStakeCredentialWitness cert
     case mStakeCred of
       Nothing -> return Nothing
       Just sCred ->
         case mScriptWitnessFiles of
           Just scriptWitnessFiles -> do
            sWit <- createScriptWitness era scriptWitnessFiles
            return $ Just ( sCred
                          , ScriptWitness ScriptWitnessForStakeAddr sWit
                          )

           Nothing -> return $ Just (sCred, KeyWitness KeyWitnessForStakeAddr)

validateProtocolParameters
  :: BccEra era
  -> Maybe ProtocolParamsSourceSpec
  -> ExceptT SophieTxCmdError IO
            (BuildTxWith BuildTx (Maybe ProtocolParameters))
validateProtocolParameters _ Nothing = return (BuildTxWith Nothing)
validateProtocolParameters era (Just pparamsspec) =
    case scriptDataSupportedInEra era of
      Nothing -> txFeatureMismatch era TxFeatureProtocolParameters
      Just _  -> BuildTxWith . Just <$>
                   readProtocolParametersSourceSpec pparamsspec

validateTxUpdateProposal :: BccEra era
                         -> Maybe UpdateProposalFile
                         -> ExceptT SophieTxCmdError IO (TxUpdateProposal era)
validateTxUpdateProposal _ Nothing = return TxUpdateProposalNone
validateTxUpdateProposal era (Just (UpdateProposalFile file)) =
    case updateProposalSupportedInEra era of
      Nothing -> txFeatureMismatch era TxFeatureCertificates
      Just supported -> do
         prop <- firstExceptT SophieTxCmdReadTextViewFileError $ newExceptT $
                   readFileTextEnvelope AsUpdateProposal file
         return (TxUpdateProposal supported prop)

validateTxScriptValidity :: forall era.
     BccEra era
  -> Maybe ScriptValidity
  -> ExceptT SophieTxCmdError IO (TxScriptValidity era)
validateTxScriptValidity _ Nothing = pure TxScriptValidityNone
validateTxScriptValidity era (Just scriptValidity) =
  case txScriptValiditySupportedInBccEra era of
    Nothing -> txFeatureMismatch era TxFeatureScriptValidity
    Just supported -> pure $ TxScriptValidity supported scriptValidity

validateTxMintValue :: forall era.
                       BccEra era
                    -> Maybe (Value, [ScriptWitnessFiles WitCtxMint])
                    -> ExceptT SophieTxCmdError IO (TxMintValue BuildTx era)
validateTxMintValue _ Nothing = return TxMintNone
validateTxMintValue era (Just (val, scriptWitnessFiles)) =
    case multiAssetSupportedInEra era of
      Left _ -> txFeatureMismatch era TxFeatureMintValue
      Right supported -> do
        -- The set of policy ids for which we need witnesses:
        let witnessesNeededSet :: Set PolicyId
            witnessesNeededSet =
              Set.fromList [ pid | (AssetId pid _, _) <- valueToList val ]

        -- The set (and map) of policy ids for which we have witnesses:
        witnesses <- mapM (createScriptWitness era) scriptWitnessFiles
        let witnessesProvidedMap :: Map PolicyId (ScriptWitness WitCtxMint era)
            witnessesProvidedMap = Map.fromList
                                     [ (scriptWitnessPolicyId witness, witness)
                                     | witness <- witnesses ]
            witnessesProvidedSet = Map.keysSet witnessesProvidedMap

        -- Check not too many, nor too few:
        validateAllWitnessesProvided   witnessesNeededSet witnessesProvidedSet
        validateNoUnnecessaryWitnesses witnessesNeededSet witnessesProvidedSet

        return (TxMintValue supported val (BuildTxWith witnessesProvidedMap))
 where
    validateAllWitnessesProvided witnessesNeeded witnessesProvided
      | null witnessesMissing = return ()
      | otherwise = left (SophieTxCmdPolicyIdsMissing witnessesMissing)
      where
        witnessesMissing = Set.elems (witnessesNeeded Set.\\ witnessesProvided)

    validateNoUnnecessaryWitnesses witnessesNeeded witnessesProvided
      | null witnessesExtra = return ()
      | otherwise = left (SophieTxCmdPolicyIdsExcess witnessesExtra)
      where
        witnessesExtra = Set.elems (witnessesProvided Set.\\ witnessesNeeded)

scriptWitnessPolicyId :: ScriptWitness witctx era -> PolicyId
scriptWitnessPolicyId witness =
  case scriptWitnessScript witness of
    ScriptInEra _ script -> scriptPolicyId script


createScriptWitness
  :: BccEra era
  -> ScriptWitnessFiles witctx
  -> ExceptT SophieTxCmdError IO (ScriptWitness witctx era)
createScriptWitness era (SimpleScriptWitnessFile (ScriptFile scriptFile)) = do
    script@(ScriptInAnyLang lang _) <- firstExceptT SophieTxCmdScriptFileError $
                                         readFileScriptInAnyLang scriptFile
    ScriptInEra langInEra script'   <- validateScriptSupportedInEra era script
    case script' of
      SimpleScript version sscript ->
        return $ SimpleScriptWitness
                   langInEra version sscript

      -- If the supplied cli flags were for a simple script (i.e. the user did
      -- not supply the datum, redeemer or ex units), but the script file turns
      -- out to be a valid zerepoch script, then we must fail.
      ZerepochScript{} ->
        left $ SophieTxCmdScriptExpectedSimple
                 scriptFile
                 (AnyScriptLanguage lang)

createScriptWitness era (ZerepochScriptWitnessFiles
                          (ScriptFile scriptFile)
                          datumOrFile
                          redeemerOrFile
                          execUnits) = do
    script@(ScriptInAnyLang lang _) <- firstExceptT SophieTxCmdScriptFileError $
                                         readFileScriptInAnyLang scriptFile
    ScriptInEra langInEra script'   <- validateScriptSupportedInEra era script
    case script' of
      ZerepochScript version pscript -> do
        datum    <- readScriptDatumOrFile    datumOrFile
        redeemer <- readScriptRedeemerOrFile redeemerOrFile
        return $ ZerepochScriptWitness
                   langInEra version pscript
                   datum
                   redeemer
                   execUnits

      -- If the supplied cli flags were for a zerepoch script (i.e. the user did
      -- supply the datum, redeemer and ex units), but the script file turns
      -- out to be a valid simple script, then we must fail.
      SimpleScript{} ->
        left $ SophieTxCmdScriptExpectedZerepoch
                 scriptFile
                 (AnyScriptLanguage lang)


readScriptDatumOrFile :: ScriptDatumOrFile witctx
                      -> ExceptT SophieTxCmdError IO (ScriptDatum witctx)
readScriptDatumOrFile (ScriptDatumOrFileForTxIn df) = ScriptDatumForTxIn <$>
                                                        readScriptDataOrFile df
readScriptDatumOrFile NoScriptDatumOrFileForMint    = pure NoScriptDatumForMint
readScriptDatumOrFile NoScriptDatumOrFileForStake   = pure NoScriptDatumForStake

readScriptRedeemerOrFile :: ScriptRedeemerOrFile
                         -> ExceptT SophieTxCmdError IO ScriptRedeemer
readScriptRedeemerOrFile = readScriptDataOrFile

readScriptDataOrFile :: ScriptDataOrFile
                     -> ExceptT SophieTxCmdError IO ScriptData
readScriptDataOrFile (ScriptDataValue d) = return d
readScriptDataOrFile (ScriptDataFile fp) = do
    bs <- handleIOExceptT (SophieTxCmdReadFileError . FileIOError fp) $
            LBS.readFile fp
    v  <- firstExceptT (SophieTxCmdScriptDataJsonParseError fp) $
            hoistEither $
              Aeson.eitherDecode' bs
    sd <- firstExceptT (SophieTxCmdScriptDataConversionError fp) $
            hoistEither $
              scriptDataFromJson ScriptDataJsonDetailedSchema v
    firstExceptT (SophieTxCmdScriptDataValidationError fp) $
      hoistEither $
        validateScriptData sd
    return sd


-- ----------------------------------------------------------------------------
-- Transaction signing
--

runTxSign :: TxBodyFile
          -> [WitnessSigningData]
          -> Maybe NetworkId
          -> TxFile
          -> ExceptT SophieTxCmdError IO ()
runTxSign (TxBodyFile txbodyFile) witSigningData mnw (TxFile txFile) = do
  InAnySophieBasedEra _era txbody <-
        --TODO: in principle we should be able to support Cole era txs too
        onlyInSophieBasedEras "sign for Cole era transactions"
    =<< readFileTxBody txbodyFile

  sks <- firstExceptT SophieTxCmdReadWitnessSigningDataError $
           mapM readWitnessSigningData witSigningData

  let (sksCole, sksSophie) = partitionSomeWitnesses $ map categoriseSomeWitness sks

  -- Cole witnesses require the network ID. This can either be provided
  -- directly or derived from a provided Cole address.
  coleWitnesses <- firstExceptT SophieTxCmdBootstrapWitnessError
    . hoistEither
    $ mkSophieBootstrapWitnesses mnw txbody sksCole

  let sophieKeyWitnesses = map (makeSophieKeyWitness txbody) sksSophie
      tx = makeSignedTransaction (coleWitnesses ++ sophieKeyWitnesses) txbody

  firstExceptT SophieTxCmdWriteFileError . newExceptT $
    writeFileTextEnvelope txFile Nothing tx


-- ----------------------------------------------------------------------------
-- Transaction submission
--


runTxSubmit
  :: AnyConsensusModeParams
  -> NetworkId
  -> FilePath
  -> ExceptT SophieTxCmdError IO ()
runTxSubmit (AnyConsensusModeParams cModeParams) network txFile = do
    SocketPath sockPath <- firstExceptT SophieTxCmdSocketEnvError readEnvSocketPath

    InAnyBccEra era tx <- readFileTx txFile
    let cMode = AnyConsensusMode $ consensusModeOnly cModeParams
    eraInMode <- hoistMaybe
                   (SophieTxCmdEraConsensusModeMismatch (Just txFile) cMode (AnyBccEra era))
                   (toEraInMode era $ consensusModeOnly cModeParams)
    let txInMode = TxInMode tx eraInMode
        localNodeConnInfo = LocalNodeConnectInfo
                              { localConsensusModeParams = cModeParams
                              , localNodeNetworkId = network
                              , localNodeSocketPath = sockPath
                              }

    res <- liftIO $ submitTxToNodeLocal localNodeConnInfo txInMode
    case res of
      Net.Tx.SubmitSuccess -> liftIO $ putTextLn "Transaction successfully submitted."
      Net.Tx.SubmitFail reason ->
        case reason of
          TxValidationErrorInMode err _eraInMode -> left . SophieTxCmdTxSubmitError . Text.pack $ show err
          TxValidationEraMismatch mismatchErr -> left $ SophieTxCmdTxSubmitErrorEraMismatch mismatchErr

-- ----------------------------------------------------------------------------
-- Transaction fee calculation
--

runTxCalculateMinFee
  :: TxBodyFile
  -> Maybe NetworkId
  -> ProtocolParamsSourceSpec
  -> TxInCount
  -> TxOutCount
  -> TxSophieWitnessCount
  -> TxColeWitnessCount
  -> ExceptT SophieTxCmdError IO ()
runTxCalculateMinFee (TxBodyFile txbodyFile) nw protocolParamsSourceSpec
                     (TxInCount nInputs) (TxOutCount nOutputs)
                     (TxSophieWitnessCount nSophieKeyWitnesses)
                     (TxColeWitnessCount nColeKeyWitnesses) = do
    InAnySophieBasedEra _era txbody <-
          --TODO: in principle we should be able to support Cole era txs too
          onlyInSophieBasedEras "calculate-min-fee for Cole era transactions"
      =<< readFileTxBody txbodyFile

    pparams <- readProtocolParametersSourceSpec protocolParamsSourceSpec

    let tx = makeSignedTransaction [] txbody
        Entropic fee = estimateTransactionFee
                             (fromMaybe Mainnet nw)
                             (protocolParamTxFeeFixed pparams)
                             (protocolParamTxFeePerByte pparams)
                             tx
                             nInputs nOutputs
                             nColeKeyWitnesses nSophieKeyWitnesses

    liftIO $ putStrLn $ (show fee :: String) <> " Entropic"

-- ----------------------------------------------------------------------------
-- Transaction fee calculation
--

runTxCalculateMinRequiredUTxO
  :: AnyBccEra
  -> ProtocolParamsSourceSpec
  -> TxOutAnyEra
  -> ExceptT SophieTxCmdError IO ()
runTxCalculateMinRequiredUTxO (AnyBccEra era) protocolParamsSourceSpec txOut = do
  pp <- readProtocolParametersSourceSpec protocolParamsSourceSpec
  out <- toTxOutInAnyEra era txOut
  case bccEraStyle era of
    LegacyColeEra -> error "runTxCalculateMinRequiredUTxO: Cole era not implemented yet"
    SophieBasedEra sbe -> do
      firstExceptT SophieTxCmdPParamsErr . hoistEither
        $ checkProtocolParameters sbe pp
      minValue <- firstExceptT SophieTxCmdMinimumUTxOErr
                    . hoistEither $ calculateMinimumUTxO sbe out pp
      liftIO . IO.print $ selectEntropic minValue

runTxCreatePolicyId :: ScriptFile -> ExceptT SophieTxCmdError IO ()
runTxCreatePolicyId (ScriptFile sFile) = do
  ScriptInAnyLang _ script <- firstExceptT SophieTxCmdScriptFileError $
                                readFileScriptInAnyLang sFile
  liftIO . putTextLn . serialiseToRawBytesHexText $ hashScript script

readProtocolParametersSourceSpec :: ProtocolParamsSourceSpec
                                 -> ExceptT SophieTxCmdError IO
                                            ProtocolParameters
readProtocolParametersSourceSpec (ParamsFromGenesis (GenesisFile f)) =
    fromSophiePParams . sgProtocolParams <$>
      firstExceptT SophieTxCmdGenesisCmdError
        (readSophieGenesis f identity)
readProtocolParametersSourceSpec (ParamsFromFile f) =
    readProtocolParameters f

--TODO: eliminate this and get only the necessary params, and get them in a more
-- helpful way rather than requiring them as a local file.
readProtocolParameters :: ProtocolParamsFile
                       -> ExceptT SophieTxCmdError IO ProtocolParameters
readProtocolParameters (ProtocolParamsFile fpath) = do
  pparams <- handleIOExceptT (SophieTxCmdReadFileError . FileIOError fpath) $ LBS.readFile fpath
  firstExceptT (SophieTxCmdAesonDecodeProtocolParamsError fpath . Text.pack) . hoistEither $
    Aeson.eitherDecode' pparams


-- ----------------------------------------------------------------------------
-- Witness handling
--

data SomeWitness
  = AColeSigningKey           (SigningKey ColeKey) (Maybe (Address ColeAddr))
  | APaymentSigningKey         (SigningKey PaymentKey)
  | APaymentExtendedSigningKey (SigningKey PaymentExtendedKey)
  | AStakeSigningKey           (SigningKey StakeKey)
  | AStakeExtendedSigningKey   (SigningKey StakeExtendedKey)
  | AStakePoolSigningKey       (SigningKey StakePoolKey)
  | AGenesisSigningKey         (SigningKey GenesisKey)
  | AGenesisExtendedSigningKey (SigningKey GenesisExtendedKey)
  | AGenesisDelegateSigningKey (SigningKey GenesisDelegateKey)
  | AGenesisDelegateExtendedSigningKey
                               (SigningKey GenesisDelegateExtendedKey)
  | AGenesisUTxOSigningKey     (SigningKey GenesisUTxOKey)
  | AVestedSigningKey           (SigningKey VestedKey)
  | AVestedExtendedSigningKey   (SigningKey VestedExtendedKey)
  | AVestedDelegateSigningKey   (SigningKey VestedDelegateKey)
  | AVestedDelegateExtendedSigningKey
                               (SigningKey VestedDelegateExtendedKey)
  | AVestedUTxOSigningKey       (SigningKey VestedUTxOKey)


-- | Error reading the data required to construct a key witness.
data ReadWitnessSigningDataError
  = ReadWitnessSigningDataSigningKeyDecodeError !(FileError InputDecodeError)
  | ReadWitnessSigningDataScriptError !(FileError JsonDecodeError)
  | ReadWitnessSigningDataSigningKeyAndAddressMismatch
  -- ^ A Cole address was specified alongside a non-Cole signing key.
  deriving Show

-- | Render an error message for a 'ReadWitnessSigningDataError'.
renderReadWitnessSigningDataError :: ReadWitnessSigningDataError -> Text
renderReadWitnessSigningDataError err =
  case err of
    ReadWitnessSigningDataSigningKeyDecodeError fileErr ->
      "Error reading signing key: " <> Text.pack (displayError fileErr)
    ReadWitnessSigningDataScriptError fileErr ->
      "Error reading script: " <> Text.pack (displayError fileErr)
    ReadWitnessSigningDataSigningKeyAndAddressMismatch ->
      "Only a Cole signing key may be accompanied by a Cole address."

readWitnessSigningData
  :: WitnessSigningData
  -> ExceptT ReadWitnessSigningDataError IO SomeWitness
readWitnessSigningData (KeyWitnessSigningData skFile mbColeAddr) = do
    res <- firstExceptT ReadWitnessSigningDataSigningKeyDecodeError
      . newExceptT
      $ readSigningKeyFileAnyOf bech32FileTypes textEnvFileTypes skFile
    case (res, mbColeAddr) of
      (AColeSigningKey _ _, Just _) -> pure res
      (AColeSigningKey _ _, Nothing) -> pure res
      (_, Nothing) -> pure res
      (_, Just _) ->
        -- A Cole address should only be specified along with a Cole signing key.
        left ReadWitnessSigningDataSigningKeyAndAddressMismatch
  where
    textEnvFileTypes =
      [ FromSomeType (AsSigningKey AsColeKey)
                          (`AColeSigningKey` mbColeAddr)
      , FromSomeType (AsSigningKey AsPaymentKey)
                          APaymentSigningKey
      , FromSomeType (AsSigningKey AsPaymentExtendedKey)
                          APaymentExtendedSigningKey
      , FromSomeType (AsSigningKey AsStakeKey)
                          AStakeSigningKey
      , FromSomeType (AsSigningKey AsStakeExtendedKey)
                          AStakeExtendedSigningKey
      , FromSomeType (AsSigningKey AsStakePoolKey)
                          AStakePoolSigningKey
      , FromSomeType (AsSigningKey AsGenesisKey)
                          AGenesisSigningKey
      , FromSomeType (AsSigningKey AsGenesisExtendedKey)
                          AGenesisExtendedSigningKey
      , FromSomeType (AsSigningKey AsGenesisDelegateKey)
                          AGenesisDelegateSigningKey
      , FromSomeType (AsSigningKey AsGenesisDelegateExtendedKey)
                          AGenesisDelegateExtendedSigningKey
      , FromSomeType (AsSigningKey AsGenesisUTxOKey)
                          AGenesisUTxOSigningKey
      , FromSomeType (AsSigningKey AsVestedKey)
                      AVestedSigningKey
      , FromSomeType (AsSigningKey AsVestedExtendedKey)
                      AVestedExtendedSigningKey
      , FromSomeType (AsSigningKey AsVestedDelegateKey)
                      AVestedDelegateSigningKey
      , FromSomeType (AsSigningKey AsVestedDelegateExtendedKey)
                      AVestedDelegateExtendedSigningKey
      , FromSomeType (AsSigningKey AsVestedUTxOKey)
                      AVestedUTxOSigningKey
      ]

    bech32FileTypes =
      [ FromSomeType (AsSigningKey AsPaymentKey)
                          APaymentSigningKey
      , FromSomeType (AsSigningKey AsPaymentExtendedKey)
                          APaymentExtendedSigningKey
      , FromSomeType (AsSigningKey AsStakeKey)
                          AStakeSigningKey
      , FromSomeType (AsSigningKey AsStakeExtendedKey)
                          AStakeExtendedSigningKey
      , FromSomeType (AsSigningKey AsStakePoolKey)
                          AStakePoolSigningKey
      ]

partitionSomeWitnesses
  :: [ColeOrSophieWitness]
  -> ( [SophieBootstrapWitnessSigningKeyData]
     , [SophieWitnessSigningKey]
     )
partitionSomeWitnesses = reversePartitionedWits . foldl' go mempty
  where
    reversePartitionedWits (bw, skw) =
      (reverse bw, reverse skw)

    go (coleAcc, sophieKeyAcc) coleOrSophieWit =
      case coleOrSophieWit of
        AColeWitness coleWit ->
          (coleWit:coleAcc, sophieKeyAcc)
        ASophieKeyWitness sophieKeyWit ->
          (coleAcc, sophieKeyWit:sophieKeyAcc)


-- | Some kind of Cole or Sophie witness.
data ColeOrSophieWitness
  = AColeWitness !SophieBootstrapWitnessSigningKeyData
  | ASophieKeyWitness !SophieWitnessSigningKey

categoriseSomeWitness :: SomeWitness -> ColeOrSophieWitness
categoriseSomeWitness swsk =
  case swsk of
    AColeSigningKey         sk addr -> AColeWitness (SophieBootstrapWitnessSigningKeyData sk addr)
    APaymentSigningKey         sk      -> ASophieKeyWitness (WitnessPaymentKey         sk)
    APaymentExtendedSigningKey sk      -> ASophieKeyWitness (WitnessPaymentExtendedKey sk)
    AStakeSigningKey           sk      -> ASophieKeyWitness (WitnessStakeKey           sk)
    AStakeExtendedSigningKey   sk      -> ASophieKeyWitness (WitnessStakeExtendedKey   sk)
    AStakePoolSigningKey       sk      -> ASophieKeyWitness (WitnessStakePoolKey       sk)
    AGenesisSigningKey         sk      -> ASophieKeyWitness (WitnessGenesisKey         sk)
    AGenesisExtendedSigningKey sk      -> ASophieKeyWitness (WitnessGenesisExtendedKey sk)
    AGenesisDelegateSigningKey sk      -> ASophieKeyWitness (WitnessGenesisDelegateKey sk)
    AGenesisDelegateExtendedSigningKey sk
                                       -> ASophieKeyWitness (WitnessGenesisDelegateExtendedKey sk)
    AGenesisUTxOSigningKey     sk      -> ASophieKeyWitness (WitnessGenesisUTxOKey       sk)
    AVestedSigningKey           sk      -> ASophieKeyWitness (WitnessVestedKey           sk)
    AVestedExtendedSigningKey   sk      -> ASophieKeyWitness (WitnessVestedExtendedKey   sk)
    AVestedDelegateSigningKey   sk      -> ASophieKeyWitness (WitnessVestedDelegateKey   sk)
    AVestedDelegateExtendedSigningKey sk
                                       -> ASophieKeyWitness (WitnessVestedDelegateExtendedKey sk)
    AVestedUTxOSigningKey       sk      -> ASophieKeyWitness (WitnessVestedUTxOKey       sk)
-- | Data required for constructing a Sophie bootstrap witness.
data SophieBootstrapWitnessSigningKeyData
  = SophieBootstrapWitnessSigningKeyData
      !(SigningKey ColeKey)
      -- ^ Cole signing key.
      !(Maybe (Address ColeAddr))
      -- ^ An optionally specified Cole address.
      --
      -- If specified, both the network ID and derivation path are extracted
      -- from the address and used in the construction of the Cole witness.

-- | Error constructing a Sophie bootstrap witness (i.e. a Cole key witness
-- in the Sophie era).
data SophieBootstrapWitnessError
  = MissingNetworkIdOrColeAddressError
  -- ^ Neither a network ID nor a Cole address were provided to construct the
  -- Sophie bootstrap witness. One or the other is required.
  deriving Show

-- | Render an error message for a 'SophieBootstrapWitnessError'.
renderSophieBootstrapWitnessError :: SophieBootstrapWitnessError -> Text
renderSophieBootstrapWitnessError MissingNetworkIdOrColeAddressError =
  "Transactions witnessed by a Cole signing key must be accompanied by a "
    <> "network ID. Either provide a network ID or provide a Cole "
    <> "address with each Cole signing key (network IDs can be derived "
    <> "from Cole addresses)."

-- | Construct a Sophie bootstrap witness (i.e. a Cole key witness in the
-- Sophie era).
mkSophieBootstrapWitness
  :: IsSophieBasedEra era
  => Maybe NetworkId
  -> TxBody era
  -> SophieBootstrapWitnessSigningKeyData
  -> Either SophieBootstrapWitnessError (KeyWitness era)
mkSophieBootstrapWitness Nothing _ (SophieBootstrapWitnessSigningKeyData _ Nothing) =
  Left MissingNetworkIdOrColeAddressError
mkSophieBootstrapWitness (Just nw) txBody (SophieBootstrapWitnessSigningKeyData skey Nothing) =
  Right $ makeSophieBootstrapWitness (WitnessNetworkId nw) txBody skey
mkSophieBootstrapWitness _ txBody (SophieBootstrapWitnessSigningKeyData skey (Just addr)) =
  Right $ makeSophieBootstrapWitness (WitnessColeAddress addr) txBody skey

-- | Attempt to construct Sophie bootstrap witnesses until an error is
-- encountered.
mkSophieBootstrapWitnesses
  :: IsSophieBasedEra era
  => Maybe NetworkId
  -> TxBody era
  -> [SophieBootstrapWitnessSigningKeyData]
  -> Either SophieBootstrapWitnessError [KeyWitness era]
mkSophieBootstrapWitnesses mnw txBody =
  mapM (mkSophieBootstrapWitness mnw txBody)


-- ----------------------------------------------------------------------------
-- Other misc small commands
--

runTxHashScriptData :: ScriptDataOrFile -> ExceptT SophieTxCmdError IO ()
runTxHashScriptData scriptDataOrFile = do
    d <- readScriptDataOrFile scriptDataOrFile
    liftIO $ BS.putStrLn $ serialiseToRawBytesHex (hashScriptData d)

runTxGetTxId :: InputTxFile -> ExceptT SophieTxCmdError IO ()
runTxGetTxId txfile = do
    InAnyBccEra _era txbody <-
      case txfile of
        InputTxBodyFile (TxBodyFile txbodyFile) -> readFileTxBody txbodyFile
        InputTxFile (TxFile txFile) -> do
          InAnyBccEra era tx <- readFileTx txFile
          return . InAnyBccEra era $ getTxBody tx

    liftIO $ BS.putStrLn $ serialiseToRawBytesHex (getTxId txbody)

runTxView :: InputTxFile -> ExceptT SophieTxCmdError IO ()
runTxView txfile = do
  InAnyBccEra era txbody <-
    case txfile of
      InputTxBodyFile (TxBodyFile txbodyFile) -> readFileTxBody txbodyFile
      InputTxFile (TxFile txFile) -> do
        InAnyBccEra era tx <- readFileTx txFile
        return . InAnyBccEra era $ getTxBody tx
  liftIO $ BS.putStr $ friendlyTxBodyBS era txbody


-- ----------------------------------------------------------------------------
-- Witness commands
--

runTxCreateWitness
  :: TxBodyFile
  -> WitnessSigningData
  -> Maybe NetworkId
  -> OutputFile
  -> ExceptT SophieTxCmdError IO ()
runTxCreateWitness (TxBodyFile txbodyFile) witSignData mbNw (OutputFile oFile) = do

  InAnySophieBasedEra _era txbody <-
        --TODO: in principle we should be able to support Cole era txs too
        onlyInSophieBasedEras "witness for Cole era transactions"
    =<< readFileTxBody txbodyFile
  -- We use the era of the tx we read to determine the era we use for the rest:

  someWit <- firstExceptT SophieTxCmdReadWitnessSigningDataError
    $ readWitnessSigningData witSignData

  witness <-
    case categoriseSomeWitness someWit of
      -- Cole witnesses require the network ID. This can either be provided
      -- directly or derived from a provided Cole address.
      AColeWitness bootstrapWitData ->
        firstExceptT SophieTxCmdBootstrapWitnessError
          . hoistEither
          $ mkSophieBootstrapWitness mbNw txbody bootstrapWitData
      ASophieKeyWitness skSophie ->
        pure $ makeSophieKeyWitness txbody skSophie

  firstExceptT SophieTxCmdWriteFileError
    . newExceptT
    $ writeFileTextEnvelope oFile Nothing witness


runTxSignWitness
  :: TxBodyFile
  -> [WitnessFile]
  -> OutputFile
  -> ExceptT SophieTxCmdError IO ()
runTxSignWitness (TxBodyFile txbodyFile) witnessFiles (OutputFile oFp) = do
    InAnyBccEra era txbody  <- readFileTxBody txbodyFile
    InAnySophieBasedEra _ _ <-
          --TODO: in principle we should be able to support Cole era txs too
          onlyInSophieBasedEras "sign for Cole era transactions"
                                 (InAnyBccEra era txbody)

    witnesses <-
      sequence
        [ do InAnyBccEra era' witness <- readFileWitness file
             case testEquality era era' of
               Nothing   -> left $ SophieTxCmdWitnessEraMismatch
                                     (AnyBccEra era)
                                     (AnyBccEra era')
                                     witnessFile
               Just Refl -> return witness
        | witnessFile@(WitnessFile file) <- witnessFiles ]

    let tx = makeSignedTransaction witnesses txbody
    firstExceptT SophieTxCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope oFp Nothing tx


-- ----------------------------------------------------------------------------
-- Reading files in any era
--

readFileWitness :: FilePath
                -> ExceptT SophieTxCmdError IO (InAnyBccEra KeyWitness)
readFileWitness = readFileInAnyBccEra AsKeyWitness


readFileTxBody :: FilePath
               -> ExceptT SophieTxCmdError IO (InAnyBccEra TxBody)
readFileTxBody = readFileInAnyBccEra AsTxBody


readFileTx :: FilePath -> ExceptT SophieTxCmdError IO (InAnyBccEra Tx)
readFileTx = readFileInAnyBccEra AsTx


readFileInAnyBccEra
  :: ( HasTextEnvelope (thing ColeEra)
     , HasTextEnvelope (thing SophieEra)
     , HasTextEnvelope (thing EvieEra)
     , HasTextEnvelope (thing JenEra)
     , HasTextEnvelope (thing AurumEra)
     )
  => (forall era. AsType era -> AsType (thing era))
  -> FilePath
  -> ExceptT SophieTxCmdError IO
            (InAnyBccEra thing)
readFileInAnyBccEra asThing file =
    firstExceptT SophieTxCmdReadTextViewFileError
  . newExceptT
  $ readFileTextEnvelopeAnyOf
      [ FromSomeType (asThing AsColeEra)   (InAnyBccEra ColeEra)
      , FromSomeType (asThing AsSophieEra) (InAnyBccEra SophieEra)
      , FromSomeType (asThing AsEvieEra) (InAnyBccEra EvieEra)
      , FromSomeType (asThing AsJenEra)    (InAnyBccEra JenEra)
      , FromSomeType (asThing AsAurumEra)  (InAnyBccEra AurumEra)
      ]
      file

-- | Constrain the era to be Sophie based. Fail for the Cole era.
--
onlyInSophieBasedEras :: Text
                       -> InAnyBccEra a
                       -> ExceptT SophieTxCmdError IO
                                  (InAnySophieBasedEra a)
onlyInSophieBasedEras notImplMsg (InAnyBccEra era x) =
    case bccEraStyle era of
      LegacyColeEra       -> left (SophieTxCmdNotImplemented notImplMsg)
      SophieBasedEra era' -> return (InAnySophieBasedEra era' x)


-- ----------------------------------------------------------------------------
-- Reading other files
--

validateScriptSupportedInEra :: BccEra era
                             -> ScriptInAnyLang
                             -> ExceptT SophieTxCmdError IO (ScriptInEra era)
validateScriptSupportedInEra era script@(ScriptInAnyLang lang _) =
    case toScriptInEra era script of
      Nothing -> left $ SophieTxCmdScriptLanguageNotSupportedInEra
                          (AnyScriptLanguage lang) (anyBccEra era)
      Just script' -> pure script'


-- ----------------------------------------------------------------------------
-- Transaction metadata
--

readFileTxMetadata :: TxMetadataJsonSchema -> MetadataFile
                   -> ExceptT SophieTxCmdError IO TxMetadata
readFileTxMetadata mapping (MetadataFileJSON fp) = do
    bs <- handleIOExceptT (SophieTxCmdReadFileError . FileIOError fp) $
          LBS.readFile fp
    v  <- firstExceptT (SophieTxCmdMetadataJsonParseError fp) $
          hoistEither $
            Aeson.eitherDecode' bs
    txMetadata <- firstExceptT (SophieTxCmdMetadataConversionError fp) $ hoistEither $
      metadataFromJson mapping v
    firstExceptT (SophieTxCmdMetaValidationError fp) $ hoistEither $ do
        validateTxMetadata txMetadata
        return txMetadata

readFileTxMetadata _ (MetadataFileCBOR fp) = do
    bs <- handleIOExceptT (SophieTxCmdReadFileError . FileIOError fp) $
          BS.readFile fp
    txMetadata <- firstExceptT (SophieTxCmdMetaDecodeError fp) $ hoistEither $
      deserialiseFromCBOR AsTxMetadata bs
    firstExceptT (SophieTxCmdMetaValidationError fp) $ hoistEither $ do
        validateTxMetadata txMetadata
        return txMetadata
