{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}


module Bcc.ZerepochExample.ScriptContextChecker where

import           Prelude hiding (($))

import           Bcc.Api
import           Bcc.Api.Cole
import           Bcc.Api.Sophie
import qualified Bcc.Api.Sophie as Api

import           Codec.Serialise
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except.Extra
import qualified Data.Aeson as Aeson
import           Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Short as SBS
import qualified Data.Map.Strict as Map
import           Data.Maybe as M
import qualified Data.Sequence.Strict as Seq
import qualified Data.Set as Set
import           GHC.Records (HasField (..))
import           System.FilePath.Posix

import           Bcc.CLI.Environment
import           Bcc.CLI.Sophie.Run.Query
import           Bcc.CLI.Types (SocketPath (..))
import qualified Bcc.Ledger.Aurum.PParams as Aurum
import qualified Bcc.Ledger.Aurum.ZerepochScriptApi as Aurum
import qualified Bcc.Ledger.Aurum.Tx as Aurum
import qualified Bcc.Ledger.Aurum.TxInfo as Aurum
import qualified Bcc.Ledger.Aurum.TxWitness as Aurum
import           Bcc.Ledger.Crypto (StandardCrypto)
import           Bcc.Protocol.TOptimum (ProtVer)
import           Bcc.Slotting.EpochInfo (EpochInfo, hoistEpochInfo)
import           Bcc.Slotting.Time (SystemStart)
import           Control.Monad.Trans.Except
import qualified Ledger as Zerepoch
import qualified Ledger.Typed.Scripts as Scripts
import qualified Shardagnostic.Consensus.HardFork.Combinator.AcrossEras as Consensus
import qualified Shardagnostic.Consensus.HardFork.History as Consensus
import           Shardagnostic.Network.Protocol.LocalStateQuery.Type (AcquireFailure)
import qualified Zerepoch.V1.Ledger.DCert as Zerepoch
import qualified ZerepochTx
import qualified ZerepochTx.AssocMap as AMap
import           ZerepochTx.IsData.Class
import           ZerepochTx.Prelude hiding (Semigroup (..), unless)
import qualified ZerepochTx.Prelude as P
import qualified Sophie.Spec.Ledger.TxBody as Sophie

-- Description
-- MyCustomRedeemer mimics the ScriptContext. MyCustomRedeemer is built via reading
-- the transaction containing the script and the script itself just compares MyCustomRedeemer
-- to the ScriptContext to be sure they are equivalent.
-- The overall aim is to make sure what is provided via ScriptContext (i.e. the transaction)
-- is what it's supposed to be. We check this by creating MyCustomRedeemer based on
-- the actual transaction which is created via the create-script-context executable.

newtype MyCustomDatum = MyCustomDatum Integer

data MyCustomRedeemer
  = MyCustomRedeemer
      { mCrOutputs :: [Zerepoch.TxOut]
      , mCrInputs :: [Zerepoch.TxInInfo]
      , mCrMint :: Zerepoch.Value
      , mCrValidRange :: Zerepoch.POSIXTimeRange
      , mCrFee :: Zerepoch.Value
      , mCrDatums :: [(Zerepoch.DatumHash, Zerepoch.Datum)]
      , mCrCerts :: [Zerepoch.DCert]
      , mCrSignatories :: [Zerepoch.PubKeyHash]
      , mCrScriptPurpose :: Maybe Zerepoch.ScriptPurpose
      } deriving (Prelude.Eq, Show)

ZerepochTx.unstableMakeIsData ''MyCustomDatum
ZerepochTx.unstableMakeIsData ''MyCustomRedeemer

data ScriptContextTest
instance Scripts.ValidatorTypes ScriptContextTest where
    type instance DatumType ScriptContextTest    = MyCustomDatum
    type instance RedeemerType ScriptContextTest = MyCustomRedeemer

{-# INLINABLE mkValidator #-}
mkValidator :: MyCustomDatum-> MyCustomRedeemer -> Zerepoch.ScriptContext -> Bool
mkValidator _datum (MyCustomRedeemer txouts txins minted txValidRange _fee datumsAndHashes certs signatories mPurpose) scriptContext =
  -- Minted field is equivalent
  Zerepoch.txInfoMint txInfo P.== minted P.&&
  -- Validity range is equivalent
  Zerepoch.txInfoValidRange txInfo P.== txValidRange P.&&
  -- Datums and datum hashes are equivalent
  Zerepoch.txInfoData txInfo P.== datumsAndHashes P.&&
  -- Required tx signers are equivalent
  Zerepoch.txInfoSignatories txInfo P.== signatories P.&&
  -- Payment tx out is equivalent
  AMap.member paymentOutputFromRedeemer scriptContextOutputsMap P.&&
  -- Txins are equivalent
  (AMap.member txinA scriptContextTxinsMap P.&& AMap.member txinB scriptContextTxinsMap) P.&&
  -- Cert if equivalent
  AMap.member singleRedeemerCert scriptContextCertsMap P.&&
  -- Check if the script purposes are equivalent
  case mPurpose of
    Just sPurp -> sPurp P.== sPurpose
    Nothing -> ZerepochTx.Prelude.error ()
 where
   scriptContextCertsMap :: AMap.Map Zerepoch.DCert Integer
   scriptContextCertsMap = AMap.fromList P.$ P.zip (Zerepoch.txInfoDCert txInfo) [1]

   singleRedeemerCert :: Zerepoch.DCert
   singleRedeemerCert = P.head certs

   txinA :: Zerepoch.TxInInfo
   txinA = P.head redeemerTxins

   txinB :: Zerepoch.TxInInfo
   txinB = P.head $ P.reverse redeemerTxins

   redeemerTxins :: [Zerepoch.TxInInfo]
   redeemerTxins = txins

   scriptContextTxins :: [Zerepoch.TxInInfo]
   scriptContextTxins = Zerepoch.txInfoInputs txInfo

   scriptContextTxinsMap :: AMap.Map Zerepoch.TxInInfo Integer
   scriptContextTxinsMap = AMap.fromList P.$ P.zip scriptContextTxins [1,2 :: Integer]

   -- This is paid to the dummy address. We can't compute the change address amount
   -- because the redeemer we computed is based on an older tx which affects the fee
   -- and therefore the change address amount.
   paymentOutputFromRedeemer :: Zerepoch.Value
   paymentOutputFromRedeemer = P.head $ P.reverse redeemerValues

   redeemerValues :: [Zerepoch.Value]
   redeemerValues = P.map Zerepoch.txOutValue txouts

   scriptContextOutputValues :: [Zerepoch.Value]
   scriptContextOutputValues = P.map Zerepoch.txOutValue $ Zerepoch.txInfoOutputs txInfo

   scriptContextOutputsMap :: AMap.Map Zerepoch.Value Integer
   scriptContextOutputsMap = AMap.fromList P.$ P.zip scriptContextOutputValues [1,2 :: Integer]

   txInfo :: Zerepoch.TxInfo
   txInfo = Zerepoch.scriptContextTxInfo scriptContext

   sPurpose :: Zerepoch.ScriptPurpose
   sPurpose = Zerepoch.scriptContextPurpose scriptContext

inst :: Scripts.TypedValidator ScriptContextTest
inst = Scripts.mkTypedValidator @ScriptContextTest
    $$(ZerepochTx.compile [|| mkValidator ||])
    $$(ZerepochTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @MyCustomDatum @MyCustomRedeemer

validator :: Zerepoch.Validator
validator = Scripts.validatorScript inst

script :: Zerepoch.Script
script = Zerepoch.unValidatorScript validator

scriptContextCheckAsShortBs :: SBS.ShortByteString
scriptContextCheckAsShortBs = SBS.toShort . LB.toStrict $ serialise script

scriptContextCheckScript :: ZerepochScript ZerepochScriptV1
scriptContextCheckScript = ZerepochScriptSerialised scriptContextCheckAsShortBs

------------------------------------------------------

sampleTestScriptContextDataJSON :: LB.ByteString
sampleTestScriptContextDataJSON =
  Aeson.encode
    . scriptDataToJson ScriptDataJsonDetailedSchema
    . customRedeemerToScriptData
    $ MyCustomRedeemer
        dummyTxOuts
        dummyTxIns
        dummyLedgerVal
        dummyPOSIXTimeRange
        dummyLedgerVal
        dummyDatumHashes
        dummyCerts
        dummySignatories
        dummyScriptPurpose

customRedeemerToScriptData :: MyCustomRedeemer -> ScriptData
customRedeemerToScriptData cRedeem =
  fromZerepochData $ ZerepochTx.builtinDataToData $ toBuiltinData cRedeem

customRedeemerFromScriptData :: ScriptData -> Either String MyCustomRedeemer
customRedeemerFromScriptData sDat =
  let bIData = ZerepochTx.dataToBuiltinData $ toZerepochData sDat
  in case fromBuiltinData bIData of
      Just mCRedeem -> Right mCRedeem
      Nothing -> Left "Could not decode MyCustomRedeemer from ScriptData"

dummyCerts :: [Zerepoch.DCert]
dummyCerts = []

dummyTxIns :: [Zerepoch.TxInInfo]
dummyTxIns = []

dummySignatories :: [Zerepoch.PubKeyHash]
dummySignatories = []

dummyDatumHashes :: [(Zerepoch.DatumHash, Zerepoch.Datum)]
dummyDatumHashes = []

dummyLedgerVal :: Zerepoch.Value
dummyLedgerVal = Aurum.transValue $ toJenValue Prelude.mempty

dummyTxOuts :: [Zerepoch.TxOut]
dummyTxOuts = []

dummyPOSIXTimeRange :: Zerepoch.POSIXTimeRange
dummyPOSIXTimeRange = Zerepoch.from $ Zerepoch.POSIXTime 42

dummyScriptPurpose :: Maybe Zerepoch.ScriptPurpose
dummyScriptPurpose = Nothing

data ScriptContextError = NoScriptsInColeEra
                        | NoScriptsInEra
                        | ReadTxBodyError (FileError TextEnvelopeError)
                        | IntervalConvError TransactionValidityIntervalError
                        | AcquireFail AcquireFailure
                        | NoTipLocalStateError
                        | NoSystemStartTimeError
                        | EnvVarSocketErr EnvSocketError
                        | ScriptContextErrorColeEra
                        | QueryError SophieQueryCmdError
                        | ConsensusModeMismatch AnyConsensusMode AnyBccEra
                        | EraMismatch !Consensus.EraMismatch
                        deriving Show

txToCustomRedeemer
  :: SophieBasedEra era
  -> ProtocolParameters
  -> UTxO era
  -> EpochInfo (Either TransactionValidityIntervalError)
  -> SystemStart
  -> Api.Tx era
  -> Either ScriptContextError MyCustomRedeemer
txToCustomRedeemer _ _ _ _ _ (ColeTx _) = Left NoScriptsInColeEra
txToCustomRedeemer sbe pparams utxo eInfo sStart (SophieTx SophieBasedEraAurum ledgerTx) = do
  let txBody = Aurum.body ledgerTx
      witness = Aurum.wits ledgerTx
      Aurum.TxWitness _ _ _ _ rdmrs = witness
      redeemerPtrs = Map.toList $ Aurum.unRedeemers rdmrs
      ledgerUTxO = toLedgerUTxO SophieBasedEraAurum utxo
      scriptsNeeded = Aurum.scriptsNeeded ledgerUTxO ledgerTx
      sPurpose = case scriptsNeeded of
                   [(p ,_)] -> Aurum.transScriptPurpose p
                   _ -> Prelude.error $ "More than one redeemer ptr: " <> show redeemerPtrs
      mTxIns = Prelude.map (Aurum.txInfoIn ledgerUTxO) . Set.toList $ Aurum.inputs txBody
      mTouts = Prelude.map Aurum.txInfoOut $ seqToList $ Aurum.outputs txBody
      minted = Aurum.transValue $ Aurum.mint txBody
      txfee = Aurum.transValue . toJenValue . entropicToValue . fromSophieEntropic $ Aurum.txfee txBody
      Aurum.TxDats datumHashMap = Aurum.txdats witness
      datumHashes = Prelude.map Aurum.transDataPair $ Map.toList datumHashMap
      _txid = Aurum.txInfoId . toSophieTxId $ getTxIdSophie SophieBasedEraAurum txBody
      txcerts = Prelude.map Aurum.transDCert . seqToList $ Aurum.txcerts txBody
      txsignatories = Prelude.map Aurum.transKeyHash . Set.toList $ Aurum.reqSignerHashes txBody
  valRange <-
    first IntervalConvError
      $ Aurum.transVITime (toLedgerPParams sbe pparams) eInfo sStart $ Aurum.txvldt txBody

  tOuts <- if Prelude.all M.isJust mTouts
           then return $ catMaybes mTouts
           else Prelude.error "Tx Outs not all Just"
  txins <- if Prelude.all M.isJust mTxIns
           then return $ catMaybes mTxIns
           else Prelude.error "Tx Ins not all Just"
  Right $ MyCustomRedeemer tOuts txins minted valRange txfee datumHashes txcerts txsignatories (Just sPurpose)
 where
  seqToList (x Seq.:<| rest) = x : seqToList rest
  seqToList Seq.Empty = []

txToCustomRedeemer _ _ _ _ _ (SophieTx _ _) = Left NoScriptsInEra


obtainLedgerEraClassConstraints
  :: SophieLedgerEra era ~ ledgerera
  => SophieBasedEra era
  -> ( HasField "_protocolVersion" (Aurum.PParams ledgerera) ProtVer
       => a) -> a
obtainLedgerEraClassConstraints SophieBasedEraSophie f = f
obtainLedgerEraClassConstraints SophieBasedEraEvie f = f
obtainLedgerEraClassConstraints SophieBasedEraJen    f = f
obtainLedgerEraClassConstraints SophieBasedEraAurum  f = f

testScriptContextToScriptData :: MyCustomRedeemer -> ScriptData
testScriptContextToScriptData = fromZerepochData . ZerepochTx.builtinDataToData . toBuiltinData

readCustomRedeemerFromTx
  :: FilePath
  -> AnyConsensusModeParams
  -> NetworkId
  -> ExceptT ScriptContextError IO MyCustomRedeemer
readCustomRedeemerFromTx fp (AnyConsensusModeParams cModeParams) network = do
  InAnyBccEra cEra aurumTx
    <- firstExceptT ReadTxBodyError
         . newExceptT
         $ readFileTextEnvelopeAnyOf
             [ FromSomeType (AsTx AsAurumEra) (InAnyBccEra AurumEra)
             ]
             fp

  sbe <- getSbe $ bccEraStyle cEra
  SocketPath sockPath <- firstExceptT EnvVarSocketErr readEnvSocketPath
  case consensusModeOnly cModeParams of
    BccMode -> do
      let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath
      eInMode <- hoistMaybe
                   (ConsensusModeMismatch (AnyConsensusMode BccMode) (AnyBccEra cEra))
                   $ toEraInMode cEra BccMode

      eResult <-
        liftIO $ executeLocalStateQueryExpr localNodeConnInfo Nothing
          $ \ntcVersion -> do
              (EraHistory _ interpreter) <- queryExpr $ QueryEraHistory BccModeIsMultiEra
              mSystemStart <-
                if ntcVersion Prelude.>= NodeToClientV_9
                then Just Prelude.<$> queryExpr QuerySystemStart
                else return Nothing
              let eInfo = hoistEpochInfo (first TransactionValidityIntervalError . runExcept)
                            $ Consensus.interpreterToEpochInfo interpreter
              ppResult <- queryExpr $ QueryInEra eInMode $ QueryInSophieBasedEra sbe QueryProtocolParameters
              return (eInfo, mSystemStart, ppResult)

      (eInfo, mSystemStart, ePParams) <- firstExceptT AcquireFail $ hoistEither eResult
      pparams <- firstExceptT EraMismatch $ hoistEither ePParams
      sStart <- hoistMaybe NoSystemStartTimeError mSystemStart

      -- Query UTxO
      let utxoQ = QueryInSophieBasedEra sbe (QueryUTxO QueryUTxOWhole)
          utxoQinMode = case toEraInMode cEra BccMode of
                          Just eInMode' -> QueryInEra eInMode' utxoQ
                          Nothing -> Prelude.error "Cannot determine era in mode"
      utxo <- firstExceptT QueryError
                $ executeQuery
                    cEra
                    cModeParams
                    localNodeConnInfo
                    utxoQinMode
      hoistEither $ txToCustomRedeemer sbe pparams
                                       utxo eInfo sStart aurumTx
    _ -> Prelude.error "Please specify --bcc-mode on cli."

txToRedeemer
  :: FilePath
  -> AnyConsensusModeParams
  -> NetworkId
  -> ExceptT ScriptContextError IO ()
txToRedeemer txFp anyCmodeParams nid = do
  testScrContext <- readCustomRedeemerFromTx txFp anyCmodeParams nid
  let redeemer = Aeson.encode . scriptDataToJson ScriptDataJsonDetailedSchema
                   $ testScriptContextToScriptData testScrContext
      outFp = dropFileName txFp </> "script-context.redeemer"
  liftIO . print $ "Tx generated redeemer: " <> show redeemer
  liftIO $ LB.writeFile outFp redeemer

getSbe :: BccEraStyle era -> ExceptT ScriptContextError IO (SophieBasedEra era)
getSbe LegacyColeEra = left ScriptContextErrorColeEra
getSbe (SophieBasedEra sbe) = return sbe


-- Used in roundtrip testing

fromZerepochTxId :: Zerepoch.TxId -> Sophie.TxId StandardCrypto
fromZerepochTxId (Zerepoch.TxId builtInBs) =
  case deserialiseFromRawBytes AsTxId $ fromBuiltin builtInBs of
    Just txidHash -> toSophieTxId txidHash
    Nothing -> Prelude.error "Could not derserialize txid"

-- Minting script that checks the minting value, validty interval and
-- required signers in the ScriptContext is equivalent to what's in the
-- redeemer.

{-# INLINABLE mkPolicy #-}
mkPolicy :: MyCustomRedeemer -> Zerepoch.ScriptContext -> Bool
mkPolicy (MyCustomRedeemer _ _ minted txValidRange _fee _ _ signatories mPurpose) scriptContext =
  -- Minted value is equivalent
  minted P.== Zerepoch.txInfoMint txInfo P.&&
  -- Validity range is equivalent
  Zerepoch.txInfoValidRange txInfo P.== txValidRange P.&&
  -- Required signers are equivalent
  AMap.member singleSignatory scriptContextSignatoriesMap P.&&

  case mPurpose of
    Just sPurp -> sPurp P.== sPurpose
    Nothing -> ZerepochTx.Prelude.error ()
 where
   sPurpose :: Zerepoch.ScriptPurpose
   sPurpose = Zerepoch.scriptContextPurpose scriptContext

   scriptContextSignatoriesMap :: AMap.Map Zerepoch.PubKeyHash Integer
   scriptContextSignatoriesMap = AMap.fromList P.$ P.zip (Zerepoch.txInfoSignatories txInfo) [1]

   singleSignatory :: Zerepoch.PubKeyHash
   singleSignatory = P.head signatories

   txInfo :: Zerepoch.TxInfo
   txInfo = Zerepoch.scriptContextTxInfo scriptContext

policy :: Scripts.MintingPolicy
policy = Zerepoch.mkMintingPolicyScript
           $$(ZerepochTx.compile [|| wrap ||])
 where
   wrap = Scripts.wrapMintingPolicy mkPolicy

zerepochMintingScript :: Zerepoch.Script
zerepochMintingScript =
  Zerepoch.unMintingPolicyScript policy

mintingValidator :: Zerepoch.Validator
mintingValidator =
  Zerepoch.Validator $ Zerepoch.unMintingPolicyScript policy

scriptAsCbor :: LB.ByteString
scriptAsCbor = serialise mintingValidator

customApiExampleZerepochMintingScript :: ZerepochScript ZerepochScriptV1
customApiExampleZerepochMintingScript = ZerepochScriptSerialised . SBS.toShort $ LB.toStrict scriptAsCbor

mintingScriptShortBs :: SBS.ShortByteString
mintingScriptShortBs = SBS.toShort . LB.toStrict $ scriptAsCbor
