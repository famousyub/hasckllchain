{-# LANGUAGE GADTs #-}

module Test.ZerepochExample.Gen where

import           Bcc.Api
import           Bcc.Api.Sophie
import           Prelude

import qualified Data.Map.Strict as Map

import qualified Bcc.Ledger.Aurum.Tx as Aurum
import qualified Bcc.Ledger.Aurum.TxInfo as Aurum
import           Bcc.Ledger.Crypto (StandardCrypto)
import qualified Bcc.Ledger.Era as Ledger
import           Bcc.ZerepochExample.ScriptContextChecker
import           Gen.Bcc.Api.Typed
import qualified Ledger as Zerepoch
import qualified Zerepoch.V1.Ledger.DCert as Zerepoch
import qualified Sophie.Spec.Ledger.TxBody as Ledger
import qualified Sophie.Spec.Ledger.UTxO as Ledger

import           Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


genZerepochTxOut :: Gen Zerepoch.TxOut
genZerepochTxOut = do
  aurumTxOut <-
    TxOut <$> (sophieAddressInEra <$> genAddressSophie)
          <*> genTxOutValue AurumEra
          <*> genTxOutDatumHash AurumEra
  Gen.just $ return $ Aurum.txInfoOut $ toSophieTxOut SophieBasedEraAurum aurumTxOut

genMyCustomRedeemer :: Gen MyCustomRedeemer
genMyCustomRedeemer =
  MyCustomRedeemer
    <$> Gen.list (Range.singleton 1) genZerepochTxOut
    <*> return mempty --TODO: Investigate why genTxInfoIn generates Nothing
    <*> (Aurum.transValue . toJenValue <$> genValueForMinting)
    <*> genPOSIXTimeRange
    <*> (Aurum.transValue . toJenValue <$> genValueForTxOut)
    <*> genDatumMap
    <*> Gen.list (Range.constant 0 2) genZerepochCert
    <*> Gen.list (Range.constant 0 2) genReqSigners
    <*> return Nothing

genTxInfoIn :: Gen Zerepoch.TxInInfo
genTxInfoIn = do
  txinput <- genTxIn
  txout <- genTxOut AurumEra
  lUTxO <- genLedgerUTxO SophieBasedEraAurum (txinput, txout)
  let mTxInfoIn = Aurum.txInfoIn lUTxO (toSophieTxIn txinput)
  case mTxInfoIn of
    Just txin -> return txin
    Nothing -> error $ "Utxo: " ++ show lUTxO ++ "\n" ++ "Txin: " ++ show txinput

genReqSigners :: Gen Zerepoch.PubKeyHash
genReqSigners = do
  PaymentKeyHash kh <- genVerificationKeyHash AsPaymentKey
  return $ Aurum.transKeyHash kh

genLedgerUTxO
  :: (Ledger.Crypto (SophieLedgerEra era) ~ StandardCrypto)
  => SophieBasedEra era
  -> (TxIn, TxOut era)
  -> Gen (Ledger.UTxO (SophieLedgerEra era))
genLedgerUTxO sbe (txin, out) = do
  UTxO utxoMap <- genUTxO (sophieBasedToBccEra sbe)
  return . toLedgerUTxO sbe . UTxO $ Map.insert txin out  utxoMap

genZerepochCert :: Gen Zerepoch.DCert
genZerepochCert = Aurum.transDCert . toSophieCertificate <$> genCertificate

genLedgerTxIn :: Gen (Ledger.TxIn StandardCrypto)
genLedgerTxIn = toSophieTxIn <$> genTxIn

genZerepochTxId :: Gen Zerepoch.TxId
genZerepochTxId =
  Aurum.txInfoId . toSophieTxId <$> genTxId

genDatumMap :: Gen [(Zerepoch.DatumHash, Zerepoch.Datum)]
genDatumMap =
  map Aurum.transDataPair <$> Gen.list (Range.linear 0 5) genDatumHashTuple

genDatumHashTuple :: Gen (Aurum.DataHash StandardCrypto, Aurum.Data ledgerera)
genDatumHashTuple = do
  sData <- genScriptData
  let ScriptDataHash h = hashScriptData sData
  return (h, toAurumData sData)

genPOSIXTimeRange :: Gen Zerepoch.POSIXTimeRange
genPOSIXTimeRange = do
  ptime <- Zerepoch.POSIXTime <$> Gen.integral (Range.linear 0 10)
  Gen.element [ Zerepoch.to ptime
              , Zerepoch.always
              , Zerepoch.never
              , Zerepoch.singleton ptime
              ]
