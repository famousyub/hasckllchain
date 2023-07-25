{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bcc.Benchmarking.ZerepochExample
where
import Prelude

import qualified Data.ByteString.Char8 as BSC
import Control.Monad.Trans.Except

import Bcc.CLI.Sophie.Script (readFileScriptInAnyLang)

import Bcc.Api
import Bcc.Api.Sophie (ProtocolParameters)

import Bcc.Benchmarking.FundSet
import Bcc.Benchmarking.GeneratorTx.Tx as Tx (mkTxOutValueBccOnly)
import Bcc.Benchmarking.Wallet

mkUtxoScript ::
     NetworkId
  -> SigningKey PaymentKey
  -> (Script ZerepochScriptV1, Hash ScriptData)
  -> Validity
  -> ToUTxO AurumEra
mkUtxoScript networkId key (script, txOutDatumHash) validity values
  = ( map mkTxOut values
    , newFunds
    )
 where
  mkTxOut v = TxOut zerepochScriptAddr (mkTxOutValueBccOnly v) (TxOutDatumHash ScriptDataInAurumEra txOutDatumHash)

  zerepochScriptAddr = makeSophieAddressInEra
                       networkId
                       (PaymentCredentialByScript $ hashScript script)
                       NoStakeAddress

  newFunds txId = zipWith (mkNewFund txId) [TxIx 0 ..] values

  mkNewFund :: TxId -> TxIx -> Entropic -> Fund
  mkNewFund txId txIx val = Fund $ InAnyBccEra AurumEra $ FundInEra {
      _fundTxIn = TxIn txId txIx
    , _fundVal = mkTxOutValueBccOnly val
    , _fundSigningKey = key
    , _fundValidity = validity
    , _fundVariant = ZerepochScriptFund
    }

readScript :: FilePath -> IO (Script ZerepochScriptV1)
readScript fp = do
  res <- runExceptT $ readFileScriptInAnyLang fp
  case res of
    Left err -> do
      print err
      error $ show err
    Right (ScriptInAnyLang (ZerepochScriptLanguage ZerepochScriptV1) script) -> return script
    Right _otherScript ->
      error "Wrong script version."

toScriptHash :: String -> Hash ScriptData
toScriptHash str
  = case deserialiseFromRawBytesHex (AsHash AsScriptData) (BSC.pack str) of
    Just x -> x
    Nothing  -> error $ "Invalid datum hash: " ++ show str

genTxZerepochSpend ::
     ProtocolParameters
  -> [Fund]
  -> ScriptWitness WitCtxTxIn AurumEra
  -> TxFee AurumEra
  -> TxMetadataInEra AurumEra
  -> TxGenerator AurumEra
genTxZerepochSpend protocolParameters collateral scriptWitness fee metadata inFunds outputs
  = case makeTransactionBody txBodyContent of
      Left err -> error $ show err
      Right b -> Right ( signSophieTransaction b (map (WitnessPaymentKey . getFundKey) inFunds)
                       , getTxId b
                       )
 where
  txBodyContent = TxBodyContent {
      txIns = map (\f -> (getFundTxIn f, BuildTxWith $ ScriptWitness ScriptWitnessForSpending scriptWitness )) inFunds
    , txInsCollateral = TxInsCollateral CollateralInAurumEra $  map getFundTxIn collateral
    , txOuts = outputs
    , txFee = fee
    , txValidityRange = (TxValidityNoLowerBound, TxValidityNoUpperBound ValidityNoUpperBoundInAurumEra)
    , txMetadata = metadata
    , txAuxScripts = TxAuxScriptsNone
    , txExtraScriptData = BuildTxWith TxExtraScriptDataNone
    , txExtraKeyWits = TxExtraKeyWitnessesNone
    , txProtocolParams = BuildTxWith $ Just protocolParameters
    , txWithdrawals = TxWithdrawalsNone
    , txCertificates = TxCertificatesNone
    , txUpdateProposal = TxUpdateProposalNone
    , txMintValue = TxMintNone
    , txScriptValidity = TxScriptValidityNone
    }
