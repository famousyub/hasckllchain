{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Bcc.Benchmarking.GeneratorTx.Tx
  ( Fund
  , fundTxIn
  , fundBccValue
  , keyAddress
  , mkGenesisTransaction
  , mkFund
  , mkFee
  , mkTransactionGen
  , mkTxOutValueBccOnly
  , mkValidityUpperBound
  , txOutValueToEntropic
  , txInModeBcc
  )
where

import           Prelude

import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)

import           Bcc.Benchmarking.Types (TxAdditionalSize(..))

import           Bcc.Api

type Fund = (TxIn, InAnyBccEra TxOutValue)

mkFund :: forall era. IsBccEra era => TxIn -> TxOutValue era -> Fund
mkFund txIn val = (txIn, InAnyBccEra bccEra val)

fundTxIn :: Fund -> TxIn
fundTxIn (x,_) = x

fundBccValue :: Fund -> Entropic
fundBccValue (_, InAnyBccEra _ txOut) = txOutValueToEntropic txOut

keyAddress :: forall era. IsSophieBasedEra era => NetworkId -> SigningKey PaymentKey -> AddressInEra era
keyAddress networkId k
  = makeSophieAddressInEra
      networkId
      (PaymentCredentialByKey $ verificationKeyHash $ getVerificationKey k)
      NoStakeAddress

--{-# DEPRECATED mkGenesisTransaction "to be removed" #-}
mkGenesisTransaction :: forall era .
     IsSophieBasedEra era
  => SigningKey GenesisUTxOKey
  -> TxAdditionalSize
  -> SlotNo
  -> Entropic
  -> [TxIn]
  -> [TxOut era]
  -> Tx era
mkGenesisTransaction key _payloadSize ttl fee txins txouts
  = case makeTransactionBody txBodyContent of
    Right b -> signSophieTransaction b [WitnessGenesisUTxOKey key]
    Left err -> error $ show err
 where
  txBodyContent = TxBodyContent {
      txIns = zip txins $ repeat $ BuildTxWith $ KeyWitness KeyWitnessForSpending
    , txInsCollateral = TxInsCollateralNone
    , txOuts = txouts
    , txFee = fees
    , txValidityRange = (TxValidityNoLowerBound, validityUpperBound)
    , txMetadata = TxMetadataNone
    , txAuxScripts = TxAuxScriptsNone
    , txExtraScriptData = BuildTxWith TxExtraScriptDataNone
    , txExtraKeyWits = TxExtraKeyWitnessesNone
    , txProtocolParams = BuildTxWith Nothing
    , txWithdrawals = TxWithdrawalsNone
    , txCertificates = TxCertificatesNone
    , txUpdateProposal = TxUpdateProposalNone
    , txMintValue = TxMintNone
    , txScriptValidity = TxScriptValidityNone
    }
  fees = case sophieBasedEra @ era of
    SophieBasedEraSophie -> TxFeeExplicit TxFeesExplicitInSophieEra fee
    SophieBasedEraEvie -> TxFeeExplicit TxFeesExplicitInEvieEra fee
    SophieBasedEraJen    -> TxFeeExplicit TxFeesExplicitInJenEra fee
    SophieBasedEraAurum  -> TxFeeExplicit TxFeesExplicitInAurumEra fee
  validityUpperBound = case sophieBasedEra @ era of
    SophieBasedEraSophie -> TxValidityUpperBound ValidityUpperBoundInSophieEra ttl
    SophieBasedEraEvie -> TxValidityUpperBound ValidityUpperBoundInEvieEra ttl
    SophieBasedEraJen    -> TxValidityUpperBound ValidityUpperBoundInJenEra ttl
    SophieBasedEraAurum  -> TxValidityUpperBound ValidityUpperBoundInAurumEra ttl

mkTransaction :: forall era .
     IsSophieBasedEra era
  => SigningKey PaymentKey
  -> TxMetadataInEra era
  -> SlotNo
  -> Entropic
  -> [TxIn]
  -> [TxOut era]
  -> Tx era
mkTransaction key metadata ttl fee txins txouts
  = case makeTransactionBody txBodyContent of
    Right b -> signSophieTransaction b [WitnessPaymentKey key]
    Left err -> error $ show err
 where
  txBodyContent = TxBodyContent {
      txIns = zip txins $ repeat $ BuildTxWith $ KeyWitness KeyWitnessForSpending
    , txInsCollateral = TxInsCollateralNone
    , txOuts = txouts
    , txFee = mkFee fee
    , txValidityRange = (TxValidityNoLowerBound, mkValidityUpperBound ttl)
    , txMetadata = metadata
    , txAuxScripts = TxAuxScriptsNone
    , txExtraScriptData = BuildTxWith TxExtraScriptDataNone
    , txExtraKeyWits = TxExtraKeyWitnessesNone
    , txProtocolParams = BuildTxWith Nothing
    , txWithdrawals = TxWithdrawalsNone
    , txCertificates = TxCertificatesNone
    , txUpdateProposal = TxUpdateProposalNone
    , txMintValue = TxMintNone
    , txScriptValidity = TxScriptValidityNone
    }

mkFee :: forall era .
     IsSophieBasedEra era
  => Entropic
  -> TxFee era
mkFee f = case sophieBasedEra @ era of
  SophieBasedEraSophie -> TxFeeExplicit TxFeesExplicitInSophieEra f
  SophieBasedEraEvie -> TxFeeExplicit TxFeesExplicitInEvieEra f
  SophieBasedEraJen    -> TxFeeExplicit TxFeesExplicitInJenEra f
  SophieBasedEraAurum  -> TxFeeExplicit TxFeesExplicitInAurumEra f

mkValidityUpperBound :: forall era .
     IsSophieBasedEra era
  => SlotNo
  -> TxValidityUpperBound era
mkValidityUpperBound ttl = case sophieBasedEra @ era of
  SophieBasedEraSophie -> TxValidityUpperBound ValidityUpperBoundInSophieEra ttl
  SophieBasedEraEvie -> TxValidityUpperBound ValidityUpperBoundInEvieEra ttl
  SophieBasedEraJen    -> TxValidityUpperBound ValidityUpperBoundInJenEra ttl
  SophieBasedEraAurum  -> TxValidityUpperBound ValidityUpperBoundInAurumEra ttl

mkTransactionGen :: forall era .
     IsSophieBasedEra era
  => SigningKey PaymentKey
  -> NonEmpty Fund
  -> AddressInEra era
  -> [(Int, TxOut era)]
  -- ^ Each recipient and their payment details
  -> TxMetadataInEra era
  -- ^ Optional size of additional binary blob in transaction (as 'txAttributes')
  -> Entropic
  -- ^ Tx fee.
  -> ( Maybe (TxIx, Entropic)   -- The 'change' index and value (if any)
     , Entropic                 -- The associated fees
     , Map Int TxIx             -- The offset map in the transaction below
     , Tx era
     )
mkTransactionGen signingKey inputs address payments metadata fee =
  (mChange, fee, offsetMap, tx)
 where
  tx = mkTransaction signingKey metadata (SlotNo 10000000)
         fee
         (NonEmpty.toList $ fundTxIn <$> inputs)
         (NonEmpty.toList txOutputs)

  payTxOuts     = map snd payments

  totalInpValue = sum $ fundBccValue <$> inputs
  totalOutValue = txOutSum payTxOuts
  changeValue = totalInpValue - totalOutValue - fee
      -- change the order of comparisons first check emptyness of txouts AND remove appendr after

  (txOutputs, mChange) = case compare changeValue 0 of
    GT ->
      let changeTxOut   = TxOut address (mkTxOutValueBccOnly changeValue) TxOutDatumHashNone
          changeIndex   = TxIx $ fromIntegral $ length payTxOuts -- 0-based index
      in
          (appendr payTxOuts (changeTxOut :| []), Just (changeIndex, changeValue))
    EQ ->
      case payTxOuts of
        []                 -> error "change is zero and txouts is empty"
        txout0: txoutsRest -> (txout0 :| txoutsRest, Nothing)
    LT -> error "Bad transaction: insufficient funds"

  -- TxOuts of recipients are placed at the first positions
  offsetMap = Map.fromList $ zipWith (\payment index -> (fst payment, TxIx index))
                                     payments
                                     [0..]
  txOutSum :: [ TxOut era ] -> Entropic
  txOutSum l = sum $ map toVal l

  toVal (TxOut _ val _) = txOutValueToEntropic val

  -- | Append a non-empty list to a list.
  -- > appendr [1,2,3] (4 :| [5]) == 1 :| [2,3,4,5]
  appendr :: [a] -> NonEmpty a -> NonEmpty a
  appendr l nel = foldr NonEmpty.cons nel l

mkTxOutValueBccOnly :: forall era . IsSophieBasedEra era => Entropic -> TxOutValue era
mkTxOutValueBccOnly l = case sophieBasedEra @ era of
  SophieBasedEraSophie -> TxOutBccOnly BccOnlyInSophieEra l
  SophieBasedEraEvie -> TxOutBccOnly BccOnlyInEvieEra l
  SophieBasedEraJen    -> TxOutValue MultiAssetInJenEra $ entropicToValue l
  SophieBasedEraAurum  -> TxOutValue MultiAssetInAurumEra $ entropicToValue l

txOutValueToEntropic :: TxOutValue era -> Entropic
txOutValueToEntropic = \case
  TxOutBccOnly BccOnlyInColeEra   x -> x
  TxOutBccOnly BccOnlyInSophieEra x -> x
  TxOutBccOnly BccOnlyInEvieEra x -> x
  TxOutValue _ v -> case valueToEntropic v of
    Just c -> c
    Nothing -> error "txOutValueEntropic  TxOut contains no BCC"

txInModeBcc :: forall era . IsSophieBasedEra era => Tx era -> TxInMode BccMode
txInModeBcc tx = case sophieBasedEra @ era of
  SophieBasedEraSophie -> TxInMode tx SophieEraInBccMode
  SophieBasedEraEvie -> TxInMode tx EvieEraInBccMode
  SophieBasedEraJen    -> TxInMode tx JenEraInBccMode
  SophieBasedEraAurum  -> TxInMode tx AurumEraInBccMode
