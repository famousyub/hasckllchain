{-# LANGUAGE TemplateHaskell #-}

module Test.ZerepochExample.Zerepoch where

import           Bcc.Prelude

import           Bcc.Api
import           Bcc.Api.Sophie


import           Hedgehog (Property, checkParallel, discover, forAll, property, (===))

import qualified Bcc.Ledger.Aurum.TxInfo as Aurum
import           Bcc.ZerepochExample.ScriptContextChecker

import           Gen.Bcc.Api.Typed

prop_TxId_Api_Ledger_Zerepoch_Roundtrip :: Property
prop_TxId_Api_Ledger_Zerepoch_Roundtrip =
  property $ do
    -- Api <-> ledger round trip

    (SophieTxBody _ txBody _ _ _ _) <- forAll $ genTxBody AurumEra
    let apiTxId = getTxIdSophie SophieBasedEraAurum txBody
        ledgerTxId = toSophieTxId apiTxId
        roundTripped = fromSophieTxId ledgerTxId
    apiTxId === roundTripped

    -- Zerepoch <-> ledger roundtrip
    let zerepochTxId = Aurum.txInfoId ledgerTxId
        rtLedgerTxId = fromZerepochTxId zerepochTxId
        rtZerepochTxIf = Aurum.txInfoId rtLedgerTxId
    rtLedgerTxId === ledgerTxId
    zerepochTxId === rtZerepochTxIf



prop_TxId_Api_Ledger_Roundtrip :: Property
prop_TxId_Api_Ledger_Roundtrip =
  property $ do
    (SophieTxBody _ txBody _ _ _ _) <- forAll $ genTxBody AurumEra
    let apiTxId = getTxIdSophie SophieBasedEraAurum txBody
        ledgerTxId = toSophieTxId apiTxId
        roundTripped = fromSophieTxId ledgerTxId
    apiTxId === roundTripped

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests = checkParallel $$discover

