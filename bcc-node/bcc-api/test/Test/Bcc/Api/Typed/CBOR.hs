{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Bcc.Api.Typed.CBOR
  ( tests
  ) where

import           Bcc.Api
import           Gen.Bcc.Api.Typed
import           Gen.Hedgehog.Roundtrip.CBOR (roundtrip_CBOR)
import           Gen.Tasty.Hedgehog.Group (fromGroup)
import           Hedgehog (Property, discover)
import           Test.Bcc.Api.Typed.Orphans ()
import           Test.Tasty (TestTree)

{- HLINT ignore "Use camelCase" -}

-- TODO: Need to add PaymentExtendedKey roundtrip tests however
-- we can't derive an Eq instance for Crypto.HD.XPrv

prop_roundtrip_txbody_cole_CBOR :: Property
prop_roundtrip_txbody_cole_CBOR =
  roundtrip_CBOR (AsTxBody AsColeEra) (genTxBody ColeEra)

prop_roundtrip_txbody_sophie_CBOR :: Property
prop_roundtrip_txbody_sophie_CBOR =
  roundtrip_CBOR (AsTxBody AsSophieEra) (genTxBody SophieEra)

prop_roundtrip_txbody_evie_CBOR :: Property
prop_roundtrip_txbody_evie_CBOR =
  roundtrip_CBOR (AsTxBody AsEvieEra) (genTxBody EvieEra)

prop_roundtrip_txbody_jen_CBOR :: Property
prop_roundtrip_txbody_jen_CBOR =
  roundtrip_CBOR (AsTxBody AsJenEra) (genTxBody JenEra)

prop_roundtrip_txbody_aurum_CBOR :: Property
prop_roundtrip_txbody_aurum_CBOR =
  roundtrip_CBOR (AsTxBody AsAurumEra) (genTxBody AurumEra)

prop_roundtrip_tx_cole_CBOR :: Property
prop_roundtrip_tx_cole_CBOR =
  roundtrip_CBOR (AsTx AsColeEra) (genTx ColeEra)

prop_roundtrip_tx_sophie_CBOR :: Property
prop_roundtrip_tx_sophie_CBOR =
  roundtrip_CBOR (AsTx AsSophieEra) (genTx SophieEra)

prop_roundtrip_tx_evie_CBOR :: Property
prop_roundtrip_tx_evie_CBOR =
  roundtrip_CBOR (AsTx AsEvieEra) (genTx EvieEra)

prop_roundtrip_tx_jen_CBOR :: Property
prop_roundtrip_tx_jen_CBOR =
  roundtrip_CBOR (AsTx AsJenEra) (genTx JenEra)

prop_roundtrip_tx_aurum_CBOR :: Property
prop_roundtrip_tx_aurum_CBOR =
  roundtrip_CBOR (AsTx AsAurumEra) (genTx AurumEra)

prop_roundtrip_witness_cole_CBOR :: Property
prop_roundtrip_witness_cole_CBOR =
  roundtrip_CBOR (AsKeyWitness AsColeEra) genColeKeyWitness

prop_roundtrip_witness_sophie_CBOR :: Property
prop_roundtrip_witness_sophie_CBOR =
  roundtrip_CBOR (AsKeyWitness AsSophieEra) (genSophieWitness SophieEra)

prop_roundtrip_witness_evie_CBOR :: Property
prop_roundtrip_witness_evie_CBOR =
  roundtrip_CBOR (AsKeyWitness AsEvieEra) (genSophieWitness EvieEra)

prop_roundtrip_witness_jen_CBOR :: Property
prop_roundtrip_witness_jen_CBOR =
  roundtrip_CBOR (AsKeyWitness AsJenEra) (genSophieWitness JenEra)

prop_roundtrip_witness_aurum_CBOR :: Property
prop_roundtrip_witness_aurum_CBOR =
  roundtrip_CBOR (AsKeyWitness AsAurumEra) (genSophieWitness AurumEra)

prop_roundtrip_operational_certificate_CBOR :: Property
prop_roundtrip_operational_certificate_CBOR =
  roundtrip_CBOR AsOperationalCertificate genOperationalCertificate

prop_roundtrip_operational_certificate_issue_counter_CBOR :: Property
prop_roundtrip_operational_certificate_issue_counter_CBOR =
  roundtrip_CBOR AsOperationalCertificateIssueCounter genOperationalCertificateIssueCounter

prop_roundtrip_verification_key_cole_CBOR :: Property
prop_roundtrip_verification_key_cole_CBOR =
  roundtrip_CBOR (AsVerificationKey AsColeKey) (genVerificationKey AsColeKey)

prop_roundtrip_signing_key_cole_CBOR :: Property
prop_roundtrip_signing_key_cole_CBOR =
  roundtrip_CBOR (AsSigningKey AsColeKey) (genSigningKey AsColeKey)

prop_roundtrip_verification_key_payment_CBOR :: Property
prop_roundtrip_verification_key_payment_CBOR =
  roundtrip_CBOR (AsVerificationKey AsPaymentKey) (genVerificationKey AsPaymentKey)

prop_roundtrip_signing_key_payment_CBOR :: Property
prop_roundtrip_signing_key_payment_CBOR =
  roundtrip_CBOR (AsSigningKey AsPaymentKey) (genSigningKey AsPaymentKey)

prop_roundtrip_verification_key_stake_CBOR :: Property
prop_roundtrip_verification_key_stake_CBOR =
  roundtrip_CBOR (AsVerificationKey AsStakeKey) (genVerificationKey AsStakeKey)

prop_roundtrip_signing_key_stake_CBOR :: Property
prop_roundtrip_signing_key_stake_CBOR =
  roundtrip_CBOR (AsSigningKey AsStakeKey) (genSigningKey AsStakeKey)

prop_roundtrip_verification_key_genesis_CBOR :: Property
prop_roundtrip_verification_key_genesis_CBOR =
  roundtrip_CBOR (AsVerificationKey AsGenesisKey) (genVerificationKey AsGenesisKey)

prop_roundtrip_signing_key_genesis_CBOR :: Property
prop_roundtrip_signing_key_genesis_CBOR =
  roundtrip_CBOR (AsSigningKey AsGenesisKey) (genSigningKey AsGenesisKey)

prop_roundtrip_verification_key_genesis_delegate_CBOR :: Property
prop_roundtrip_verification_key_genesis_delegate_CBOR =
  roundtrip_CBOR (AsVerificationKey AsGenesisDelegateKey) (genVerificationKey AsGenesisDelegateKey)

prop_roundtrip_signing_key_genesis_delegate_CBOR :: Property
prop_roundtrip_signing_key_genesis_delegate_CBOR =
  roundtrip_CBOR (AsSigningKey AsGenesisDelegateKey) (genSigningKey AsGenesisDelegateKey)

prop_roundtrip_verification_key_stake_pool_CBOR :: Property
prop_roundtrip_verification_key_stake_pool_CBOR =
  roundtrip_CBOR (AsVerificationKey AsStakePoolKey) (genVerificationKey AsStakePoolKey)

prop_roundtrip_signing_key_stake_pool_CBOR :: Property
prop_roundtrip_signing_key_stake_pool_CBOR =
  roundtrip_CBOR (AsSigningKey AsStakePoolKey) (genSigningKey AsStakePoolKey)

prop_roundtrip_verification_key_vrf_CBOR :: Property
prop_roundtrip_verification_key_vrf_CBOR =
  roundtrip_CBOR (AsVerificationKey AsVrfKey) (genVerificationKey AsVrfKey)

prop_roundtrip_signing_key_vrf_CBOR :: Property
prop_roundtrip_signing_key_vrf_CBOR =
  roundtrip_CBOR (AsSigningKey AsVrfKey) (genSigningKey AsVrfKey)

prop_roundtrip_verification_key_kes_CBOR :: Property
prop_roundtrip_verification_key_kes_CBOR =
  roundtrip_CBOR (AsVerificationKey AsKesKey) (genVerificationKey AsKesKey)

prop_roundtrip_signing_key_kes_CBOR :: Property
prop_roundtrip_signing_key_kes_CBOR =
  roundtrip_CBOR (AsSigningKey AsKesKey) (genSigningKey AsKesKey)

prop_roundtrip_script_SimpleScriptV1_CBOR :: Property
prop_roundtrip_script_SimpleScriptV1_CBOR =
  roundtrip_CBOR (AsScript AsSimpleScriptV1)
                 (genScript (SimpleScriptLanguage SimpleScriptV1))

prop_roundtrip_script_SimpleScriptV2_CBOR :: Property
prop_roundtrip_script_SimpleScriptV2_CBOR =
  roundtrip_CBOR (AsScript AsSimpleScriptV2)
                 (genScript (SimpleScriptLanguage SimpleScriptV2))

prop_roundtrip_script_ZerepochScriptV1_CBOR :: Property
prop_roundtrip_script_ZerepochScriptV1_CBOR =
  roundtrip_CBOR (AsScript AsZerepochScriptV1)
                 (genScript (ZerepochScriptLanguage ZerepochScriptV1))

prop_roundtrip_UpdateProposal_CBOR :: Property
prop_roundtrip_UpdateProposal_CBOR =
  roundtrip_CBOR AsUpdateProposal genUpdateProposal

-- -----------------------------------------------------------------------------

tests :: TestTree
tests = fromGroup $$discover
