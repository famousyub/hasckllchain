{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Sophie
  ( keyTests
  , certificateTests
  , keyConversionTests
  , metadataTests
  , multiSigTests
  , txTests
  ) where

import           Bcc.Prelude

import           Test.Golden.Sophie.Address.Build (golden_sophieAddressBuild)
import           Test.Golden.Sophie.Address.Info (golden_sophieAddressInfo)
import           Test.Golden.Sophie.Address.KeyGen (golden_sophieAddressKeyGen)
import           Test.Golden.Sophie.Genesis.Create (golden_sophieGenesisCreate)
import           Test.Golden.Sophie.Genesis.InitialTxIn (golden_sophieGenesisInitialTxIn)
import           Test.Golden.Sophie.Genesis.KeyGenDelegate (golden_sophieGenesisKeyGenDelegate)
import           Test.Golden.Sophie.Genesis.KeyGenGenesis (golden_sophieGenesisKeyGenGenesis)
import           Test.Golden.Sophie.Genesis.KeyGenUtxo (golden_sophieGenesisKeyGenUtxo)
import           Test.Golden.Sophie.Genesis.KeyHash (golden_sophieGenesisKeyHash)
import           Test.Golden.Sophie.Key.ConvertBccAddressKey
                   (golden_convertBccAddressColeSigningKey,
                   golden_convertBccAddressIcarusSigningKey,
                   golden_convertBccAddressSophiePaymentSigningKey,
                   golden_convertBccAddressSophieStakeSigningKey)
import           Test.Golden.Sophie.Node.IssueOpCert (golden_sophieNodeIssueOpCert)
import           Test.Golden.Sophie.Node.KeyGen (golden_sophieNodeKeyGen)
import           Test.Golden.Sophie.Node.KeyGenKes (golden_sophieNodeKeyGenKes)
import           Test.Golden.Sophie.Node.KeyGenVrf (golden_sophieNodeKeyGenVrf)
import           Test.Golden.Sophie.StakeAddress.Build (golden_sophieStakeAddressBuild)
import           Test.Golden.Sophie.StakeAddress.DeregistrationCertificate
                   (golden_sophieStakeAddressDeregistrationCertificate)
import           Test.Golden.Sophie.StakeAddress.KeyGen (golden_sophieStakeAddressKeyGen)
import           Test.Golden.Sophie.StakeAddress.RegistrationCertificate
                   (golden_sophieStakeAddressRegistrationCertificate)
import           Test.Golden.Sophie.StakePool.RegistrationCertificate
                   (golden_sophieStakePoolRegistrationCertificate)
import           Test.Golden.Sophie.TextEnvelope.Certificates.GenesisKeyDelegationCertificate
                   (golden_sophieGenesisKeyDelegationCertificate)
import           Test.Golden.Sophie.TextEnvelope.Certificates.MIRCertificate
                   (golden_sophieMIRCertificate)
import           Test.Golden.Sophie.TextEnvelope.Certificates.OperationalCertificate
                   (golden_sophieOperationalCertificate)
import           Test.Golden.Sophie.TextEnvelope.Certificates.StakeAddressCertificates
                   (golden_sophieStakeAddressCertificates)
import           Test.Golden.Sophie.TextEnvelope.Certificates.StakePoolCertificates
                   (golden_sophieStakePoolCertificates)

import           Test.Golden.Sophie.Metadata.StakePoolMetadata (golden_stakePoolMetadataHash)
import           Test.Golden.Sophie.MultiSig.Address (golden_sophieAllMultiSigAddressBuild,
                   golden_sophieAnyMultiSigAddressBuild, golden_sophieAtLeastMultiSigAddressBuild)
import           Test.Golden.Sophie.TextEnvelope.Keys.ExtendedPaymentKeys
                   (golden_sophieExtendedPaymentKeys)
import           Test.Golden.Sophie.TextEnvelope.Keys.GenesisDelegateKeys
                   (golden_sophieGenesisDelegateKeys)
import           Test.Golden.Sophie.TextEnvelope.Keys.GenesisKeys (golden_sophieGenesisKeys)
import           Test.Golden.Sophie.TextEnvelope.Keys.GenesisUTxOKeys
                   (golden_sophieGenesisUTxOKeys)
import           Test.Golden.Sophie.TextEnvelope.Keys.KESKeys (golden_sophieKESKeys)
import           Test.Golden.Sophie.TextEnvelope.Keys.PaymentKeys (golden_sophiePaymentKeys)
import           Test.Golden.Sophie.TextEnvelope.Keys.StakeKeys (golden_sophieStakeKeys)
import           Test.Golden.Sophie.TextEnvelope.Keys.VRFKeys (golden_sophieVRFKeys)
import           Test.Golden.Sophie.TextView.DecodeCbor (golden_sophieTextViewDecodeCbor)
import           Test.Golden.Sophie.Transaction.Assemble
                   (golden_sophieTransactionAssembleWitness_SigningKey)
import           Test.Golden.Sophie.Transaction.Build (golden_sophieTransactionBuild,
                   golden_sophieTransactionBuild_CertificateScriptWitnessed,
                   golden_sophieTransactionBuild_Minting,
                   golden_sophieTransactionBuild_TxInScriptWitnessed,
                   golden_sophieTransactionBuild_WithdrawalScriptWitnessed)
import           Test.Golden.Sophie.Transaction.CalculateMinFee
                   (golden_sophieTransactionCalculateMinFee)
import           Test.Golden.Sophie.Transaction.CreateWitness
                   (golden_sophieTransactionSigningKeyWitness)
import           Test.Golden.Sophie.Transaction.Sign (golden_sophieTransactionSign)

import           Test.Golden.Sophie.TextEnvelope.Tx.Tx (golden_sophieTx)
import           Test.Golden.Sophie.TextEnvelope.Tx.TxBody (golden_sophieTxBody)

import           Test.Golden.Version (golden_version)

import qualified Hedgehog as H

keyTests :: IO Bool
keyTests =
  H.checkSequential
    $ H.Group "TextEnvelope Key Goldens"
        [ ("golden_sophieAddressInfo", golden_sophieAddressInfo)
        , ("golden_sophieAddressKeyGen", golden_sophieAddressKeyGen)
        , ("golden_sophieAddressBuild", golden_sophieAddressBuild)
        , ("golden_sophieExtendedPaymentKeys", golden_sophieExtendedPaymentKeys)
        , ("golden_sophieGenesisCreate", golden_sophieGenesisCreate)
        , ("golden_sophieGenesisDelegateKeys", golden_sophieGenesisDelegateKeys)
        , ("golden_sophieGenesisInitialTxIn", golden_sophieGenesisInitialTxIn)
        , ("golden_sophieGenesisKeyGenDelegate", golden_sophieGenesisKeyGenDelegate)
        , ("golden_sophieGenesisKeyGenGenesis", golden_sophieGenesisKeyGenGenesis)
        , ("golden_sophieGenesisKeyGenUtxo", golden_sophieGenesisKeyGenUtxo)
        , ("golden_sophieGenesisKeyHash", golden_sophieGenesisKeyHash)
        , ("golden_sophieGenesisKeys", golden_sophieGenesisKeys)
        , ("golden_sophieGenesisUTxOKeys", golden_sophieGenesisUTxOKeys)
        , ("golden_sophieKESKeys", golden_sophieKESKeys)
        , ("golden_sophieNodeIssueOpCert", golden_sophieNodeIssueOpCert)
        , ("golden_sophieNodeKeyGen", golden_sophieNodeKeyGen)
        , ("golden_sophieNodeKeyGenKes", golden_sophieNodeKeyGenKes)
        , ("golden_sophieNodeKeyGenVrf", golden_sophieNodeKeyGenVrf)
        , ("golden_sophiePaymentKeys", golden_sophiePaymentKeys)
        , ("golden_sophieStakeAddressBuild", golden_sophieStakeAddressBuild)
        , ("golden_sophieStakeAddressDeregistrationCertificate", golden_sophieStakeAddressDeregistrationCertificate)
        , ("golden_sophieStakeAddressKeyGen", golden_sophieStakeAddressKeyGen)
        , ("golden_sophieStakeAddressRegistrationCertificate", golden_sophieStakeAddressRegistrationCertificate)
        , ("golden_sophieStakeKeys", golden_sophieStakeKeys)
        , ("golden_sophieStakePoolRegistrationCertificate", golden_sophieStakePoolRegistrationCertificate)
        , ("golden_sophieTextViewDecodeCbor", golden_sophieTextViewDecodeCbor)
        , ("golden_sophieTransactionBuild", golden_sophieTransactionBuild)
        , ("golden_sophieTransactionBuild_TxInScriptWitnessed", golden_sophieTransactionBuild_TxInScriptWitnessed)
        , ("golden_sophieTransactionBuild_Minting", golden_sophieTransactionBuild_Minting)
        , ("golden_sophieTransactionBuild_CertificateScriptWitnessed", golden_sophieTransactionBuild_CertificateScriptWitnessed)
        , ("golden_sophieTransactionBuild_WithdrawalScriptWitnessed", golden_sophieTransactionBuild_WithdrawalScriptWitnessed)
        , ("golden_sophieTransactionCalculateMinFee", golden_sophieTransactionCalculateMinFee)
        , ("golden_sophieTransactionSign", golden_sophieTransactionSign)
        , ("golden_sophieVRFKeys", golden_sophieVRFKeys)
        , ("golden_version", golden_version)
        ]

txTests :: IO Bool
txTests =
  H.checkSequential
    $ H.Group "TextEnvelope Tx Goldens"
        [ ("golden_sophieTxBody", golden_sophieTxBody)
        , ("golden_sophieTx", golden_sophieTx)
        ]

certificateTests :: IO Bool
certificateTests =
  H.checkSequential
    $ H.Group "TextEnvelope Certificate Goldens"
        [ ("golden_sophieStakeAddressCertificates", golden_sophieStakeAddressCertificates)
        , ("golden_sophieOperationalCertificate", golden_sophieOperationalCertificate)
        , ("golden_sophieStakePoolCertificates", golden_sophieStakePoolCertificates)
        , ("golden_sophieMIRCertificate", golden_sophieMIRCertificate)
        , ("golden_sophieGenesisKeyDelegationCertificate", golden_sophieGenesisKeyDelegationCertificate)
        ]

keyConversionTests :: IO Bool
keyConversionTests =
  H.checkSequential
    $ H.Group "Key Conversion Goldens"
        [ ("golden_convertBccAddressColeSigningKey", golden_convertBccAddressColeSigningKey)
        , ("golden_convertBccAddressIcarusSigningKey", golden_convertBccAddressIcarusSigningKey)
        , ("golden_convertBccAddressSophiePaymentSigningKey", golden_convertBccAddressSophiePaymentSigningKey)
        , ("golden_convertBccAddressSophieStakeSigningKey", golden_convertBccAddressSophieStakeSigningKey)
        ]

metadataTests :: IO Bool
metadataTests =
  H.checkSequential
    $ H.Group "Metadata Goldens"
        [ ("golden_stakePoolMetadataHash", golden_stakePoolMetadataHash)
        ]

multiSigTests :: IO Bool
multiSigTests =
  H.checkSequential
    $ H.Group "Multisig Goldens"
        [ ("golden_sophieAllMultiSigAddressBuild", golden_sophieAllMultiSigAddressBuild)
        , ("golden_sophieAnyMultiSigAddressBuild", golden_sophieAnyMultiSigAddressBuild)
        , ("golden_sophieAtLeastMultiSigAddressBuild", golden_sophieAtLeastMultiSigAddressBuild)
        , ("golden_sophieTransactionAssembleWitness_SigningKey", golden_sophieTransactionAssembleWitness_SigningKey)
        , ("golden_sophieTransactionSigningKeyWitness", golden_sophieTransactionSigningKeyWitness)
        ]
