{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Test.Bcc.Api.Genesis
  ( exampleSophieGenesis
  ) where

import           Bcc.Prelude

import           Bcc.Api.Sophie (SophieGenesis (..))

import qualified Data.Map.Strict as Map
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import           Bcc.Slotting.Slot (EpochSize (..))
import           Shardagnostic.Consensus.Sophie.Eras (StandardCrypto, StandardSophie)
import           Shardagnostic.Consensus.Sophie.Node (emptyGenesisStaking)
import           Shardagnostic.Consensus.Util.Time

import           Bcc.Ledger.Address (Addr (..))
import           Bcc.Ledger.Credential (Credential (..), PaymentCredential,
                   StakeCredential, StakeReference (..))
import           Bcc.Ledger.BaseTypes (Network (..))
import           Bcc.Ledger.Coin (Coin (..))
import           Bcc.Ledger.Keys (GenDelegPair (..), VestedDelegPair (..), Hash, KeyHash (..), KeyRole (..),
                   VerKeyVRF)
import           Sophie.Spec.Ledger.PParams (PParams' (..), emptyPParams)

import           Test.Sophie.Spec.Ledger.SentryUtils (unsafeBoundRational)

exampleSophieGenesis :: SophieGenesis StandardSophie
exampleSophieGenesis =
  SophieGenesis
    { sgSystemStart = posixSecondsToUTCTime $ realToFrac (1234566789 :: Integer)
    , sgNetworkMagic = 4036000900
    , sgNetworkId = Testnet
    , sgActiveSlotsCoeff = unsafeBoundRational 0.259
    , sgSecurityParam = 120842
    , sgVestMultiple = 1
    , sgEpochLength = EpochSize 1215
    , sgSlotsPerKESPeriod = 8541
    , sgMaxKESEvolutions = 28899
    , sgSlotLength =  secondsToNominalDiffTime 8
    , sgUpdateQuorum = 16991
    , sgMaxEntropicSupply = 71
    , sgProtocolParams = emptyPParams
        { _d  = unsafeBoundRational 1.9e-2
        , _maxBBSize = 239857
        , _maxBHSize = 217569
        }
    , sgGenDelegs = Map.fromList
                      [( genesisVerKeyHash
                       , GenDelegPair delegVerKeyHash delegVrfKeyHash)
                      ]
    , sgVestedDelegs = Map.fromList
                      [( vestedVerKeyHash
                       , VestedDelegPair vestedDelegVerKeyHash adelegVrfKeyHash)
                      ]
    , sgInitialFunds = Map.fromList [(initialFundedAddress,initialFunds)]
    , sgStaking = emptyGenesisStaking
    }
 where
  -- hash of the genesis verification key
  genesisVerKeyHash :: KeyHash Genesis StandardCrypto
  genesisVerKeyHash = KeyHash "23d51e91ae5adc7ae801e9de4cd54175fb7464ec2680b25686bbb194"
  -- hash of the delegators verification key
  delegVerKeyHash :: KeyHash GenesisDelegate StandardCrypto
  delegVerKeyHash = KeyHash "839b047f56e50654bdb504832186dc1ee0c73c8de2daec7ae6273827"
  delegVrfKeyHash :: Hash StandardCrypto (VerKeyVRF StandardCrypto)
  delegVrfKeyHash = "231391e7ec1c450a8518134cf6fad1a8e0ed7ffd66d740f8e8271347a6de7bf2"
  vestedVerKeyHash :: KeyHash Vested StandardCrypto
  vestedVerKeyHash = KeyHash "13d51e91ae5adc7ae801e6de4cd54175fb7464ec2680b25686bbb194"
  -- hash of the delegators verification key
  vestedDelegVerKeyHash :: KeyHash VestedDelegate StandardCrypto
  vestedDelegVerKeyHash = KeyHash "939b047f56e50654bdb504832186dc1ee0c73c8de2daec7ae6273827"
  adelegVrfKeyHash :: Hash StandardCrypto (VerKeyVRF StandardCrypto)
  adelegVrfKeyHash = "431391e7ec1c450a8518134cf6fad1a8e0ed7ffd66d740f8e8271347a6de7bf2"
  initialFundedAddress :: Addr StandardCrypto
  initialFundedAddress = Addr Testnet paymentCredential (StakeRefBase stakingCredential)
    where
      paymentCredential :: PaymentCredential StandardCrypto
      paymentCredential =
        KeyHashObj $ KeyHash
          "1c14ee8e58fbcbd48dc7367c95a63fd1d937ba989820015db16ac7e5"

      stakingCredential :: StakeCredential StandardCrypto
      stakingCredential =
        KeyHashObj $ KeyHash
          "e37a65ea2f9bcefb645de4312cf13d8ac12ae61cf242a9aa2973c9ee"

  initialFunds :: Coin
  initialFunds = Coin 12157196
