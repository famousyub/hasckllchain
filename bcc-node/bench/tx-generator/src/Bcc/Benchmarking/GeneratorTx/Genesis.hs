{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bcc.Benchmarking.GeneratorTx.Genesis
  ( genesisFundForKey
  , genesisExpenditure
  )
where

import           Bcc.Prelude hiding (TypeError, filter)
import           Prelude (error, filter)
import qualified Data.Map.Strict as Map

import           Control.Arrow ((***))
import           Bcc.Api
import           Bcc.Api.Sophie (fromSophieEntropic, fromSophieStakeReference, fromSophiePaymentCredential)

import           Bcc.Benchmarking.GeneratorTx.Tx

import           Sophie.Spec.Ledger.API (Addr(..), SophieGenesis, sgInitialFunds)
import           Shardagnostic.Consensus.Sophie.Eras (StandardSophie)

genesisFunds :: forall era. IsSophieBasedEra era
  => NetworkId -> SophieGenesis StandardSophie -> [(AddressInEra era, Entropic)]
genesisFunds networkId g
 = map (castAddr *** fromSophieEntropic)
     $ Map.toList
     $ sgInitialFunds g
 where
  castAddr (Addr _ pcr stref)
    = sophieAddressInEra $ makeSophieAddress networkId (fromSophiePaymentCredential pcr) (fromSophieStakeReference stref)
  castAddr _ = error "castAddr:  unhandled Sophie.Addr case"

genesisFundForKey :: forall era. IsSophieBasedEra era
  => NetworkId
  -> SophieGenesis StandardSophie
  -> SigningKey PaymentKey
  -> (AddressInEra era, Entropic)
genesisFundForKey networkId genesis key
  = fromMaybe (error "No genesis funds for signing key.")
    . head
    . filter (isTxOutForKey . fst)
    $ genesisFunds networkId genesis
 where
  isTxOutForKey addr = keyAddress networkId key == addr

genesisExpenditure ::
     IsSophieBasedEra era
  => NetworkId
  -> SigningKey PaymentKey
  -> AddressInEra era
  -> Entropic
  -> Entropic
  -> SlotNo
  -> (Tx era, Fund)
genesisExpenditure networkId key addr coin fee ttl = (tx, fund)
 where
  tx = mkGenesisTransaction (castKey key) 0 ttl fee [ pseudoTxIn ] [ txout ]

  value = mkTxOutValueBccOnly $ coin - fee
  txout = TxOut addr value TxOutDatumHashNone

  pseudoTxIn = genesisUTxOPseudoTxIn networkId
                 (verificationKeyHash $ getVerificationKey $ castKey key)

  castKey :: SigningKey PaymentKey -> SigningKey GenesisUTxOKey
  castKey(PaymentSigningKey skey) = GenesisUTxOSigningKey skey

  fund = mkFund (TxIn (getTxId $ getTxBody tx) (TxIx 0)) value
