{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Bcc.Benchmarking.DSL
where

import           Prelude (error)
import           Bcc.Prelude

import           Bcc.Api
import           Bcc.Benchmarking.Tracer
import           Bcc.Benchmarking.Types (SubmissionErrorPolicy, NodeIPv4Address)
import           Bcc.Benchmarking.GeneratorTx
import           Bcc.Benchmarking.GeneratorTx.Tx

type ScriptM a = ExceptT TxGenError IO a
type BenchmarkScript a = (BenchTracers, MonoDSLs) ->  ScriptM a

type MonoDSLs = (DSL SophieEra, DSL EvieEra, DSL JenEra)

getDSL :: MonoDSLs -> BccEra era -> DSL era
getDSL _         ColeEra = error "ColeEra not supported"
getDSL (x, _, _) SophieEra = x
getDSL (_, x, _) EvieEra = x
getDSL (_, _, x) JenEra    = x
getDSL _         AurumEra = error "AurumEra not supported" -- use json mode

type Fee = Entropic
type TTL = SlotNo

type SecureGenesisFund era =
     Fee
  -> TTL
  -> SigningKey PaymentKey
  -> AddressInEra era
  -> ScriptM Fund

type SplitFunds era =
     Entropic
  -> NumberOfTxs
  -> NumberOfInputsPerTx
  -> SigningKey PaymentKey
  -> AddressInEra era
  -> Fund
  -> ScriptM [Fund]

-- txGenerator is basically pure except for logging.
type TxGenerator era =
     Entropic
  -> NumberOfTxs
  -> NumberOfInputsPerTx
  -> NumberOfOutputsPerTx
  -> TxAdditionalSize
  -> AddressInEra era
  -> SigningKey PaymentKey
  -> Int
  -> [Fund]
  -> ScriptM [Tx era]

type RunBenchmark era =
     NonEmpty NodeIPv4Address
  -> TPSRate
  -> SubmissionErrorPolicy
  -> [Tx era]
  -> ScriptM ()

type KeyAddress era = SigningKey PaymentKey -> AddressInEra era

data DSL era = DSL {
    keyAddress :: !(KeyAddress era)
  , secureGenesisFund :: !(SecureGenesisFund era)
  , splitFunds :: !(SplitFunds era)
  , txGenerator :: !(TxGenerator era)
  , runBenchmark :: !(RunBenchmark era)
  }

coolDown :: InitCooldown -> ScriptM ()
coolDown (InitCooldown t) = liftIO $ threadDelay $ 1000 * 1000 * t
