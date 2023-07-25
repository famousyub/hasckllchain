{- HLINT ignore "Use record patterns" -}
{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Reduce duplication" -}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bcc.Benchmarking.CliArgsScript
  (
    GeneratorCmd
  , parseGeneratorCmd
  , runPlainOldCliScript
  , runEraTransitionTest
  ) where

import           Prelude (error)
import           Data.Text (unpack)
import qualified Data.List.NonEmpty as NE

import           Control.Tracer (traceWith)

import           Bcc.Api
import           Bcc.Prelude hiding (option)

import           Shardagnostic.Network.NodeToClient (IOManager)

import           Bcc.Benchmarking.GeneratorTx.Benchmark
import           Bcc.Benchmarking.GeneratorTx (readSigningKey)
import           Bcc.Benchmarking.DSL
import           Bcc.Benchmarking.Tracer
import           Bcc.Benchmarking.GeneratorTx.Error (TxGenError(..))
import           Bcc.Benchmarking.GeneratorTx.LocalProtocolDefinition (CliError(..), runBenchmarkScriptWith)

runPlainOldCliScript :: IOManager -> GeneratorCmd -> IO (Either CliError ())
runPlainOldCliScript
  iocp
  (GenerateCmd
     logConfigFile
     socketFile
     benchmarkEra
     cliPartialBenchmark
     fundOptions
  )
  = runExceptT $ runBenchmarkScriptWith iocp logConfigFile socketFile
      $ plainOldCliScript cliPartialBenchmark benchmarkEra fundOptions

runEraTransitionTest :: IOManager -> GeneratorCmd -> IO (Either CliError ())
runEraTransitionTest
  iocp
  (GenerateCmd
     logConfigFile
     socketFile
     _benchmarkEra
     cliPartialBenchmark
     fundOptions
  )
  = runExceptT $ runBenchmarkScriptWith iocp logConfigFile socketFile
      $ eraTransitionTest cliPartialBenchmark fundOptions

plainOldCliScript :: PartialBenchmark -> AnyBccEra -> GeneratorFunds -> BenchmarkScript ()
plainOldCliScript _ _ (FundsUtxo _ _ _) _ = error "plainOldCliScript FundsUtxo not supported"
plainOldCliScript _ _ (FundsSplitUtxo _ _) _ = error "plainOldCliScript FundsSplitUtxo not supported"
plainOldCliScript cliPartialBenchmark benchmarkEra (FundsGenesis keyFile) (tracers, dslSet) = do
  case benchmarkEra of
      AnyBccEra AurumEra  -> error "AurumEra not supported"
      AnyBccEra ColeEra   -> error "ColeEra not supported"
      AnyBccEra SophieEra -> do
        myTracer "POScript :: SophieEra"
        genericScript $ getDSL dslSet SophieEra
      AnyBccEra EvieEra -> do
        myTracer "POScript  :: EvieEra"
        genericScript $ getDSL dslSet EvieEra
      AnyBccEra JenEra    -> do
        myTracer "POScript  :: JenEra"
        genericScript $ getDSL dslSet JenEra
 where
  myTracer msg = liftIO $ traceWith (btTxSubmit_ tracers) $ TraceBenchTxSubDebug msg
  genericScript :: forall era. DSL era -> ExceptT TxGenError IO ()
  genericScript (DSL{..}) = do
    b <- case mkBenchmark (defaultBenchmark <> cliPartialBenchmark) of
       Left e -> error $ "Incomplete benchmark spec (is defaultBenchmark complete?):  " <> unpack e
       Right b -> return b
    let
      fees = bTxFee b
      coolDownDelay = bInitCooldown b

    key <- readSigningKey keyFile
    let
      globalOutAddr = keyAddress key    -- A globalOutAddr is used for all TXs in the generator.

    myTracer "POScript: securing funds"
    firstUTxO <- secureGenesisFund fees (bInitialTTL b) key globalOutAddr

    myTracer $ "******* Tx generator: waiting for first UTxO" ++ show coolDownDelay ++ "s *******"
    coolDown coolDownDelay
    funds <- splitFunds fees (bTxCount b) (bTxFanIn b) key globalOutAddr firstUTxO
    myTracer $ "******* Tx generator: waiting for funds split" ++ show coolDownDelay ++ "s *******"
    coolDown coolDownDelay

    myTracer "POScript: pre-computing transactions"
    finalTransactions <- txGenerator (bTxFee b) (bTxCount b) (bTxFanIn b) (bTxFanOut b) (bTxExtraPayload b)
      globalOutAddr key (fromIntegral $ NE.length $ bTargets b) funds

    myTracer "POScript: sending transactions"
    runBenchmark (bTargets b) (bTps b) (bErrorPolicy b) finalTransactions

eraTransitionTest :: PartialBenchmark -> GeneratorFunds -> BenchmarkScript ()
eraTransitionTest _ (FundsUtxo _ _ _) _ = error "eraTransitionTest FundsUtxo not supported"
eraTransitionTest _ (FundsSplitUtxo _ _) _ = error "eraTransitionTest FundsSplitUtxo not supported"
eraTransitionTest cliPartialBenchmark (FundsGenesis keyFile) (tracers, dslSet) = do
  b <- case mkBenchmark (defaultBenchmark <> cliPartialBenchmark) of
     Left e -> error $ "Incomplete benchmark spec (is defaultBenchmark complete?):  " <> unpack e
     Right b -> return b
  let
    fees = bTxFee b
    coolDownDelay = bInitCooldown b

  key <- readSigningKey keyFile
  let
    addr_sophie :: AddressInEra SophieEra
    addr_sophie = keyAddress key

    addr_jen :: AddressInEra JenEra
    addr_jen = keyAddress_jen key

  myTracer "POScript: securing funds"

  firstUTxO <- secureGenesisFund fees (bInitialTTL b) key addr_sophie

  myTracer $ "******* Tx generator: waiting for first UTxO" ++ show coolDownDelay ++ "s *******"
  coolDown coolDownDelay
  [fund1,fund2] <- splitFunds fees 2 (bTxFanIn b) key addr_sophie firstUTxO
  funds_sophie <- splitFunds fees (bTxCount b) (bTxFanIn b) key addr_sophie fund1
  funds_jen   <- splitFunds fees (bTxCount b) (bTxFanIn b) key addr_sophie fund2

  myTracer $ "******* Tx generator: waiting for funds split" ++ show coolDownDelay ++ "s *******"
  coolDown coolDownDelay

  myTracer "POScript: pre-computing transactions Sophie"
  tx1 <- txGenerator (bTxFee b) (bTxCount b) (bTxFanIn b) (bTxFanOut b) (bTxExtraPayload b)
             addr_sophie key (fromIntegral $ NE.length $ bTargets b) funds_sophie
  myTracer "POScript: sending transactions Sophie"
  runBenchmark (bTargets b) (bTps b) (bErrorPolicy b) tx1

  myTracer "POScript: pre-computing transactions Jen"
  (tx2 :: [Tx JenEra]) <- txGenerator_jen  (bTxFee b) (bTxCount b) (bTxFanIn b) (bTxFanOut b) (bTxExtraPayload b)
                              addr_jen key (fromIntegral $ NE.length $ bTargets b) funds_jen
  myTracer "POScript: sending transactions Jen"
  runBenchmark_jen (bTargets b) (bTps b) (bErrorPolicy b) tx2
 where
  DSL {..} = getDSL dslSet SophieEra
  DSL {
    runBenchmark = runBenchmark_jen
   , txGenerator  = txGenerator_jen
   , keyAddress   = keyAddress_jen
   } =getDSL dslSet JenEra

  myTracer msg = liftIO $ traceWith (btTxSubmit_ tracers) $ TraceBenchTxSubDebug msg
