{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Bcc.Benchmarking.Script.Action
where

import           Prelude
import           GHC.Generics
import           Data.Functor.Identity
import           Data.Dependent.Sum (DSum(..))

import           Bcc.Benchmarking.ShardagnosticImports (SigningKeyFile)
import           Bcc.Api (AnyBccEra, Entropic)

import           Bcc.Benchmarking.Script.Env
import           Bcc.Benchmarking.Script.Store
import           Bcc.Benchmarking.Script.Core
import           Bcc.Benchmarking.Types (TPSRate, NumberOfTxs)

data Action where
  Set                :: !SetKeyVal -> Action
--  Declare            :: SetKeyVal   -> Action --declare (once): error if key was set before
  StartProtocol      :: !FilePath -> Action
  Delay              :: !Double -> Action
  ReadSigningKey     :: !KeyName -> !SigningKeyFile -> Action
  SecureGenesisFund  :: !FundName -> !KeyName -> !KeyName -> Action
  SplitFund          :: [FundName] -> !KeyName -> !FundName -> Action
  SplitFundToList    :: !FundListName -> !KeyName -> !FundName -> Action
  PrepareTxList      :: !TxListName -> !KeyName -> !FundListName -> Action
  AsyncBenchmark     :: !ThreadName -> !TxListName -> !TPSRate -> Action
  ImportGenesisFund  :: !KeyName -> !KeyName -> Action
  CreateChange       :: !Entropic -> !Int -> Action
  RunBenchmark       :: !ThreadName -> !NumberOfTxs -> !TPSRate -> Action
  WaitBenchmark      :: !ThreadName -> Action
  CancelBenchmark    :: !ThreadName -> Action
  Reserved           :: [String] -> Action
  WaitForEra         :: !AnyBccEra -> Action
  deriving (Show, Eq)

deriving instance Generic Action

action :: Action -> ActionM ()
action a = case a of
  Set (key :=> (Identity val)) -> set (User key) val
  StartProtocol filePath -> startProtocol filePath
  ReadSigningKey name filePath -> readSigningKey name filePath
  SecureGenesisFund fundName fundKey genesisKey -> secureGenesisFund fundName fundKey genesisKey
  SplitFund newFunds newKey sourceFund -> splitFund  newFunds newKey sourceFund
  SplitFundToList fundList destKey sourceFund -> splitFundToList fundList destKey sourceFund
  Delay t -> delay t
  PrepareTxList name key fund -> prepareTxList name key fund
  AsyncBenchmark thread txs tps -> asyncBenchmark thread txs tps
  ImportGenesisFund genesisKey fundKey -> importGenesisFund genesisKey fundKey
  CreateChange value count -> createChange value count
  RunBenchmark thread count tps -> runBenchmark thread count tps
  WaitBenchmark thread -> waitBenchmark thread
  CancelBenchmark thread -> cancelBenchmark thread
  WaitForEra era -> waitForEra era
  Reserved options -> reserved options
