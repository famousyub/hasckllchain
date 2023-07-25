{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Bcc.Benchmarking.Script.Store
where

import           Prelude

import           Data.Constraint.Extras.TH (deriveArgDict)
import           Data.GADT.Compare.TH (deriveGCompare, deriveGEq)
import           Data.GADT.Show.TH (deriveGShow)

import           Bcc.Api as Bcc (InAnyBccEra(..), Tx)
import           Bcc.Node.Protocol.Types (SomeConsensusProtocol)

import           Bcc.Benchmarking.Script.Setters as Setters
import           Bcc.Benchmarking.ShardagnosticImports as Bcc
                    ( LoggingLayer, SophieGenesis, StandardSophie
                    , NetworkId, SigningKey, PaymentKey)

import           Bcc.Benchmarking.GeneratorTx as Core (AsyncBenchmarkControl)
import qualified Bcc.Benchmarking.GeneratorTx.Tx as Core (Fund)
import           Bcc.Benchmarking.Tracer as Core (BenchTracers)
import           Bcc.Benchmarking.Wallet as Wallet

type Fund = (Core.Fund, SigningKey PaymentKey)

data Store v where
  User         :: Setters.Tag x -> Store x
  GlobalWallet :: Store WalletRef
  LoggingLayer :: Store LoggingLayer
  Protocol     :: Store SomeConsensusProtocol
  BenchTracers :: Store Core.BenchTracers
  NetworkId    :: Store Bcc.NetworkId -- could be in Setters (just need JSON instance)
  Genesis      :: Store (SophieGenesis StandardSophie)
  Named        :: Name x -> Store x

data Name x where
  KeyName      :: !String -> Name (SigningKey PaymentKey)
  FundName     :: !String -> Name Fund
  FundListName :: !String -> Name [Fund]
  TxListName   :: !String -> Name (InAnyBccEra TxList)
  ThreadName   :: !String -> Name AsyncBenchmarkControl

type KeyName      = Name (SigningKey PaymentKey)
type FundName     = Name Fund
type FundListName = Name [Fund]
type TxListName   = Name (InAnyBccEra TxList)
type ThreadName   = Name AsyncBenchmarkControl

newtype TxList era = TxList [Tx era]

-- Remember when debugging at 4:00AM :
-- TH-Haskell is imperative: It breaks up Main into smaller binding groups!
-- This means declarations below a splice are not visible above.
-- The order of splices & declarations matters.

deriveGEq ''Name
deriveGCompare ''Name
deriveGShow ''Name
deriveArgDict ''Name
deriving instance Show (Name x)
deriving instance Eq (Name x)

deriveGEq ''Store
deriveGCompare ''Store
deriveGShow ''Store
deriveArgDict ''Store

deriving instance Show (Store v)
deriving instance Eq (Store x)
