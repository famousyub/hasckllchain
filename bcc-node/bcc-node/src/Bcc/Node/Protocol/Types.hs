{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Bcc.Node.Protocol.Types
  ( Protocol(..)
  , SomeConsensusProtocol(..)
  ) where

import           Bcc.Prelude

import           Control.Monad.Fail (fail)
import           Data.Aeson
import           NoThunks.Class (NoThunks)

import qualified Bcc.Api.Protocol.Types as Bcc

import           Bcc.Node.Orphans ()
import           Bcc.Tracing.Constraints (TraceConstraints)
import           Bcc.Tracing.Metrics (HasKESInfo, HasKESMetricsData)

data Protocol = ColeProtocol
              | SophieProtocol
              | BccProtocol
  deriving (Eq, Show, Generic)

deriving instance NFData Protocol
deriving instance NoThunks Protocol

instance FromJSON Protocol where
  parseJSON =
    withText "Protocol" $ \str -> case str of

      -- The new names
      "Cole" -> pure ColeProtocol
      "Sophie" -> pure SophieProtocol
      "Bcc" -> pure BccProtocol

      -- The old names
      "RealPBFT" -> pure ColeProtocol
      "TOptimum" -> pure SophieProtocol

      _ -> fail $ "Parsing of Protocol failed. "
                <> show str <> " is not a valid protocol"



data SomeConsensusProtocol where

     SomeConsensusProtocol :: forall blk. ( Bcc.Protocol IO blk
                                          , HasKESMetricsData blk
                                          , HasKESInfo blk
                                          , TraceConstraints blk
                                          )
                           => Bcc.BlockType blk
                           -> Bcc.ProtocolInfoArgs IO blk
                           -> SomeConsensusProtocol
