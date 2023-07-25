{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bcc.Node.Protocol.Aurum
  ( AurumProtocolInstantiationError(..)
    -- * Reusable parts
  , readGenesis
  , validateGenesis
  ) where

import           Prelude (String)
import           Bcc.Prelude

import           Bcc.Api

import qualified Bcc.Ledger.Aurum.Genesis as Aurum

import           Bcc.Node.Types
import           Bcc.Node.Orphans ()

import           Bcc.Tracing.OrphanInstances.HardFork ()
import           Bcc.Tracing.OrphanInstances.Sophie ()

import           Bcc.Node.Protocol.Sophie (readGenesisAny, GenesisReadError)

--
-- Aurum genesis
--

readGenesis :: GenesisFile
            -> Maybe GenesisHash
            -> ExceptT GenesisReadError IO
                       (Aurum.AurumGenesis, GenesisHash)
readGenesis = readGenesisAny

validateGenesis :: Aurum.AurumGenesis
                -> ExceptT AurumProtocolInstantiationError IO ()
validateGenesis _ = return () --TODO aurum: do the validation

data AurumProtocolInstantiationError
  = InvalidCostModelError !FilePath
  | CostModelExtractionError !FilePath
  | AurumCostModelFileError !(FileError ())
  | AurumCostModelDecodeError !FilePath !String
  deriving Show

instance Error AurumProtocolInstantiationError where
  displayError (InvalidCostModelError fp) =
    "Invalid cost model: " <> show fp
  displayError (CostModelExtractionError fp) =
    "Error extracting the cost model at: " <> show fp
  displayError (AurumCostModelFileError err) =
    displayError err
  displayError (AurumCostModelDecodeError fp err) =
    "Error decoding cost model at: " <> show fp <> " Error: " <> err

