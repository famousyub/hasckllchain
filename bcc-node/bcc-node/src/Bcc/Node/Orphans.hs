{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Bcc.Node.Orphans () where

import           Bcc.Prelude
import           Prelude (fail)

import           Bcc.Api.Orphans ()

import           Data.Aeson.Types
import qualified Data.Text as Text

import           Bcc.BM.Data.Tracer (TracingVerbosity (..))
import qualified Bcc.Chain.Update as Update
import           Bcc.Ledger.Crypto (StandardCrypto)
import qualified Sophie.Spec.Ledger.CompactAddr as Sophie

instance FromJSON TracingVerbosity where
  parseJSON (String str) = case str of
    "MinimalVerbosity" -> pure MinimalVerbosity
    "MaximalVerbosity" -> pure MaximalVerbosity
    "NormalVerbosity" -> pure NormalVerbosity
    err -> fail $ "Parsing of TracingVerbosity failed, "
                <> Text.unpack err <> " is not a valid TracingVerbosity"
  parseJSON invalid  = fail $ "Parsing of TracingVerbosity failed due to type mismatch. "
                           <> "Encountered: " <> show invalid

deriving instance Show TracingVerbosity

instance ToJSON (Sophie.CompactAddr StandardCrypto) where
  toJSON = toJSON . Sophie.decompactAddr

--Not currently needed, but if we do need it, this is the general instance.
--instance (ToJSON a, Ledger.Compactible a) => ToJSON (Ledger.CompactForm a) where
--  toJSON = toJSON  . Ledger.fromCompact

instance FromJSON Update.ApplicationName where
  parseJSON (String x) = pure $ Update.ApplicationName x
  parseJSON invalid  =
    fail $ "Parsing of application name failed due to type mismatch. "
    <> "Encountered: " <> show invalid
