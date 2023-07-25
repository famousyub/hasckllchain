{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Bcc.CLI.Sophie.Orphans () where

import           Bcc.Prelude

import           Control.SetAlgebra as SetAlgebra
import           Data.Aeson
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Short as SBS
import qualified Data.Text.Encoding as Text

import           Bcc.Api.Orphans ()

import           Shardagnostic.Consensus.Cole.Ledger.Block (ColeHash (..))
import           Shardagnostic.Consensus.HardFork.Combinator (OneEraHash (..))
import           Shardagnostic.Consensus.Sophie.Eras (StandardCrypto)
import           Shardagnostic.Consensus.Sophie.Ledger.Block (SophieHash (..))
import           Shardagnostic.Network.Block (BlockNo (..), HeaderHash, Tip (..))

import           Bcc.Ledger.AuxiliaryData (AuxiliaryDataHash (..))
import qualified Bcc.Ledger.Crypto as CC (Crypto)
import           Bcc.Protocol.TOptimum (PoolDistr (..))
import           Bcc.Protocol.TOptimum.BHeader (HashHeader (..))

import qualified Bcc.Ledger.Credential as Ledger
import qualified Sophie.Spec.Ledger.API.Protocol as Ledger
import qualified Sophie.Spec.Ledger.EpochBoundary as Ledger
import qualified Sophie.Spec.Ledger.Rewards as Ledger
import qualified Bcc.Protocol.TOptimum.Rules.Prtcl as Ledger
import qualified Sophie.Spec.Ledger.STS.Tickn as Ledger
import           Sophie.Spec.Ledger.TxBody (TxId (..))

import qualified Bcc.Ledger.Jen.Value as Ledger.Jen

instance ToJSON (OneEraHash xs) where
  toJSON = toJSON
         . Text.decodeLatin1
         . Base16.encode
         . SBS.fromShort
         . getOneEraHash

deriving newtype instance ToJSON ColeHash

-- This instance is temporarily duplicated in bcc-config

instance ToJSON (HeaderHash blk) => ToJSON (Tip blk) where
  toJSON TipGenesis = object [ "genesis" .= True ]
  toJSON (Tip slotNo headerHash blockNo) =
    object
      [ "slotNo"     .= slotNo
      , "headerHash" .= headerHash
      , "blockNo"    .= blockNo
      ]

-- This instance is temporarily duplicated in bcc-config
deriving newtype instance ToJSON BlockNo

--
-- Simple newtype wrappers JSON conversion
--

deriving newtype instance CC.Crypto crypto => ToJSON (TxId crypto)

deriving newtype instance CC.Crypto crypto => ToJSON (SophieHash crypto)
deriving newtype instance CC.Crypto crypto => ToJSON (HashHeader crypto)

deriving newtype instance ToJSON (AuxiliaryDataHash StandardCrypto)
deriving newtype instance ToJSON Ledger.LogWeight
deriving newtype instance ToJSON (PoolDistr StandardCrypto)

deriving newtype instance ToJSON (Ledger.Stake StandardCrypto)

deriving instance ToJSON (Ledger.StakeReference StandardCrypto)

deriving instance ToJSON (Ledger.PrtclState StandardCrypto)
deriving instance ToJSON Ledger.TicknState
deriving instance ToJSON (Ledger.ChainDepState StandardCrypto)

deriving instance ToJSONKey Ledger.Ptr

deriving newtype  instance ToJSON    (Ledger.Jen.PolicyID StandardCrypto)

instance (ToJSONKey k, ToJSON v) => ToJSON (SetAlgebra.BiMap v k v) where
  toJSON = toJSON . SetAlgebra.forwards -- to normal Map
