{- HLINT ignore "Eta reduce" -}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Bcc.Benchmarking.ShardagnosticImports
  (
    BccBlock
  , LocalSubmitTx
  , LoggingLayer
  , PaymentKey
  , SophieGenesis
  , SigningKey
  , SigningKeyFile
  , StandardSophie
  , NetworkId
  , getGenesis
  , makeLocalConnectInfo
  , protocolToTopLevelConfig
  , protocolToNetworkId
  , protocolToCodecConfig
  , submitTxToNodeLocal
  ) where

import           Prelude

import           Shardagnostic.Consensus.Block.Abstract
import qualified Shardagnostic.Consensus.Bcc as Consensus
import           Shardagnostic.Consensus.Config (TopLevelConfig, configBlock, configCodec)
import           Shardagnostic.Consensus.Config.SupportsNode
                 (ConfigSupportsNode(..), getNetworkMagic)
import           Shardagnostic.Consensus.Node (ProtocolInfo(..))
import           Shardagnostic.Consensus.Sophie.Protocol (StandardCrypto)
import           Shardagnostic.Consensus.Sophie.Eras (StandardSophie)
import           Shardagnostic.Network.Protocol.LocalTxSubmission.Type (SubmitResult (..))

import           Bcc.Node.Configuration.Logging (LoggingLayer)
import           Bcc.Node.Protocol.Types ( SomeConsensusProtocol(..))

import           Bcc.Api.Sophie (BccMode)
import           Bcc.CLI.Types (SigningKeyFile)

import           Bcc.Api (NetworkId(..), LocalNodeConnectInfo(..), ConsensusModeParams(..), EpochSlots(..)
                             , TxInMode, TxValidationErrorInMode
                             , SigningKey, PaymentKey
                             , submitTxToNodeLocal)
import           Bcc.Api.Protocol.Types (BlockType(..), ProtocolInfoArgs(..), protocolInfo)

import           Sophie.Spec.Ledger.Genesis (SophieGenesis)

type BccBlock = Consensus.BccBlock StandardCrypto

toProtocolInfo :: SomeConsensusProtocol -> ProtocolInfo IO BccBlock
toProtocolInfo (SomeConsensusProtocol BccBlockType info) = protocolInfo info
toProtocolInfo _ = error "toProtocolInfo unkown protocol"

getGenesis :: SomeConsensusProtocol -> SophieGenesis StandardSophie
getGenesis (SomeConsensusProtocol BccBlockType info) = sophieBasedGenesis
 where
  (ProtocolInfoArgsBcc
   _
   Consensus.ProtocolParamsSophieBased{Consensus.sophieBasedGenesis}
    _ _ _ _ _ _ _ _) = info
getGenesis (SomeConsensusProtocol _ _ ) = error "getGenesis (SomeConsensusProtocol _ _ ) unknown protocol"

protocolToTopLevelConfig :: SomeConsensusProtocol -> TopLevelConfig BccBlock
protocolToTopLevelConfig ptcl = pInfoConfig
 where
   ProtocolInfo {pInfoConfig} = toProtocolInfo ptcl

protocolToCodecConfig :: SomeConsensusProtocol -> CodecConfig BccBlock
protocolToCodecConfig = configCodec . protocolToTopLevelConfig

protocolToNetworkId :: SomeConsensusProtocol -> NetworkId
protocolToNetworkId ptcl
  = Testnet $ getNetworkMagic $ configBlock $ protocolToTopLevelConfig ptcl

makeLocalConnectInfo :: NetworkId -> FilePath -> LocalNodeConnectInfo BccMode
makeLocalConnectInfo networkId sock
  = LocalNodeConnectInfo
      (BccModeParams (EpochSlots 21600))
      networkId
      sock

type LocalSubmitTx = (TxInMode BccMode -> IO (SubmitResult (TxValidationErrorInMode BccMode)))
