{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Bcc.Benchmarking.GeneratorTx.LocalProtocolDefinition
  ( CliError (..)
  , runBenchmarkScriptWith
  , startProtocol
  ) where

import           Prelude (error, show)
import           Paths_tx_generator (version)

import           Data.Version (showVersion)
import           Data.Text (pack)

import           Bcc.Prelude hiding (TypeError, show)
import           Control.Monad.Trans.Except.Extra (firstExceptT)

import           Shardagnostic.Consensus.Config
                   ( configBlock, configCodec)
import           Shardagnostic.Consensus.Config.SupportsNode
                   (ConfigSupportsNode(..), getNetworkMagic)
import           Shardagnostic.Network.NodeToClient (IOManager)
import           Shardagnostic.Network.Block (MaxSlotNo(..))

import           Bcc.Api

import qualified Bcc.Chain.Genesis as Genesis

import           Bcc.Node.Configuration.Logging
import           Bcc.Node.Configuration.POM
import           Bcc.Node.Protocol.Bcc
import           Bcc.Node.Protocol.Types (SomeConsensusProtocol)
import           Bcc.Node.Types

import           Bcc.Benchmarking.DSL
import           Bcc.Benchmarking.Tracer

import           Bcc.Benchmarking.GeneratorTx.NodeToNode
import           Bcc.Benchmarking.ShardagnosticImports (getGenesis, protocolToTopLevelConfig, protocolToNetworkId)

import qualified Bcc.Benchmarking.GeneratorTx as GeneratorTx
import qualified Bcc.Benchmarking.GeneratorTx.Tx as GeneratorTx

mangleLocalProtocolDefinition ::
     SomeConsensusProtocol
  -> IOManager
  -> SocketPath
  -> BenchTracers
  -> MonoDSLs
mangleLocalProtocolDefinition
  ptcl
  iom
  (SocketPath sock)
  tracers
  = (DSL {..}, DSL {..}, DSL {..})
 where
  topLevelConfig = protocolToTopLevelConfig ptcl

  localConnectInfo :: LocalNodeConnectInfo BccMode
  localConnectInfo = LocalNodeConnectInfo
     (BccModeParams (EpochSlots 21600))        -- TODO: get this from genesis
     networkId
     sock

  connectClient :: ConnectClient
  connectClient  = benchmarkConnectTxSubmit
                     iom
                     (btConnect_ tracers)
                     (btSubmission2_ tracers)
                     (configCodec topLevelConfig)
                     (getNetworkMagic $ configBlock topLevelConfig)

  networkId = protocolToNetworkId ptcl

  keyAddress :: IsSophieBasedEra era => KeyAddress era
  keyAddress = GeneratorTx.keyAddress networkId

  secureGenesisFund :: IsSophieBasedEra era => SecureGenesisFund era
  secureGenesisFund = GeneratorTx.secureGenesisFund
              (btTxSubmit_ tracers)
              (submitTxToNodeLocal localConnectInfo)
              networkId
              (getGenesis ptcl)

  splitFunds :: IsSophieBasedEra era => SplitFunds era
  splitFunds = GeneratorTx.splitFunds
              (btTxSubmit_ tracers)
              (submitTxToNodeLocal localConnectInfo)

  txGenerator :: IsSophieBasedEra era => TxGenerator era
  txGenerator = GeneratorTx.txGenerator (btTxSubmit_ tracers)

  runBenchmark :: IsSophieBasedEra era => RunBenchmark era
  runBenchmark = GeneratorTx.runBenchmark (btTxSubmit_ tracers) (btN2N_ tracers) connectClient

runBenchmarkScriptWith ::
     IOManager
  -> FilePath
  -> SocketPath
  -> BenchmarkScript a
  -> ExceptT CliError IO a
runBenchmarkScriptWith iocp logConfigFile socketFile script = do
  (loggingLayer, ptcl) <- startProtocol logConfigFile
  let tracers :: BenchTracers
      tracers = createTracers loggingLayer
      dslSet :: MonoDSLs
      dslSet = mangleLocalProtocolDefinition ptcl iocp socketFile tracers
  res <- firstExceptT BenchmarkRunnerError $ script (tracers, dslSet)
  liftIO $ do
          threadDelay (200*1000) -- Let the logging layer print out everything.
          shutdownLoggingLayer loggingLayer
  return res

startProtocol
  :: FilePath
  -> ExceptT CliError IO (LoggingLayer, SomeConsensusProtocol)
startProtocol logConfigFile = do
  nc <- liftIO $ mkNodeConfig logConfigFile
  case ncProtocolConfig nc of
    NodeProtocolConfigurationCole _    -> error "NodeProtocolConfigurationCole not supported"
    NodeProtocolConfigurationSophie _  -> error "NodeProtocolConfigurationSophie not supported"
    NodeProtocolConfigurationBcc coleConfig sophieConfig aurumConfig hardforkConfig -> do
        ptcl :: SomeConsensusProtocol <- firstExceptT (ProtocolInstantiationError . pack . show) $
                  mkSomeConsensusProtocolBcc coleConfig sophieConfig aurumConfig hardforkConfig Nothing
        
        loggingLayer <- mkLoggingLayer nc ptcl
        return (loggingLayer, ptcl)
 where
  mkLoggingLayer :: NodeConfiguration -> SomeConsensusProtocol -> ExceptT CliError IO LoggingLayer
  mkLoggingLayer nc ptcl =
    firstExceptT (\(ConfigErrorFileNotFound fp) -> ConfigNotFoundError fp) $
    createLoggingLayer (pack $ showVersion version) nc ptcl

  mkNodeConfig :: FilePath -> IO NodeConfiguration
  mkNodeConfig logConfig = do
   let configFp = ConfigYamlFilePath logConfig
       filesPc = defaultPartialNodeConfiguration
                 { pncProtocolFiles = Last . Just $
                   ProtocolFilepaths
                   { coleCertFile = Just ""
                   , coleKeyFile = Just ""
                   , sophieKESFile = Just ""
                   , sophieVRFFile = Just ""
                   , sophieCertFile = Just ""
                   , sophieBulkCredsFile = Just ""
                   }
                 , pncValidateDB = Last $ Just False
                 , pncShutdownIPC = Last $ Just Nothing
                 , pncShutdownOnSlotSynced = Last $ Just NoMaxSlotNo
                 , pncConfigFile = Last $ Just configFp
                 }
   configYamlPc <- parseNodeConfigurationFP . Just $ configFp
   case makeNodeConfiguration $ configYamlPc <> filesPc of
      Left err -> panic $ "Error in creating the NodeConfiguration: " <> pack err
      Right nc' -> return nc'

data CliError  =
    GenesisReadError !FilePath !Genesis.GenesisDataError
  | FileNotFoundError !FilePath
  | ConfigNotFoundError !FilePath
  | ProtocolInstantiationError !Text
  | BenchmarkRunnerError !GeneratorTx.TxGenError
  deriving stock Show
