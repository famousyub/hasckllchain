{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_GHC -Wno-noncanonical-monoid-instances #-}

module Bcc.Node.Configuration.POM
  ( NodeConfiguration (..)
  , PartialNodeConfiguration(..)
  , defaultPartialNodeConfiguration
  , lastOption
  , makeNodeConfiguration
  , parseNodeConfigurationFP
  , pncProtocol
  , ncProtocol
  )
where

import           Bcc.Prelude
import           Prelude (String)

import           Control.Monad (fail)
import           Data.Aeson
import           Data.Yaml (decodeFileThrow)
import           Generic.Data (gmappend)
import           Generic.Data.Orphans ()
import           Options.Applicative
import           System.FilePath (takeDirectory, (</>))
import           System.Posix.Types (Fd (..))

import qualified Bcc.Chain.Update as Cole
import           Bcc.Crypto (RequiresNetworkMagic (..))
import           Bcc.Node.Protocol.Types (Protocol (..))
import           Bcc.Node.Types
import           Bcc.Tracing.Config
import           Shardagnostic.Consensus.Storage.LedgerDB.DiskPolicy (SnapshotInterval (..))
import           Shardagnostic.Network.Block (MaxSlotNo (..))
import           Shardagnostic.Network.NodeToNode (DiffusionMode (..))

data NodeConfiguration
  = NodeConfiguration
      {  ncNodeIPv4Addr    :: !(Maybe NodeHostIPv4Address)
      ,  ncNodeIPv6Addr    :: !(Maybe NodeHostIPv6Address)
      ,  ncNodePortNumber  :: !(Maybe PortNumber)
          -- | Filepath of the configuration yaml file. This file determines
          -- all the configuration settings required for the bcc node
          -- (logging, tracing, protocol, slot length etc)
       , ncConfigFile      :: !ConfigYamlFilePath
       , ncTopologyFile    :: !TopologyFile
       , ncDatabaseFile    :: !DbFile
       , ncProtocolFiles   :: !ProtocolFilepaths
       , ncValidateDB      :: !Bool
       , ncShutdownIPC     :: !(Maybe Fd)
       , ncShutdownOnSlotSynced :: !MaxSlotNo

        -- Protocol-specific parameters:
       , ncProtocolConfig :: !NodeProtocolConfiguration

         -- Node parameters, not protocol-specific:
       , ncSocketPath       :: !(Maybe SocketPath)
       , ncDiffusionMode    :: !DiffusionMode
       , ncSnapshotInterval :: !SnapshotInterval

         -- | During the development and integration of new network protocols
         -- (node-to-node and node-to-client) we wish to be able to test them
         -- but not have everybody use them by default on the mainnet. Avoiding
         -- enabling them by default makes it practical to include such
         -- not-yet-ready protocol versions into released versions of the node
         -- without the danger that node operators on the mainnet will start
         -- using them prematurely, before the testing is complete.
         --
         -- The flag defaults to 'False'
         --
         -- This flag should be set to 'True' when testing the new protocol
         -- versions.
       , ncTestEnableDevelopmentNetworkProtocols :: !Bool

         -- BlockFetch configuration
       , ncMaxConcurrencyBulkSync :: !(Maybe MaxConcurrencyBulkSync)
       , ncMaxConcurrencyDeadline :: !(Maybe MaxConcurrencyDeadline)

         -- Logging parameters:
       , ncLoggingSwitch  :: !Bool
       , ncLogMetrics     :: !Bool
       , ncTraceConfig    :: !TraceOptions
       } deriving (Eq, Show)


data PartialNodeConfiguration
  = PartialNodeConfiguration
      {  pncNodeIPv4Addr    :: !(Last NodeHostIPv4Address)
      ,  pncNodeIPv6Addr    :: !(Last NodeHostIPv6Address)
      ,  pncNodePortNumber  :: !(Last PortNumber)
         -- | Filepath of the configuration yaml file. This file determines
         -- all the configuration settings required for the bcc node
         -- (logging, tracing, protocol, slot length etc)
       , pncConfigFile      :: !(Last ConfigYamlFilePath)
       , pncTopologyFile    :: !(Last TopologyFile)
       , pncDatabaseFile    :: !(Last DbFile)
       , pncProtocolFiles   :: !(Last ProtocolFilepaths)
       , pncValidateDB      :: !(Last Bool)
       , pncShutdownIPC     :: !(Last (Maybe Fd))
       , pncShutdownOnSlotSynced :: !(Last MaxSlotNo)

          -- Protocol-specific parameters:
       , pncProtocolConfig :: !(Last NodeProtocolConfiguration)

         -- Node parameters, not protocol-specific:
       , pncSocketPath       :: !(Last SocketPath)
       , pncDiffusionMode    :: !(Last DiffusionMode)
       , pncSnapshotInterval :: !(Last SnapshotInterval)
       , pncTestEnableDevelopmentNetworkProtocols :: !(Last Bool)

         -- BlockFetch configuration
       , pncMaxConcurrencyBulkSync :: !(Last MaxConcurrencyBulkSync)
       , pncMaxConcurrencyDeadline :: !(Last MaxConcurrencyDeadline)

         -- Logging parameters:
       , pncLoggingSwitch  :: !(Last Bool)
       , pncLogMetrics     :: !(Last Bool)
       , pncTraceConfig    :: !(Last TraceOptions)
       } deriving (Eq, Generic, Show)

instance AdjustFilePaths PartialNodeConfiguration where
  adjustFilePaths f x =
    x { pncProtocolConfig = adjustFilePaths f (pncProtocolConfig x)
      , pncSocketPath     = adjustFilePaths f (pncSocketPath x)
      }

instance Semigroup PartialNodeConfiguration where
  (<>) = gmappend

instance FromJSON PartialNodeConfiguration where
  parseJSON =
    withObject "PartialNodeConfiguration" $ \v -> do

      -- Node parameters, not protocol-specific
      pncSocketPath <- Last <$> v .:? "SocketPath"
      pncDiffusionMode
        <- Last . fmap getDiffusionMode <$> v .:? "DiffusionMode"
      pncSnapshotInterval
        <- Last . fmap RequestedSnapshotInterval <$> v .:? "SnapshotInterval"
      pncTestEnableDevelopmentNetworkProtocols
        <- Last <$> v .:? "TestEnableDevelopmentNetworkProtocols"

      -- Blockfetch parameters
      pncMaxConcurrencyBulkSync <- Last <$> v .:? "MaxConcurrencyBulkSync"
      pncMaxConcurrencyDeadline <- Last <$> v .:? "MaxConcurrencyDeadline"

      -- Logging parameters
      pncLoggingSwitch <- Last . Just <$> v .:? "TurnOnLogging" .!= True
      pncLogMetrics    <- Last        <$> v .:? "TurnOnLogMetrics"
      pncTraceConfig   <- Last . Just <$> traceConfigParser v

      -- Protocol parameters
      protocol <-  v .:? "Protocol" .!= ColeProtocol
      pncProtocolConfig <-
        case protocol of
          ColeProtocol ->
            Last . Just . NodeProtocolConfigurationCole <$> parseColeProtocol v

          SophieProtocol ->
            Last . Just . NodeProtocolConfigurationSophie <$> parseSophieProtocol v

          BccProtocol ->
            Last . Just  <$> (NodeProtocolConfigurationBcc <$> parseColeProtocol v
                                                               <*> parseSophieProtocol v
                                                               <*> parseAurumProtocol v
                                                               <*> parseHardForkProtocol v)
      pure PartialNodeConfiguration {
             pncProtocolConfig
           , pncSocketPath
           , pncDiffusionMode
           , pncSnapshotInterval
           , pncTestEnableDevelopmentNetworkProtocols
           , pncMaxConcurrencyBulkSync
           , pncMaxConcurrencyDeadline
           , pncLoggingSwitch
           , pncLogMetrics
           , pncTraceConfig
           , pncNodeIPv4Addr = mempty
           , pncNodeIPv6Addr = mempty
           , pncNodePortNumber = mempty
           , pncConfigFile = mempty
           , pncTopologyFile = mempty
           , pncDatabaseFile = mempty
           , pncProtocolFiles = mempty
           , pncValidateDB = mempty
           , pncShutdownIPC = mempty
           , pncShutdownOnSlotSynced = mempty
           }
    where
      parseColeProtocol v = do
        primary   <- v .:? "ColeGenesisFile"
        secondary <- v .:? "GenesisFile"
        npcColeGenesisFile <-
          case (primary, secondary) of
            (Just g, Nothing)  -> return g
            (Nothing, Just g)  -> return g
            (Nothing, Nothing) -> fail $ "Missing required field, either "
                                      ++ "ColeGenesisFile or GenesisFile"
            (Just _, Just _)   -> fail $ "Specify either ColeGenesisFile"
                                      ++ "or GenesisFile, but not both"
        npcColeGenesisFileHash <- v .:? "ColeGenesisHash"

        npcColeReqNetworkMagic     <- v .:? "RequiresNetworkMagic"
                                         .!= RequiresNoMagic
        npcColePbftSignatureThresh <- v .:? "PBftSignatureThreshold"
        npcColeApplicationName     <- v .:? "ApplicationName"
                                         .!= Cole.ApplicationName "bcc-sl"
        npcColeApplicationVersion  <- v .:? "ApplicationVersion" .!= 1
        protVerMajor                <- v .: "LastKnownBlockVersion-Major"
        protVerSentry                <- v .: "LastKnownBlockVersion-Sentry"
                        

        pure NodeColeProtocolConfiguration {
               npcColeGenesisFile
             , npcColeGenesisFileHash
             , npcColeReqNetworkMagic
             , npcColePbftSignatureThresh
             , npcColeApplicationName
             , npcColeApplicationVersion
             , npcColeSupportedProtocolVersionMajor = protVerMajor
             , npcColeSupportedProtocolVersionSentry = protVerSentry
             }

      parseSophieProtocol v = do
        primary   <- v .:? "SophieGenesisFile"
        secondary <- v .:? "GenesisFile"
        npcSophieGenesisFile <-
          case (primary, secondary) of
            (Just g, Nothing)  -> return g
            (Nothing, Just g)  -> return g
            (Nothing, Nothing) -> fail $ "Missing required field, either "
                                      ++ "SophieGenesisFile or GenesisFile"
            (Just _, Just _)   -> fail $ "Specify either SophieGenesisFile"
                                      ++ "or GenesisFile, but not both"
        npcSophieGenesisFileHash <- v .:? "SophieGenesisHash"

        pure NodeSophieProtocolConfiguration {
               npcSophieGenesisFile
             , npcSophieGenesisFileHash
             }

      parseAurumProtocol v = do
        npcAurumGenesisFile     <- v .:  "AurumGenesisFile"
        npcAurumGenesisFileHash <- v .:? "AurumGenesisHash"

        pure NodeAurumProtocolConfiguration {
               npcAurumGenesisFile
             , npcAurumGenesisFileHash
             }

      parseHardForkProtocol v = do
        npcTestEnableDevelopmentHardForkEras
          <- v .:? "TestEnableDevelopmentHardForkEras"
               .!= False

        npcTestSophieHardForkAtEpoch   <- v .:? "TestSophieHardForkAtEpoch"
        npcTestSophieHardForkAtVersion <- v .:? "TestSophieHardForkAtVersion"

        npcTestEvieHardForkAtEpoch   <- v .:? "TestEvieHardForkAtEpoch"
        npcTestEvieHardForkAtVersion <- v .:? "TestEvieHardForkAtVersion"

        npcTestJenHardForkAtEpoch   <- v .:? "TestJenHardForkAtEpoch"
        npcTestJenHardForkAtVersion <- v .:? "TestJenHardForkAtVersion"

        npcTestAurumHardForkAtEpoch   <- v .:? "TestAurumHardForkAtEpoch"
        npcTestAurumHardForkAtVersion <- v .:? "TestAurumHardForkAtVersion"

        pure NodeHardForkProtocolConfiguration {
               npcTestEnableDevelopmentHardForkEras,

               npcTestSophieHardForkAtEpoch,
               npcTestSophieHardForkAtVersion,

               npcTestEvieHardForkAtEpoch,
               npcTestEvieHardForkAtVersion,

               npcTestJenHardForkAtEpoch,
               npcTestJenHardForkAtVersion,

               npcTestAurumHardForkAtEpoch,
               npcTestAurumHardForkAtVersion
             }

-- | Default configuration is mainnet
defaultPartialNodeConfiguration :: PartialNodeConfiguration
defaultPartialNodeConfiguration =
  PartialNodeConfiguration
    { pncConfigFile = Last . Just $ ConfigYamlFilePath "configuration/bcc/mainnet-config.json"
    , pncDatabaseFile = Last . Just $ DbFile "mainnet/db/"
    , pncLoggingSwitch = Last $ Just True
    , pncSocketPath = mempty
    , pncDiffusionMode = Last $ Just InitiatorAndResponderDiffusionMode
    , pncSnapshotInterval = Last $ Just DefaultSnapshotInterval
    , pncTestEnableDevelopmentNetworkProtocols = Last $ Just False
    , pncTopologyFile = Last . Just $ TopologyFile "configuration/bcc/mainnet-topology.json"
    , pncNodeIPv4Addr = mempty
    , pncNodeIPv6Addr = mempty
    , pncNodePortNumber = mempty
    , pncProtocolFiles = mempty
    , pncValidateDB = mempty
    , pncShutdownIPC = mempty
    , pncShutdownOnSlotSynced = mempty
    , pncProtocolConfig = mempty
    , pncMaxConcurrencyBulkSync = mempty
    , pncMaxConcurrencyDeadline = mempty
    , pncLogMetrics = mempty
    , pncTraceConfig = mempty
    }

lastOption :: Parser a -> Parser (Last a)
lastOption = fmap Last . optional

lastToEither :: String -> Last a -> Either String a
lastToEither errMsg (Last x) = maybe (Left errMsg) Right x

makeNodeConfiguration :: PartialNodeConfiguration -> Either String NodeConfiguration
makeNodeConfiguration pnc = do
  configFile <- lastToEither "Missing YAML config file" $ pncConfigFile pnc
  topologyFile <- lastToEither "Missing TopologyFile" $ pncTopologyFile pnc
  databaseFile <- lastToEither "Missing DatabaseFile" $ pncDatabaseFile pnc
  protocolFiles <- lastToEither "Missing ProtocolFiles" $ pncProtocolFiles pnc
  validateDB <- lastToEither "Missing ValidateDB" $ pncValidateDB pnc
  shutdownIPC <- lastToEither "Missing ShutdownIPC" $ pncShutdownIPC pnc
  shutdownOnSlotSynced <- lastToEither "Missing ShutdownOnSlotSynced" $ pncShutdownOnSlotSynced pnc
  protocolConfig <- lastToEither "Missing ProtocolConfig" $ pncProtocolConfig pnc
  loggingSwitch <- lastToEither "Missing LoggingSwitch" $ pncLoggingSwitch pnc
  logMetrics <- lastToEither "Missing LogMetrics" $ pncLogMetrics pnc
  traceConfig <- lastToEither "Missing TraceConfig" $ pncTraceConfig pnc
  diffusionMode <- lastToEither "Missing DiffusionMode" $ pncDiffusionMode pnc
  snapshotInterval <- lastToEither "Missing SnapshotInterval" $ pncSnapshotInterval pnc

  testEnableDevelopmentNetworkProtocols <-
    lastToEither "Missing TestEnableDevelopmentNetworkProtocols" $
      pncTestEnableDevelopmentNetworkProtocols pnc
  return $ NodeConfiguration
             { ncNodeIPv4Addr = getLast $ pncNodeIPv4Addr pnc
             , ncNodeIPv6Addr = getLast $ pncNodeIPv6Addr pnc
             , ncNodePortNumber = getLast $ pncNodePortNumber pnc
             , ncConfigFile = configFile
             , ncTopologyFile = topologyFile
             , ncDatabaseFile = databaseFile
             , ncProtocolFiles = protocolFiles
             , ncValidateDB = validateDB
             , ncShutdownIPC = shutdownIPC
             , ncShutdownOnSlotSynced = shutdownOnSlotSynced
             , ncProtocolConfig = protocolConfig
             , ncSocketPath = getLast $ pncSocketPath pnc
             , ncDiffusionMode = diffusionMode
             , ncSnapshotInterval = snapshotInterval
             , ncTestEnableDevelopmentNetworkProtocols = testEnableDevelopmentNetworkProtocols
             , ncMaxConcurrencyBulkSync = getLast $ pncMaxConcurrencyBulkSync pnc
             , ncMaxConcurrencyDeadline = getLast $ pncMaxConcurrencyDeadline pnc
             , ncLoggingSwitch = loggingSwitch
             , ncLogMetrics = logMetrics
             , ncTraceConfig = if loggingSwitch then traceConfig
                                                else TracingOff
             }

ncProtocol :: NodeConfiguration -> Protocol
ncProtocol nc =
  case ncProtocolConfig nc of
    NodeProtocolConfigurationCole{}   -> ColeProtocol
    NodeProtocolConfigurationSophie{} -> SophieProtocol
    NodeProtocolConfigurationBcc{} -> BccProtocol

pncProtocol :: PartialNodeConfiguration -> Either Text Protocol
pncProtocol pnc =
  case pncProtocolConfig pnc of
    Last Nothing -> Left "Node protocol configuration not found"
    Last (Just NodeProtocolConfigurationCole{})   -> Right ColeProtocol
    Last (Just NodeProtocolConfigurationSophie{}) -> Right SophieProtocol
    Last (Just NodeProtocolConfigurationBcc{}) -> Right BccProtocol

parseNodeConfigurationFP :: Maybe ConfigYamlFilePath -> IO PartialNodeConfiguration
parseNodeConfigurationFP Nothing = parseNodeConfigurationFP . getLast $ pncConfigFile defaultPartialNodeConfiguration
parseNodeConfigurationFP (Just (ConfigYamlFilePath fp)) = do
    nc <- decodeFileThrow fp
    -- Make all the files be relative to the location of the config file.
    pure $ adjustFilePaths (takeDirectory fp </>) nc
