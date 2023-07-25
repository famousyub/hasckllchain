{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bcc.Chairman.Commands.Run
  ( cmdRun
  ) where

import           Bcc.Api
import           Bcc.Api.Protocol.Cole
import           Bcc.Api.Protocol.Bcc
import           Bcc.Api.Protocol.Sophie
import           Bcc.Chairman (chairmanTest)
import           Bcc.Node.Configuration.POM (parseNodeConfigurationFP, pncProtocol)
import           Bcc.Node.Protocol.Types (Protocol (..))
import           Bcc.Node.Types
import           Bcc.Prelude hiding (option)
import           Control.Monad.Class.MonadTime (DiffTime)
import           Control.Tracer (Tracer (..), stdoutTracer)
import           Options.Applicative
import           Shardagnostic.Consensus.Config.SecurityParam (SecurityParam (..))

import qualified Data.Time.Clock as DTC
import qualified Options.Applicative as Opt
import qualified System.IO as IO

--TODO: replace this with the new stuff from Bcc.Api.Protocol
mkNodeClientProtocol :: Protocol -> SomeNodeClientProtocol
mkNodeClientProtocol protocol =
  case protocol of
    ColeProtocol ->
      mkSomeNodeClientProtocolCole
        (EpochSlots 21600)

    SophieProtocol ->
      mkSomeNodeClientProtocolSophie

    BccProtocol ->
      mkSomeNodeClientProtocolBcc
        (EpochSlots 21600)

data RunOpts = RunOpts
  { -- | Stop the test after given number of seconds. The chairman will
    -- observe only for the given period of time, and check the consensus
    -- and progress conditions at the end.
    --
    caRunningTime :: !DiffTime
    -- | Expect this amount of progress (chain growth) by the end of the test.
  , caMinProgress :: !BlockNo
  , caSocketPaths :: ![SocketPath]
  , caConfigYaml :: !ConfigYamlFilePath
  , caSecurityParam :: !SecurityParam
  , caNetworkMagic :: !NetworkMagic
  }

parseConfigFile :: Parser FilePath
parseConfigFile =
  strOption
    ( long "config"
    <> metavar "NODE-CONFIGURATION"
    <> help "Configuration file for the bcc-node"
    <> completer (bashCompleter "file")
    )

parseSocketPath :: Text -> Parser SocketPath
parseSocketPath helpMessage =
  SocketPath <$> strOption
    ( long "socket-path"
    <> help (toS helpMessage)
    <> completer (bashCompleter "file")
    <> metavar "FILEPATH"
    )

parseRunningTime :: Parser DiffTime
parseRunningTime =
  option ((fromIntegral :: Int -> DiffTime) <$> auto)
    (  long "timeout"
    <> short 't'
    <> metavar "SECONDS"
    <> help "Run the chairman for this length of time in seconds."
    )

parseSecurityParam :: Parser SecurityParam
parseSecurityParam =
  option (SecurityParam <$> Opt.auto)
    ( long "security-parameter"
    <> metavar "INT"
    <> help "Security parameter"
    )


parseTestnetMagic :: Parser NetworkMagic
parseTestnetMagic =
  NetworkMagic <$>
    Opt.option Opt.auto
      (  Opt.long "testnet-magic"
      <> Opt.metavar "INT"
      <> Opt.help "The testnet network magic number"
      )

parseProgress :: Parser BlockNo
parseProgress =
  option ((fromIntegral :: Int -> BlockNo) <$> auto)
    (  long "require-progress"
    <> short 'p'
    <> metavar "INT"
    <> help "Require this much chain-growth progress, in blocks."
  )

parseRunOpts :: Parser RunOpts
parseRunOpts =
  RunOpts
  <$> parseRunningTime
  <*> parseProgress
  <*> some (parseSocketPath "Path to a bcc-node socket")
  <*> fmap ConfigYamlFilePath parseConfigFile
  <*> parseSecurityParam
  <*> parseTestnetMagic

run :: RunOpts -> IO ()
run RunOpts
    { caRunningTime
    , caMinProgress
    , caSocketPaths
    , caConfigYaml
    , caSecurityParam
    , caNetworkMagic
    } = do

  partialNc <- liftIO . parseNodeConfigurationFP $ Just caConfigYaml

  ptcl <- case pncProtocol partialNc of
            Left err -> panic $ "Chairman error: " <> err
            Right protocol -> return protocol

  let someNodeClientProtocol = mkNodeClientProtocol ptcl

  chairmanTest
    (timed stdoutTracer)
    someNodeClientProtocol
    caNetworkMagic
    caSecurityParam
    caRunningTime
    caMinProgress
    caSocketPaths

  return ()

timed :: Tracer IO a -> Tracer IO a
timed (Tracer runTracer) = Tracer $ \a -> do
  ts <- DTC.getCurrentTime
  IO.putStr ("[" <> show ts <> "] ")
  runTracer a

cmdRun :: Mod CommandFields (IO ())
cmdRun = command "run"  $ flip info idm $ run <$> parseRunOpts
