{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Bcc.TxSubmit
  ( runTxSubmitWebapi
  , opts
  ) where

import           Bcc.BM.Trace (Trace, logInfo)
import           Bcc.TxSubmit.CLI.Parsers (opts)
import           Bcc.TxSubmit.CLI.Types (ConfigFile (unConfigFile), TxSubmitNodeParams (..))
import           Bcc.TxSubmit.Config (GenTxSubmitNodeConfig (..), ToggleLogging (..),
                   TxSubmitNodeConfig, readTxSubmitNodeConfig)
import           Bcc.TxSubmit.Metrics (registerMetricsServer)
import           Bcc.TxSubmit.Web (runTxSubmitServer)
import           Control.Applicative (Applicative (..))
import           Control.Monad (void)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Either (Either (..))
import           Data.Function (($))
import           Data.Text (Text)
import           System.IO (IO)

import qualified Bcc.BM.Setup as Logging
import qualified Bcc.BM.Trace as Logging
import qualified Control.Concurrent.Async as Async

runTxSubmitWebapi :: TxSubmitNodeParams -> IO ()
runTxSubmitWebapi tsnp = do
    tsnc <- readTxSubmitNodeConfig (unConfigFile $ tspConfigFile tsnp)
    trce <- mkTracer tsnc
    (metrics, metricsServer) <- registerMetricsServer
    txSubmitServer <- Async.async $
      runTxSubmitServer trce metrics tspWebserverConfig tspProtocol tspNetworkId tspSocketPath
    void $ Async.waitAnyCancel
      [ txSubmitServer
      , metricsServer
      ]
    logInfo trce "runTxSubmitWebapi: Async.waitAnyCancel returned"
  where
    TxSubmitNodeParams
      { tspProtocol
      , tspNetworkId
      , tspSocketPath
      , tspWebserverConfig
      } = tsnp

mkTracer :: TxSubmitNodeConfig -> IO (Trace IO Text)
mkTracer enc = case tscToggleLogging enc of
  LoggingOn -> liftIO $ Logging.setupTrace (Right $ tscLoggingConfig enc) "bcc-tx-submit"
  LoggingOff -> pure Logging.nullTracer
