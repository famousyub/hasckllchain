{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bcc.CLI.Cole.Query
  ( ColeQueryError(..)
  , renderColeQueryError
  , runGetLocalNodeTip
  ) where

import           Bcc.Prelude

import           Control.Monad.Trans.Except.Extra (firstExceptT)
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.Encoding as Text

import           Bcc.Api
import           Bcc.CLI.Environment (EnvSocketError, readEnvSocketPath, renderEnvSocketError)
import           Bcc.CLI.Types (SocketPath (..))

{- HLINT ignore "Reduce duplication" -}

newtype ColeQueryError = ColeQueryEnvVarSocketErr EnvSocketError
  deriving Show

renderColeQueryError :: ColeQueryError -> Text
renderColeQueryError err =
  case err of
    ColeQueryEnvVarSocketErr sockEnvErr -> renderEnvSocketError sockEnvErr

--------------------------------------------------------------------------------
-- Query local node's chain tip
--------------------------------------------------------------------------------

runGetLocalNodeTip :: NetworkId -> ExceptT ColeQueryError IO ()
runGetLocalNodeTip networkId = do
    SocketPath sockPath <- firstExceptT ColeQueryEnvVarSocketErr
                           readEnvSocketPath
    let connctInfo =
          LocalNodeConnectInfo {
            localNodeSocketPath    = sockPath,
            localNodeNetworkId     = networkId,
            localConsensusModeParams = ColeModeParams (EpochSlots 21600)
          }

    tip <- liftIO $ getLocalChainTip connctInfo
    liftIO . putTextLn . Text.decodeUtf8 . LB.toStrict $ encodePretty tip


