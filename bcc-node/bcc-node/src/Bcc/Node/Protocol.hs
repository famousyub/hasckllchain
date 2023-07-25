{-# LANGUAGE NamedFieldPuns #-}

module Bcc.Node.Protocol
  ( mkConsensusProtocol
  , SomeConsensusProtocol(..)
  , ProtocolInstantiationError(..)
  ) where

import           Bcc.Prelude

import           Control.Monad.Trans.Except.Extra (firstExceptT)

import           Bcc.Api

import           Bcc.Node.Configuration.POM (NodeConfiguration (..))
import           Bcc.Node.Types

import           Bcc.Node.Orphans ()
import           Bcc.Node.Protocol.Cole
import           Bcc.Node.Protocol.Bcc
import           Bcc.Node.Protocol.Sophie
import           Bcc.Node.Protocol.Types (SomeConsensusProtocol (..))


------------------------------------------------------------------------------
-- Conversions from configuration into specific protocols and their params
--

mkConsensusProtocol
  :: NodeConfiguration
  -> ExceptT ProtocolInstantiationError IO SomeConsensusProtocol
mkConsensusProtocol NodeConfiguration{ncProtocolConfig, ncProtocolFiles} =
    case ncProtocolConfig of

      NodeProtocolConfigurationCole config ->
        firstExceptT ColeProtocolInstantiationError $
          mkSomeConsensusProtocolCole config (Just ncProtocolFiles)

      NodeProtocolConfigurationSophie config ->
        firstExceptT SophieProtocolInstantiationError $
          mkSomeConsensusProtocolSophie config (Just ncProtocolFiles)

      NodeProtocolConfigurationBcc coleConfig
                                       sophieConfig
                                       aurumConfig
                                       hardForkConfig ->
        firstExceptT BccProtocolInstantiationError $
          mkSomeConsensusProtocolBcc
            coleConfig
            sophieConfig
            aurumConfig
            hardForkConfig
            (Just ncProtocolFiles)

------------------------------------------------------------------------------
-- Errors
--

data ProtocolInstantiationError =
    ColeProtocolInstantiationError   ColeProtocolInstantiationError
  | SophieProtocolInstantiationError SophieProtocolInstantiationError
  | BccProtocolInstantiationError BccProtocolInstantiationError
  deriving Show


instance Error ProtocolInstantiationError where
  displayError (ColeProtocolInstantiationError   err) = displayError err
  displayError (SophieProtocolInstantiationError err) = displayError err
  displayError (BccProtocolInstantiationError err) = displayError err

