{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Bcc.Node.Protocol.Bcc
  ( mkSomeConsensusProtocolBcc

    -- * Errors
  , BccProtocolInstantiationError(..)
  ) where

import           Prelude

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT)

import qualified Bcc.Chain.Update as Cole

import           Shardagnostic.Consensus.Bcc
import qualified Shardagnostic.Consensus.Bcc as Consensus
import qualified Shardagnostic.Consensus.Bcc.CanHardFork as Consensus
import           Shardagnostic.Consensus.HardFork.Combinator.Condense ()

import           Shardagnostic.Consensus.Bcc.Condense ()
import qualified Shardagnostic.Consensus.Mempool.TxLimits as TxLimits

import           Bcc.Api
import           Bcc.Api.Orphans ()
import           Bcc.Api.Protocol.Types
import           Bcc.Node.Types

import           Bcc.Tracing.OrphanInstances.Cole ()
import           Bcc.Tracing.OrphanInstances.Sophie ()

import qualified Bcc.Node.Protocol.Aurum as Aurum
import qualified Bcc.Node.Protocol.Cole as Cole
import qualified Bcc.Node.Protocol.Sophie as Sophie

import           Bcc.Node.Protocol.Types

------------------------------------------------------------------------------
-- Real Bcc protocol
--

-- | Make 'SomeConsensusProtocol' using the Bcc instance.
--
-- The Bcc protocol instance is currently the sequential composition of
-- the Cole and Sophie protocols, and will likely be extended in future
-- with further sequentially composed protocol revisions.
--
-- The use of 'SomeConsensusProtocol' lets us handle multiple protocols in a
-- generic way.
--
-- This also serves a purpose as a sanity check that we have all the necessary
-- type class instances available.
--
mkSomeConsensusProtocolBcc
  :: NodeColeProtocolConfiguration
  -> NodeSophieProtocolConfiguration
  -> NodeAurumProtocolConfiguration
  -> NodeHardForkProtocolConfiguration
  -> Maybe ProtocolFilepaths
  -> ExceptT BccProtocolInstantiationError IO SomeConsensusProtocol
mkSomeConsensusProtocolBcc NodeColeProtocolConfiguration {
                             npcColeGenesisFile,
                             npcColeGenesisFileHash,
                             npcColeReqNetworkMagic,
                             npcColePbftSignatureThresh,
                             npcColeApplicationName,
                             npcColeApplicationVersion,
                             npcColeSupportedProtocolVersionMajor,
                             npcColeSupportedProtocolVersionSentry
                           }
                           NodeSophieProtocolConfiguration {
                             npcSophieGenesisFile,
                             npcSophieGenesisFileHash
                           }
                           NodeAurumProtocolConfiguration {
                             npcAurumGenesisFile,
                             npcAurumGenesisFileHash
                           }
                           NodeHardForkProtocolConfiguration {
                            -- npcTestEnableDevelopmentHardForkEras,
                            -- During testing of the Aurum era, we conditionally declared that we
                            -- knew about the Aurum era. We do so only when a config option for
                            -- testing development/unstable eras is used. This lets us include
                            -- not-yet-ready eras in released node versions without mainnet nodes
                            -- prematurely advertising that they could hard fork into the new era.
                             npcTestSophieHardForkAtEpoch,
                             npcTestSophieHardForkAtVersion,
                             npcTestEvieHardForkAtEpoch,
                             npcTestEvieHardForkAtVersion,
                             npcTestJenHardForkAtEpoch,
                             npcTestJenHardForkAtVersion,
                             npcTestAurumHardForkAtEpoch,
                             npcTestAurumHardForkAtVersion
                           }
                           files = do
    coleGenesis <-
      firstExceptT BccProtocolInstantiationErrorCole $
        Cole.readGenesis npcColeGenesisFile
                          npcColeGenesisFileHash
                          npcColeReqNetworkMagic

    coleLeaderCredentials <-
      firstExceptT BccProtocolInstantiationErrorCole $
        Cole.readLeaderCredentials coleGenesis files

    (sophieGenesis, sophieGenesisHash) <-
      firstExceptT BccProtocolInstantiationSophieGenesisReadError $
        Sophie.readGenesis npcSophieGenesisFile
                            npcSophieGenesisFileHash

    (aurumGenesis, _aurumGenesisHash) <-
      firstExceptT BccProtocolInstantiationAurumGenesisReadError $
        Aurum.readGenesis npcAurumGenesisFile
                           npcAurumGenesisFileHash

    sophieLeaderCredentials <-
      firstExceptT BccProtocolInstantiationOptimumLeaderCredentialsError $
        Sophie.readLeaderCredentials files

    --TODO: all these protocol versions below are confusing and unnecessary.
    -- It could and should all be automated and these config entries eliminated.
    return $!
      SomeConsensusProtocol BccBlockType $ ProtocolInfoArgsBcc
        Consensus.ProtocolParamsCole {
          coleGenesis = coleGenesis,
          colePbftSignatureThreshold =
            PBftSignatureThreshold <$> npcColePbftSignatureThresh,

          -- This is /not/ the Cole protocol version. It is the protocol
          -- version that this node will use in blocks it creates. It is used
          -- in the Cole update mechanism to signal that this block-producing
          -- node is ready to move to the new protocol. For example, when the
          -- protocol version (according to the ledger state) is 0, this setting
          -- should be 1 when we are ready to move. Similarly when the current
          -- protocol version is 1, this should be 2 to indicate we are ready
          -- to move into the Sophie era.
          coleProtocolVersion =
            Cole.ProtocolVersion
              npcColeSupportedProtocolVersionMajor
              npcColeSupportedProtocolVersionSentry,
          coleSoftwareVersion =
            Cole.SoftwareVersion
              npcColeApplicationName
              npcColeApplicationVersion,
          coleLeaderCredentials =
            coleLeaderCredentials,
          coleMaxTxCapacityOverrides =
            TxLimits.mkOverrides TxLimits.noOverridesMeasure
        }
        Consensus.ProtocolParamsSophieBased {
          sophieBasedGenesis           = sophieGenesis,
          sophieBasedInitialNonce      = Sophie.genesisHashToOptimumNonce
                                            sophieGenesisHash,
          sophieBasedLeaderCredentials = sophieLeaderCredentials
        }
        Consensus.ProtocolParamsSophie {
          -- This is /not/ the Sophie protocol version. It is the protocol
          -- version that this node will declare that it understands, when it
          -- is in the Sophie era. That is, it is the version of protocol
          -- /after/ Sophie, i.e. Evie.
          sophieProtVer =
            ProtVer 3 0,
          sophieMaxTxCapacityOverrides =
            TxLimits.mkOverrides TxLimits.noOverridesMeasure
        }
        Consensus.ProtocolParamsEvie {
          -- This is /not/ the Evie protocol version. It is the protocol
          -- version that this node will declare that it understands, when it
          -- is in the Evie era. That is, it is the version of protocol
          -- /after/ Evie, i.e. Jen.
          evieProtVer =
            ProtVer 4 0,
          evieMaxTxCapacityOverrides =
            TxLimits.mkOverrides TxLimits.noOverridesMeasure
        }
        Consensus.ProtocolParamsJen {
          -- This is /not/ the Jen protocol version. It is the protocol
          -- version that this node will declare that it understands, when it
          -- is in the Jen era. That is, it is the version of protocol
          -- /after/ Jen, i.e. Aurum.
          jenProtVer = ProtVer 5 0,
          jenMaxTxCapacityOverrides =
            TxLimits.mkOverrides TxLimits.noOverridesMeasure
        }
        Consensus.ProtocolParamsAurum {
          -- This is /not/ the Aurum protocol version. It is the protocol
          -- version that this node will declare that it understands, when it
          -- is in the Aurum era. Since Aurum is currently the last known
          -- protocol version then this is also the Aurum protocol version.
          aurumProtVer = ProtVer 6 0,
          aurumMaxTxCapacityOverrides =
            TxLimits.mkOverrides TxLimits.noOverridesMeasure
        }

        -- ProtocolParamsTransition specifies the parameters needed to transition between two eras
        -- The comments below also apply for the Sophie -> Evie and Evie -> Jen hard forks.
        -- Cole to Sophie hard fork parameters
        Consensus.ProtocolTransitionParamsSophieBased {
          transitionTranslationContext = (),
          transitionTrigger =
            -- What will trigger the Cole -> Sophie hard fork?
            case npcTestSophieHardForkAtEpoch of

               -- This specifies the major protocol version number update that will
               -- trigger us moving to the Sophie protocol.
               --
               -- Version 0 is Cole with Shardagnostic classic
               -- Version 1 is Cole with Shardagnostic Permissive BFT
               -- Version 2 is Sophie
               -- Version 3 is Evie
               -- Version 4 is Jen
               -- Version 5 is Aurum
               --
               -- But we also provide an override to allow for simpler test setups
               -- such as triggering at the 0 -> 1 transition .
               --
               Nothing -> Consensus.TriggerHardForkAtVersion
                            (maybe 2 fromIntegral npcTestSophieHardForkAtVersion)

               -- Alternatively, for testing we can transition at a specific epoch.
               --
               Just epochNo -> Consensus.TriggerHardForkAtEpoch epochNo
        }
        -- Sophie to Evie hard fork parameters
        Consensus.ProtocolTransitionParamsSophieBased {
          transitionTranslationContext = (),
          transitionTrigger =
            case npcTestEvieHardForkAtEpoch of
               Nothing -> Consensus.TriggerHardForkAtVersion
                            (maybe 3 fromIntegral npcTestEvieHardForkAtVersion)
               Just epochNo -> Consensus.TriggerHardForkAtEpoch epochNo
        }
        -- Evie to Jen hard fork parameters
        Consensus.ProtocolTransitionParamsSophieBased {
          transitionTranslationContext = (),
          transitionTrigger =
            case npcTestJenHardForkAtEpoch of
               Nothing -> Consensus.TriggerHardForkAtVersion
                            (maybe 4 fromIntegral npcTestJenHardForkAtVersion)
               Just epochNo -> Consensus.TriggerHardForkAtEpoch epochNo
        }
        -- Jen to Aurum hard fork parameters
        Consensus.ProtocolTransitionParamsSophieBased {
          transitionTranslationContext = aurumGenesis,
          transitionTrigger =
            case npcTestAurumHardForkAtEpoch of
               Nothing -> Consensus.TriggerHardForkAtVersion
                            (maybe 5 fromIntegral npcTestAurumHardForkAtVersion)
               Just epochNo -> Consensus.TriggerHardForkAtEpoch epochNo
        }


------------------------------------------------------------------------------
-- Errors
--

data BccProtocolInstantiationError =
       BccProtocolInstantiationErrorCole
         Cole.ColeProtocolInstantiationError

     | BccProtocolInstantiationSophieGenesisReadError
         Sophie.GenesisReadError

     | BccProtocolInstantiationAurumGenesisReadError
         Sophie.GenesisReadError

     | BccProtocolInstantiationOptimumLeaderCredentialsError
         Sophie.OptimumLeaderCredentialsError

     | BccProtocolInstantiationErrorAurum
         Aurum.AurumProtocolInstantiationError
  deriving Show

instance Error BccProtocolInstantiationError where
  displayError (BccProtocolInstantiationErrorCole err) =
    displayError err
  displayError (BccProtocolInstantiationSophieGenesisReadError err) =
    "Sophie related: " <> displayError err
  displayError (BccProtocolInstantiationAurumGenesisReadError err) =
    "Aurum related: " <> displayError err
  displayError (BccProtocolInstantiationOptimumLeaderCredentialsError err) =
    displayError err
  displayError (BccProtocolInstantiationErrorAurum err) =
    displayError err
