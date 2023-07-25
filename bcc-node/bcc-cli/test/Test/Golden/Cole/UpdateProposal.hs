{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Cole.UpdateProposal
  ( updateProposalTest
  ) where

import           Bcc.Prelude
import qualified Data.Text as Text

import           Bcc.CLI.Cole.UpdateProposal

import           Hedgehog (Property, (===))
import qualified Hedgehog as H
import           Hedgehog.Internal.Property (failWith)
import           Test.OptParse

import qualified Hedgehog.Extras.Test.Base as H

{- HLINT ignore "Use camelCase" -}

golden_cole_update_proposal :: Property
golden_cole_update_proposal = propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  goldenUpdateProposal <- noteInputFile "test/data/golden/cole/update-proposal"
  signingKey <- noteInputFile "test/data/golden/cole/keys/cole.skey"
  createdUpdateProposal <- noteTempFile tempDir "cole-update-proposal"
  void $ execBccCLI
    [ "cole","governance","create-update-proposal"
    , "--mainnet"
    , "--signing-key", signingKey
    , "--protocol-version-major", "1"
    , "--protocol-version-sentry", "0"
    , "--application-name", "bcc-sl"
    , "--software-version-num", "1"
    , "--system-tag", "linux"
    , "--installer-hash", "0"
    , "--filepath", createdUpdateProposal
    ]

  eGolden <- liftIO . runExceptT $ readColeUpdateProposal goldenUpdateProposal
  golden <- case eGolden of
              Left err -> failWith Nothing . Text.unpack $ renderColeUpdateProposalError err
              Right prop -> return prop

  eCreated <- liftIO . runExceptT $ readColeUpdateProposal createdUpdateProposal
  created <- case eCreated of
               Left err -> failWith Nothing . Text.unpack $ renderColeUpdateProposalError err
               Right prop -> return prop

  golden === created

updateProposalTest :: IO Bool
updateProposalTest =
  H.checkSequential
    $ H.Group "Cole Update Proposal Golden"
        [ ("golden_cole_update_proposal", golden_cole_update_proposal)
        ]
