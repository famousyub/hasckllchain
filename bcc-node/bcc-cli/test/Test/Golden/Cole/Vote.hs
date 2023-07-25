{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Cole.Vote
  ( voteTests
  ) where

import           Bcc.CLI.Cole.Vote
import           Bcc.Prelude
import qualified Data.Text as Text

import           Hedgehog (Property, (===))
import qualified Hedgehog as H
import           Hedgehog.Internal.Property (failWith)
import           Test.OptParse

import qualified Hedgehog.Extras.Test.Base as H

{- HLINT ignore "Use camelCase" -}

golden_cole_yes_vote :: Property
golden_cole_yes_vote = propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  goldenYesVote <- noteInputFile "test/data/golden/cole/votes/vote-yes"
  proposal <- noteInputFile "test/data/golden/cole/update-proposal"
  signingKey <- noteInputFile "test/data/golden/cole/keys/cole.skey"
  createdYesVote <- noteTempFile tempDir "cole-yes-vote"
  void $ execBccCLI
    [ "cole","governance","create-proposal-vote"
    , "--mainnet"
    , "--proposal-filepath", proposal
    , "--signing-key", signingKey
    , "--vote-yes"
    , "--output-filepath", createdYesVote
    ]

  eGolden <- liftIO . runExceptT $ readColeVote goldenYesVote
  golden <- case eGolden of
              Left err -> failWith Nothing . Text.unpack $ renderColeVoteError err
              Right prop -> return prop

  eCreated <- liftIO . runExceptT $ readColeVote createdYesVote
  created <- case eCreated of
               Left err -> failWith Nothing . Text.unpack $ renderColeVoteError err
               Right prop -> return prop

  golden === created

golden_cole_no_vote :: Property
golden_cole_no_vote = propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  goldenNoVote <- noteInputFile "test/data/golden/cole/votes/vote-no"
  proposal <- noteInputFile "test/data/golden/cole/update-proposal"
  signingKey <- noteInputFile "test/data/golden/cole/keys/cole.skey"
  createdNoVote <- noteTempFile tempDir "cole-no-vote"
  void $ execBccCLI
    [ "cole","governance","create-proposal-vote"
    , "--mainnet"
    , "--proposal-filepath", proposal
    , "--signing-key", signingKey
    , "--vote-no"
    , "--output-filepath", createdNoVote
    ]

  eGolden <- liftIO . runExceptT $ readColeVote goldenNoVote
  golden <- case eGolden of
              Left err -> failWith Nothing . Text.unpack $ renderColeVoteError err
              Right prop -> return prop

  eCreated <- liftIO . runExceptT $ readColeVote createdNoVote
  created <- case eCreated of
               Left err -> failWith Nothing . Text.unpack $ renderColeVoteError err
               Right prop -> return prop

  golden === created

voteTests :: IO Bool
voteTests =
  H.checkSequential
    $ H.Group "Cole Vote Goldens"
        [ ("golden_cole_no_vote", golden_cole_no_vote)
        , ("golden_cole_yes_vote", golden_cole_yes_vote)
        ]
