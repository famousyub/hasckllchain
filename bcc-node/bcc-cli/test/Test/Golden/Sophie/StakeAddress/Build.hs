{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Sophie.StakeAddress.Build
  ( golden_sophieStakeAddressBuild
  ) where

import           Bcc.Prelude
import           Hedgehog (Property)
import           Test.OptParse as OP

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

golden_sophieStakeAddressBuild :: Property
golden_sophieStakeAddressBuild = propertyOnce . H.moduleWorkspace "tmp" $ \_ -> do
  verificationKeyFile <- noteInputFile "test/data/golden/sophie/keys/stake_keys/verification_key"
  goldenRewardAddressFile <- noteInputFile "test/data/golden/sophie/keys/stake_keys/reward_address"

  rewardAddress <- execBccCLI
    [ "stake-address","build"
    , "--mainnet"
    , "--staking-verification-key-file", verificationKeyFile
    ]

  goldenRewardsAddress <- H.readFile goldenRewardAddressFile

  equivalence rewardAddress goldenRewardsAddress
