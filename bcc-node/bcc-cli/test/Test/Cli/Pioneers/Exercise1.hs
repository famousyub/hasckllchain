{-# LANGUAGE OverloadedStrings #-}

module Test.Cli.Pioneers.Exercise1
  ( tests
  ) where

import           Bcc.Prelude
import           Hedgehog (Property)
import           Test.OptParse

import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

-- | 1. Generate a key pair
--   2. Check for the existence of the key pair
--   3. We use the generated verification key to build a sophie payment address.
prop_buildSophiePaymentAddress :: Property
prop_buildSophiePaymentAddress = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  -- Key filepaths
  verKey <- noteTempFile tempDir "payment-verification-key-file"
  signKey <- noteTempFile tempDir "payment-signing-key-file"

  -- Generate payment verification key
  void $ execBccCLI
    [ "address","key-gen"
    , "--verification-key-file", verKey
    , "--signing-key-file", signKey
    ]

  H.assertFilesExist [verKey, signKey]

  -- Build sophie payment address
  void $ execBccCLI
    [ "address", "build"
    , "--payment-verification-key-file", verKey
    , "--mainnet"
    ]

-- | 1. We generate a key payment pair
--   2. We generate a staking key pair
--   2. Check for the existence of the key pairs
--   3. We use the payment verification key & staking verification key
--      to build a sophie stake address.
prop_buildSophieStakeAddress :: Property
prop_buildSophieStakeAddress = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  -- Key filepaths
  stakeVerKey <- noteTempFile tempDir "stake-verification-key-file"
  stakeSignKey <- noteTempFile tempDir "stake-signing-key-file"
  paymentVerKey <- noteTempFile tempDir "payment-verification-key-file"
  paymentSignKey <- noteTempFile tempDir "payment-signing-key-file"

  -- Generate payment verification key
  void $ execBccCLI
    [ "address","key-gen"
    , "--verification-key-file", paymentVerKey
    , "--signing-key-file", paymentSignKey
    ]

  -- Generate stake verification key
  void $ execBccCLI
    [ "stake-address","key-gen"
    , "--verification-key-file", stakeVerKey
    , "--signing-key-file", stakeSignKey
    ]

  H.assertFilesExist [stakeVerKey, stakeSignKey, paymentVerKey, paymentSignKey]

  -- Build sophie stake address
  void $ execBccCLI
    [ "address", "build"
    , "--payment-verification-key-file", paymentVerKey
    , "--stake-verification-key-file", stakeVerKey
    , "--mainnet"
    ]

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  H.checkSequential
    $ H.Group "Pioneers Example 1"
        [ ("prop_buildSophiePaymentAddress", prop_buildSophiePaymentAddress)
        , ("prop_buildSophieStakeAddress", prop_buildSophieStakeAddress)
        ]
