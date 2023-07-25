{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Sophie.Node.KeyGenVrf
  ( golden_sophieNodeKeyGenVrf
  ) where

import           Bcc.Prelude
import           Hedgehog (Property)
import           Test.OptParse

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

golden_sophieNodeKeyGenVrf :: Property
golden_sophieNodeKeyGenVrf = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  verificationKey <- noteTempFile tempDir "kes.vkey"
  signingKey <- noteTempFile tempDir "kes.skey"

  void $ execBccCLI
    [ "node","key-gen-VRF"
    , "--verification-key-file", verificationKey
    , "--signing-key-file", signingKey
    ]

  H.assertFileOccurences 1 "VRF Verification Key" verificationKey
  H.assertFileOccurences 1 "VRF Signing Key" signingKey

  H.assertEndsWithSingleNewline verificationKey
  H.assertEndsWithSingleNewline signingKey
