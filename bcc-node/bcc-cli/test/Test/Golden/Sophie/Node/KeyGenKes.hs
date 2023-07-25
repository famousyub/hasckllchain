{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Sophie.Node.KeyGenKes
  ( golden_sophieNodeKeyGenKes
  ) where

import           Bcc.Prelude
import           Hedgehog (Property)
import           Test.OptParse

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

golden_sophieNodeKeyGenKes :: Property
golden_sophieNodeKeyGenKes = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  verificationKey <- noteTempFile tempDir "kes.vkey"
  signingKey <- noteTempFile tempDir "kes.skey"

  void $ execBccCLI
    [ "node","key-gen-KES"
    , "--verification-key-file", verificationKey
    , "--signing-key-file", signingKey
    ]

  H.assertFileOccurences 1 "KesVerificationKey_ed25519_kes_2^6" verificationKey
  H.assertFileOccurences 1 "KesSigningKey_ed25519_kes_2^6" signingKey

  H.assertEndsWithSingleNewline verificationKey
  H.assertEndsWithSingleNewline signingKey
