{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Bcc.Api.KeysCole
  ( tests
  ) where

import           Bcc.Api
import           Bcc.Prelude
import           Gen.Hedgehog.Roundtrip.CBOR (roundtrip_CBOR)
import           Gen.Tasty.Hedgehog.Group (fromGroup)
import           Hedgehog (Property, discover)
import           Test.Bcc.Api.Typed.Orphans ()
import           Test.Tasty (TestTree)

import qualified Gen.Bcc.Crypto.Seed as Gen

{- HLINT ignore "Use camelCase" -}

prop_roundtrip_cole_key_CBOR :: Property
prop_roundtrip_cole_key_CBOR =
  roundtrip_CBOR (AsSigningKey AsColeKey) (deterministicSigningKey AsColeKey <$> Gen.genSeedForKey AsColeKey)

tests :: TestTree
tests = fromGroup $$discover
