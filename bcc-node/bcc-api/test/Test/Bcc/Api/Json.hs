{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Bcc.Api.Json
  ( tests
  ) where

import           Bcc.Api.Orphans ()
import           Bcc.Prelude (($))
import           Data.Aeson (eitherDecode, encode)
import           Gen.Bcc.Api (genAurumGenesis)
import           Gen.Tasty.Hedgehog.Group (fromGroup)
import           Hedgehog (Property, discover, forAll, tripping)
import           Test.Tasty (TestTree)

import qualified Hedgehog as H

{- HLINT ignore "Use camelCase" -}

prop_roundtrip_aurum_genesis :: Property
prop_roundtrip_aurum_genesis = H.property $ do
  genesis <- forAll genAurumGenesis
  tripping genesis encode eitherDecode

tests :: TestTree
tests = fromGroup $$discover
