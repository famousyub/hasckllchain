{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Sophie.MultiSig.Address
  ( golden_sophieAllMultiSigAddressBuild
  , golden_sophieAnyMultiSigAddressBuild
  , golden_sophieAtLeastMultiSigAddressBuild
  ) where

import           Bcc.Prelude
import           Hedgehog (Property)
import           Test.OptParse as OP

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

golden_sophieAllMultiSigAddressBuild :: Property
golden_sophieAllMultiSigAddressBuild = propertyOnce . H.moduleWorkspace "tmp" $ \_ -> do
  allMultiSigFp <- noteInputFile "test/data/golden/sophie/multisig/scripts/all"

  allMultiSigAddress <- execBccCLI
    [ "address", "build-script"
    , "--script-file", allMultiSigFp
    , "--mainnet"
    ]

  goldenAllMultiSigAddrFp <- noteInputFile "test/data/golden/sophie/multisig/addresses/all"

  goldenAllMs <- H.readFile goldenAllMultiSigAddrFp

  equivalence allMultiSigAddress goldenAllMs

golden_sophieAnyMultiSigAddressBuild :: Property
golden_sophieAnyMultiSigAddressBuild = propertyOnce . H.moduleWorkspace "tmp" $ \_ -> do
  anyMultiSigFp <- noteInputFile "test/data/golden/sophie/multisig/scripts/any"

  anyMultiSigAddress <- execBccCLI
    [ "address", "build-script"
    , "--script-file", anyMultiSigFp
    , "--mainnet"
    ]

  goldenAnyMultiSigAddrFp <- noteInputFile "test/data/golden/sophie/multisig/addresses/any"

  goldenAnyMs <- H.readFile goldenAnyMultiSigAddrFp

  equivalence anyMultiSigAddress goldenAnyMs

golden_sophieAtLeastMultiSigAddressBuild :: Property
golden_sophieAtLeastMultiSigAddressBuild = propertyOnce . H.moduleWorkspace "tmp" $ \_ -> do
  atLeastMultiSigFp <- noteInputFile "test/data/golden/sophie/multisig/scripts/atleast"

  atLeastMultiSigAddress <- execBccCLI
    [ "address", "build-script"
    , "--script-file", atLeastMultiSigFp
    , "--mainnet"
    ]

  goldenAtLeastMultiSigAddrFp <- noteInputFile "test/data/golden/sophie/multisig/addresses/atleast"

  goldenAtLeastMs <- H.readFile goldenAtLeastMultiSigAddrFp

  equivalence atLeastMultiSigAddress goldenAtLeastMs
