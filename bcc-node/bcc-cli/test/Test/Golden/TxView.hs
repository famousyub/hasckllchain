{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.TxView (txViewTests) where

import           Bcc.Prelude

import           Hedgehog (Group (..), Property, checkSequential)
import           Hedgehog.Extras.Test.Base (moduleWorkspace, propertyOnce)

import           Test.OptParse (execBccCLI, noteTempFile)
import           Test.Utilities (diffVsGoldenFile)

{- HLINT ignore "Use camelCase" -}

txViewTests :: IO Bool
txViewTests =
  checkSequential $
    Group "`transaction view` Goldens"
      [ ("golden_view_sophie", golden_view_sophie)
      , ("golden_view_evie", golden_view_evie)
      , ("golden_view_jen",    golden_view_jen)
      ]

golden_view_sophie :: Property
golden_view_sophie =
  propertyOnce $
  moduleWorkspace "tmp" $ \tempDir -> do
    transactionBodyFile <- noteTempFile tempDir "transaction-body-file"

    -- Create transaction body
    void $
      execBccCLI
        [ "transaction", "build-raw"
        , "--sophie-era"
        , "--tx-in"
        ,   "fe5dd07fb576bff960d6e066eade5b26cdb5afebe29f76ea58d0a098bce5d891\
            \#29"
        , "--tx-out"
        ,   "addr1v9wmu83pzajplrtpsq6tsqdgwr98x888trpmah2u0ezznsge7del3+31"
        , "--fee", "32"
        , "--invalid-hereafter", "33"
        , "--out-file", transactionBodyFile
        ]

    -- View transaction body
    result <-
      execBccCLI
        ["transaction", "view", "--tx-body-file", transactionBodyFile]
    diffVsGoldenFile result "test/data/golden/sophie/transaction-view.out"

golden_view_evie :: Property
golden_view_evie =
  propertyOnce $
  moduleWorkspace "tmp" $ \tempDir -> do
    transactionBodyFile <- noteTempFile tempDir "transaction-body-file"

    -- Create transaction body
    void $
      execBccCLI
        [ "transaction", "build-raw"
        , "--evie-era"
        , "--tx-in"
        ,   "fe5dd07fb576bff960d6e066eade5b26cdb5afebe29f76ea58d0a098bce5d891\
            \#94"
        , "--tx-out"
        ,   "addr_test1qrefnr4k09pvge6dq83v6s67ruter8sftmky8qrmkqqsxy7q5psgn8tgqmupq4r79jmxlyk4eqt6z6hj5g8jd8393msqaw47f4\
            \+99"
        , "--fee", "100"
        , "--invalid-hereafter", "101"
        , "--out-file", transactionBodyFile
        ]

    -- View transaction body
    result <-
      execBccCLI
        ["transaction", "view", "--tx-body-file", transactionBodyFile]
    diffVsGoldenFile result "test/data/golden/evie/transaction-view.out"

golden_view_jen :: Property
golden_view_jen =
  propertyOnce $
  moduleWorkspace "tmp" $ \tempDir -> do
    transactionBodyFile <- noteTempFile tempDir "transaction-body-file"

    -- Create transaction body
    void $
      execBccCLI
        [ "transaction", "build-raw"
        , "--jen-era"
        , "--tx-in"
        ,   "fe5dd07fb576bff960d6e066eade5b26cdb5afebe29f76ea58d0a098bce5d891\
            \#135"
        , "--tx-out"
        ,   "addr_test1qrefnr4k09pvge6dq83v6s67ruter8sftmky8qrmkqqsxy7q5psgn8tgqmupq4r79jmxlyk4eqt6z6hj5g8jd8393msqaw47f4\
            \+138"
        , "--fee", "139"
        , "--invalid-before", "140"
        , "--mint"
        ,   "142 69596718df8203759c0f9e86f3f79d1dd45bc9d34109a4fccc824e02"
        , "--minting-script-file", "test/data/golden/sophie/multisig/scripts/any"
        , "--out-file", transactionBodyFile
        ]

    -- View transaction body
    result <-
      execBccCLI
        ["transaction", "view", "--tx-body-file", transactionBodyFile]
    diffVsGoldenFile result "test/data/golden/jen/transaction-view.out"
