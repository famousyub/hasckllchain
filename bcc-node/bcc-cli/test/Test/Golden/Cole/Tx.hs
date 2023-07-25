{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Cole.Tx
  ( txTests
  ) where

import           Bcc.CLI.Cole.Tx
import           Bcc.Prelude
import qualified Data.Text as Text

import           Bcc.Chain.UTxO (ATxAux)


import           Hedgehog (Property, (===))
import qualified Hedgehog as H
import           Hedgehog.Internal.Property (failWith)
import           Test.OptParse

import qualified Hedgehog.Extras.Test.Base as H

{- HLINT ignore "Use camelCase" -}

golden_coleTx_legacy :: Property
golden_coleTx_legacy = propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  signingKey <- noteInputFile "test/data/golden/cole/keys/legacy.skey"
  goldenTx <- noteInputFile "test/data/golden/cole/tx/legacy.tx"
  createdTx <- noteTempFile tempDir "tx"
  void $ execBccCLI
    [ "cole","transaction","issue-utxo-expenditure"
    , "--mainnet"
    , "--cole-legacy-formats"
    , "--wallet-key", signingKey
    , "--tx", createdTx
    , "--txin", "(796a90e0a89b292d53a6129b9f0d757429063b529d27e4f56565192a8c8da5e3,10)"
    , "--txout", "(\"2657WMsDfac6eFirdvKVPVMxNVYuACd1RGM2arH3g1y1yaQCr1yYpb2jr2b2aSiDZ\",999)"
    ]

  compareColeTxs createdTx goldenTx

golden_coleTx :: Property
golden_coleTx = propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  signingKey <- noteInputFile "test/data/golden/cole/keys/cole.skey"
  goldenTx <- noteInputFile "test/data/golden/cole/tx/normal.tx"
  createdTx <- noteTempFile tempDir "tx"
  void $ execBccCLI
    [ "cole","transaction","issue-utxo-expenditure"
    , "--mainnet"
    , "--cole-formats"
    , "--wallet-key", signingKey
    , "--tx", createdTx
    , "--txin", "(796a90e0a89b292d53a6129b9f0d757429063b529d27e4f56565192a8c8da5e3,10)"
    , "--txout", "(\"2657WMsDfac6eFirdvKVPVMxNVYuACd1RGM2arH3g1y1yaQCr1yYpb2jr2b2aSiDZ\",999)"
    ]

  compareColeTxs createdTx goldenTx

getTxByteString :: FilePath -> H.PropertyT IO (ATxAux ByteString)
getTxByteString txFp = do
  eATxAuxBS <- liftIO . runExceptT $ readColeTx $ TxFile txFp
  case eATxAuxBS of
    Left err -> failWith Nothing . Text.unpack $ renderColeTxError err
    Right aTxAuxBS -> return aTxAuxBS

compareColeTxs :: FilePath -> FilePath -> H.PropertyT IO ()
compareColeTxs createdTx goldenTx = do
  createdATxAuxBS <- getTxByteString createdTx
  goldenATxAuxBS <- getTxByteString goldenTx

  normalColeTxToGenTx goldenATxAuxBS === normalColeTxToGenTx createdATxAuxBS

txTests :: IO Bool
txTests =
  H.checkSequential
    $ H.Group "Cole Tx Goldens"
        [ ("golden_coleTx", golden_coleTx)
        , ("golden_coleTx_legacy", golden_coleTx_legacy)
        ]
