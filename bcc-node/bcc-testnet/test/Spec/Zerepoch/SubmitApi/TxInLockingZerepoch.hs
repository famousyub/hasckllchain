{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Spec.Zerepoch.SubmitApi.TxInLockingZerepoch
  ( hprop_zerepoch
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson (FromJSON (..), Value, (.:))
import           Data.Bool (not)
import           Data.Eq
import           Data.Function
import           Data.Functor ((<&>))
import           Data.HashMap.Lazy (HashMap)
import           Data.Int
import           Data.List ((!!))
import           Data.Maybe
import           Data.Monoid (Last (..), (<>))
import           Data.Text (Text)
import           GHC.Num
import           GHC.Real
import           Hedgehog (Property, (===))
import           Prelude (head)
import           System.FilePath ((</>))
import           Text.Show (Show (..))

import qualified Data.Aeson as J
import qualified Data.HashMap.Lazy as HM
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import qualified Hedgehog.Extras.Test as HE
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.Process as H
import qualified System.Directory as IO
import qualified System.Environment as IO
import qualified Test.Base as Test
import qualified Test.Process as H
import qualified Test.Process as Test
import qualified Testnet.Bcc as TN
import qualified Testnet.Conf as TN
import qualified Testnet.SubmitApi as TN

{- HLINT ignore "Redundant <&>" -}
{- HLINT ignore "Redundant return" -}
{- HLINT ignore "Use let" -}

data Utxo = Utxo
  { address :: Text
  , value :: HashMap Text Integer
  } deriving (Eq, Show)

instance FromJSON Utxo where
  parseJSON = J.withObject "Utxo" $ \v -> Utxo
    <$> v .: "address"
    <*> v .: "value"

hprop_zerepoch :: Property
hprop_zerepoch = Test.integration . HE.runFinallies . HE.workspace "chairman" $ \tempAbsBasePath' -> do
  projectBase <- HE.note =<< HE.noteIO . IO.canonicalizePath =<< HE.getProjectBase
  conf@TN.Conf { TN.tempBaseAbsPath, TN.tempAbsPath } <- HE.noteShowM $ TN.mkConf tempAbsBasePath' Nothing

  TN.TestnetRuntime { TN.configurationFile, TN.bftSprockets, TN.testnetMagic } <- TN.testnet TN.defaultTestnetOptions conf

  env <- H.evalIO IO.getEnvironment

  execConfig <- H.noteShow H.ExecConfig
        { H.execConfigEnv = Last $ Just $
          [ ("BCC_NODE_SOCKET_PATH", IO.sprocketArgumentName (head bftSprockets))
          ]
          -- The environment must be passed onto child process on Windows in order to
          -- successfully start that process.
          <> env
        , H.execConfigCwd = Last $ Just tempBaseAbsPath
        }

  base <- HE.note projectBase
  work <- HE.note tempAbsPath
  utxoVKeyFile <- HE.note $ tempAbsPath </> "sophie/utxo-keys/utxo1.vkey"
  utxoSKeyFile <- HE.note $ tempAbsPath </> "sophie/utxo-keys/utxo1.skey"

  zerepochScriptFileInUse <- HE.note $ base </> "scripts/zerepoch/scripts/always-succeeds-spending.zerepoch"

  submitApiConfigFile <- HE.note configurationFile
  submitApiStdoutFile <- HE.note $ tempAbsPath </> "logs/submit-api.stdout"
  submitApiStderrFile <- HE.note $ tempAbsPath </> "logs/submit-api.stderr"

  submitApiPort <- TN.submitApi TN.SubmitApiConfig
    { TN.tempBaseAbsPath
    , TN.base
    , TN.configFile = submitApiConfigFile
    , TN.sprocket = head bftSprockets
    , TN.testnetMagic
    , TN.stdoutFile = submitApiStdoutFile
    , TN.stderrFile = submitApiStderrFile
    }

  -- This datum hash is the hash of the untyped 42
  let scriptDatumHash = "9e1199a988ba72ffd6e9c269cadb3b53b5f360ff99f112d9b2ee30c4d74ad88b"
  let zerepochRequiredSpace = id @Integer 70000000
  let zerepochRequiredTime = id @Integer 70000000

  datumFile <- HE.note $ base </> "scripts/zerepoch/data/42.datum"
  redeemerFile <- HE.note $ base </> "scripts/zerepoch/data/42.redeemer"

  -- Always succeeds Zerepoch script in use. Any datum and redeemer combination will succeed.
  -- Script at: $zerepochscriptinuse

  -- Step 1: Create a tx ouput with a datum hash at the script address. In order for a tx ouput to be locked
  -- by a zerepoch script, it must have a datahash. We also need collateral tx inputs so we split the utxo
  -- in order to accomodate this.

  zerepochScriptAddr <- Test.execCli
    [ "address", "build"
    , "--payment-script-file", zerepochScriptFileInUse
    , "--testnet-magic", show @Int testnetMagic
    ]

  utxoAddr <- Test.execCli
    [ "address", "build"
    , "--testnet-magic", show @Int testnetMagic
    , "--payment-verification-key-file", utxoVKeyFile
    ]

  void $ Test.execCli' execConfig
    [ "query", "utxo"
    , "--address", utxoAddr
    , "--bcc-mode"
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "utxo-1.json"
    ]

  HE.cat $ work </> "utxo-1.json"

  utxo1Json <- HE.leftFailM . HE.readJsonFile $ work </> "utxo-1.json"
  utxo1 <- HE.noteShowM $ HE.jsonErrorFail $ J.fromJSON @(HashMap Text Utxo) utxo1Json
  txin <- HE.noteShow $ head $ HM.keys utxo1
  entropicAtTxin <- HE.nothingFailM . HE.noteShow $ utxo1 & HM.lookup txin <&> value >>= HM.lookup "entropic"
  entropicAtTxinDiv3 <- HE.noteShow $ entropicAtTxin `div` 3

  let dummyaddress = "addr_test1vpqgspvmh6m2m5pwangvdg499srfzre2dd96qq57nlnw6yctpasy4"
      targetaddress = "addr_test1qpmxr8d8jcl25kyz2tz9a9sxv7jxglhddyf475045y8j3zxjcg9vquzkljyfn3rasfwwlkwu7hhm59gzxmsyxf3w9dps8832xh"

  void $ Test.execCli' execConfig
    [ "query", "protocol-parameters"
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "pparams.json"
    ]

  void $ H.execCli' execConfig
    [ "transaction", "build"
    , "--aurum-era"
    , "--bcc-mode"
    , "--testnet-magic", show @Int testnetMagic
    , "--change-address", dummyaddress
    , "--tx-in", Text.unpack txin
    , "--tx-out", zerepochScriptAddr <> "+" <> show @Integer entropicAtTxinDiv3
    , "--tx-out-datum-hash", scriptDatumHash
    , "--tx-out", utxoAddr <> "+" <> show @Integer entropicAtTxinDiv3
    , "--protocol-params-file", work </> "pparams.json"
    , "--out-file", work </> "create-datum-output.body"
    ]

  void $ Test.execCli
    [ "transaction", "sign"
    , "--tx-body-file", work </> "create-datum-output.body"
    , "--testnet-magic", show @Int testnetMagic
    , "--signing-key-file", utxoSKeyFile
    , "--out-file", work </> "create-datum-output.tx"
    ]

  void $ Test.execCli' execConfig
    [ "transaction", "submit"
    , "--tx-file", work </> "create-datum-output.tx"
    , "--testnet-magic", show @Int testnetMagic
    ]

  HE.threadDelay 5000000

  -- With the tx ouput at the script address we can now attempt to spend it.

  void $ Test.execCli' execConfig
    [ "query", "utxo"
    , "--address", zerepochScriptAddr
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "zerepochutxo.json"
    ]

  HE.cat $ work </> "zerepochutxo.json"

  zerepochUtxoJson <- HE.leftFailM . HE.readJsonFile $ work </> "zerepochutxo.json"
  zerepochUtxo <- HE.noteShowM $ HE.jsonErrorFail $ J.fromJSON @(HashMap Text Utxo) zerepochUtxoJson
  zerepochUtxoTxIn <- HE.noteShow $ head $ HM.keys zerepochUtxo

  void $ Test.execCli' execConfig
    [ "query", "utxo"
    , "--address", utxoAddr
    , "--bcc-mode"
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "utxo-2.json"
    ]

  HE.cat $ work </> "utxo-2.json"

  utxo2Json :: Value <- HE.leftFailM $ HE.readJsonFile $ work </> "utxo-2.json"
  utxo2 <- HE.noteShowM $ HE.jsonErrorFail $ J.fromJSON @(HashMap Text Utxo) utxo2Json
  txinCollateral <- HE.noteShow $ head $ HM.keys utxo2


  entropicAtzerepochScriptAddr <- HE.nothingFailM . HE.noteShow $ zerepochUtxo & HM.lookup zerepochUtxoTxIn <&> value >>= HM.lookup "entropic"

  spendable <- HE.noteShow $ entropicAtzerepochScriptAddr - zerepochRequiredTime - zerepochRequiredSpace

  void $ H.execCli' execConfig
    [ "transaction", "build"
    , "--aurum-era"
    , "--bcc-mode"
    , "--testnet-magic", show @Int testnetMagic
    , "--change-address", dummyaddress
    , "--tx-in", Text.unpack zerepochUtxoTxIn
    , "--tx-in-collateral", Text.unpack txinCollateral
    , "--tx-out", targetaddress <> "+" <> show @Integer spendable
    , "--tx-in-script-file", zerepochScriptFileInUse
    , "--tx-in-datum-file", datumFile
    , "--protocol-params-file", work </> "pparams.json"
    , "--tx-in-redeemer-file", redeemerFile
    , "--out-file", work </> "test-aurum.body"
    ]

  void $ Test.execCli
    [ "transaction", "sign"
    , "--tx-body-file", work </> "test-aurum.body"
    , "--testnet-magic", show @Int testnetMagic
    , "--signing-key-file", utxoSKeyFile
    , "--out-file", work </> "aurum.tx"
    ]

  void $ TN.submitApiSubmitTransaction submitApiPort $ work </> "aurum.tx"

  HE.threadDelay 5000000

  -- Querying UTxO at $targetaddress. If there is BCC at the address the Zerepoch script successfully executed!

  result <- H.evalM $ Text.pack <$> Test.execCli' execConfig
    [ "query", "utxo"
    , "--address", targetaddress
    , "--testnet-magic", show @Int testnetMagic
    ]

  HE.note_ $ Text.unpack result

  List.filter (not . Text.null) (Text.splitOn " " (Text.lines result !! 2)) !! 2 === "193333333"
