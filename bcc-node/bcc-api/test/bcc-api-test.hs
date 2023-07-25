
import           Bcc.Crypto.Libsodium (sodiumInit)
import           Bcc.Prelude

import           Test.Tasty (TestTree, defaultMain, testGroup)

import qualified Test.Bcc.Api.Crypto
import qualified Test.Bcc.Api.Json
import qualified Test.Bcc.Api.KeysCole
import qualified Test.Bcc.Api.Ledger
import qualified Test.Bcc.Api.Metadata
import qualified Test.Bcc.Api.Typed.Bech32
import qualified Test.Bcc.Api.Typed.CBOR
import qualified Test.Bcc.Api.Typed.Envelope
import qualified Test.Bcc.Api.Typed.JSON
import qualified Test.Bcc.Api.Typed.Ord
import qualified Test.Bcc.Api.Typed.Script
import qualified Test.Bcc.Api.Typed.RawBytes
import qualified Test.Bcc.Api.Typed.Value

main :: IO ()
main = do
  -- TODO: Remove sodiumInit: https://github.com/The-Blockchain-Company/bcc-base/issues/175
  sodiumInit
  defaultMain tests

tests :: TestTree
tests =
  testGroup "Bcc.Api"
    [ Test.Bcc.Api.Crypto.tests
    , Test.Bcc.Api.Json.tests
    , Test.Bcc.Api.KeysCole.tests
    , Test.Bcc.Api.Ledger.tests
    , Test.Bcc.Api.Metadata.tests
    , Test.Bcc.Api.Typed.Bech32.tests
    , Test.Bcc.Api.Typed.CBOR.tests
    , Test.Bcc.Api.Typed.Envelope.tests
    , Test.Bcc.Api.Typed.JSON.tests
    , Test.Bcc.Api.Typed.Ord.tests
    , Test.Bcc.Api.Typed.RawBytes.tests
    , Test.Bcc.Api.Typed.Script.tests
    , Test.Bcc.Api.Typed.Value.tests
    ]
