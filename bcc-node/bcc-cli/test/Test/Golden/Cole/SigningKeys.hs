{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Cole.SigningKeys
  ( tests
  ) where

import           Bcc.Prelude

import           Codec.CBOR.Read (deserialiseFromBytes)
import qualified Data.ByteString.Lazy as LB

import qualified Bcc.Crypto.Signing as Crypto

import           Bcc.Api.Cole

import           Bcc.CLI.Cole.Key (readColeSigningKey)
import           Bcc.CLI.Cole.Legacy (decodeLegacyDelegateKey)
import           Bcc.CLI.Sophie.Commands
import           Bcc.CLI.Types (SigningKeyFile (..))

import           Hedgehog (Group (..), Property, checkSequential, property, success)
import qualified Hedgehog.Extras.Test.Base as H
import           Hedgehog.Internal.Property (failWith)
import           Test.OptParse

prop_deserialise_legacy_signing_Key :: Property
prop_deserialise_legacy_signing_Key = propertyOnce $ do
  legSkeyBs <- liftIO $ LB.readFile "test/data/golden/cole/keys/legacy.skey"
  case deserialiseFromBytes decodeLegacyDelegateKey legSkeyBs of
    Left deSerFail -> failWith Nothing $ show deSerFail
    Right _ -> success

prop_deserialise_nonLegacy_signing_Key :: Property
prop_deserialise_nonLegacy_signing_Key = propertyOnce $ do
  skeyBs <- liftIO $ LB.readFile "test/data/golden/cole/keys/cole.skey"
  case deserialiseFromBytes Crypto.fromCBORXPrv skeyBs of
    Left deSerFail -> failWith Nothing $ show deSerFail
    Right _ -> success

prop_print_legacy_signing_key_address :: Property
prop_print_legacy_signing_key_address = propertyOnce $ do
  let legKeyFp = "test/data/golden/cole/keys/legacy.skey"

  void $ execBccCLI
   [ "signing-key-address", "--cole-legacy-formats"
   , "--testnet-magic", "42"
   , "--secret", legKeyFp
   ]

  void $ execBccCLI
   [ "signing-key-address", "--cole-legacy-formats"
   , "--mainnet"
   , "--secret", legKeyFp
   ]

prop_print_nonLegacy_signing_key_address :: Property
prop_print_nonLegacy_signing_key_address = propertyOnce $ do
  let nonLegKeyFp = "test/data/golden/cole/keys/cole.skey"

  void $ execBccCLI
   [ "signing-key-address", "--cole-formats"
   , "--testnet-magic", "42"
   , "--secret", nonLegKeyFp
   ]

  void $ execBccCLI
   [ "signing-key-address", "--cole-formats"
   , "--mainnet"
   , "--secret", nonLegKeyFp
   ]

prop_generate_and_read_nonlegacy_signingkeys :: Property
prop_generate_and_read_nonlegacy_signingkeys = property $ do
  coleSkey <- liftIO $ generateSigningKey AsColeKey
  case deserialiseFromRawBytes (AsSigningKey AsColeKey) (serialiseToRawBytes coleSkey ) of
    Nothing -> failWith Nothing "Failed to deserialise non-legacy Cole signing key."
    Just _ -> success

prop_migrate_legacy_to_nonlegacy_signingkeys :: Property
prop_migrate_legacy_to_nonlegacy_signingkeys =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    let legKeyFp = "test/data/golden/cole/keys/legacy.skey"
    nonLegacyKeyFp <- noteTempFile tempDir "nonlegacy.skey"

    void $ execBccCLI
     [ "migrate-delegate-key-from"
     , "--from", legKeyFp
     , "--to", nonLegacyKeyFp
     ]

    eSignKey <- liftIO . runExceptT . readColeSigningKey NonLegacyColeKeyFormat
                  $ SigningKeyFile nonLegacyKeyFp

    case eSignKey of
      Left err -> failWith Nothing $ show err
      Right _ -> success

prop_deserialise_NonLegacy_Signing_Key_API :: Property
prop_deserialise_NonLegacy_Signing_Key_API = propertyOnce $ do
  eFailOrWit <- liftIO . runExceptT $ readColeSigningKey NonLegacyColeKeyFormat "test/data/golden/cole/keys/cole.skey"
  case eFailOrWit of
    Left keyFailure -> failWith Nothing $ show keyFailure
    Right _ -> success

prop_deserialiseLegacy_Signing_Key_API :: Property
prop_deserialiseLegacy_Signing_Key_API = propertyOnce $ do
  eFailOrWit <- liftIO . runExceptT $ readColeSigningKey LegacyColeKeyFormat "test/data/golden/cole/keys/legacy.skey"
  case eFailOrWit of
    Left keyFailure -> failWith Nothing $ show keyFailure
    Right _ -> success

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  checkSequential
    $ Group "Cole Signing Key Serialisation"
        [ ("prop_deserialise_legacy_signing_Key", prop_deserialise_legacy_signing_Key)
        , ("prop_print_legacy_signing_key_address", prop_print_legacy_signing_key_address)
        , ("prop_deserialise_nonLegacy_signing_Key", prop_deserialise_nonLegacy_signing_Key)
        , ("prop_print_nonLegacy_signing_key_address", prop_print_nonLegacy_signing_key_address)
        , ("prop_generate_and_read_nonlegacy_signingkeys", prop_generate_and_read_nonlegacy_signingkeys)
        , ("prop_migrate_legacy_to_nonlegacy_signingkeys", prop_migrate_legacy_to_nonlegacy_signingkeys)
        , ("prop_deserialise_NonLegacy_Signing_Key_API", prop_deserialise_NonLegacy_Signing_Key_API)
        , ("prop_deserialiseLegacy_Signing_Key_API", prop_deserialiseLegacy_Signing_Key_API)
        ]

