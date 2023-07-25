{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bcc.CLI.Sophie.Run.Key
  ( SophieKeyCmdError
  , SomeSigningKey(..)
  , renderSophieKeyCmdError
  , runKeyCmd

    -- * Exports for testing
  , decodeBech32
  ) where

import           Bcc.Prelude

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as Text

import qualified Control.Exception as Exception
import           Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither, left, newExceptT)

import qualified Codec.Binary.Bech32 as Bech32

import qualified Bcc.Crypto.DSIGN as DSIGN
import qualified Bcc.Crypto.Signing as Cole.Crypto
import qualified Bcc.Crypto.Signing as Cole
import qualified Bcc.Crypto.Signing as Crypto
import qualified Bcc.Crypto.Wallet as Crypto
import qualified Bcc.Ledger.Keys as Sophie

import           Bcc.Api
import           Bcc.Api.Cole hiding (SomeColeSigningKey (..))
import qualified Bcc.Api.Cole as ColeApi
import           Bcc.Api.Crypto.Ed25519Bip32 (xPrvFromBytes)
import           Bcc.Api.Sophie

import qualified Bcc.CLI.Cole.Key as Cole
import           Bcc.CLI.Helpers (textShow)
import           Bcc.CLI.Sophie.Commands
import           Bcc.CLI.Sophie.Key (InputDecodeError, readSigningKeyFileAnyOf)
import           Bcc.CLI.Types (SigningKeyFile (..), VerificationKeyFile (..))


data SophieKeyCmdError
  = SophieKeyCmdReadFileError !(FileError TextEnvelopeError)
  | SophieKeyCmdReadKeyFileError !(FileError InputDecodeError)
  | SophieKeyCmdWriteFileError !(FileError ())
  | SophieKeyCmdColeKeyFailure !Cole.ColeKeyFailure
  | SophieKeyCmdColeKeyParseError
      !Text
      -- ^ Text representation of the parse error. Unfortunately, the actual
      -- error type isn't exported.
  | SophieKeyCmdItnKeyConvError !ItnKeyConversionError
  | SophieKeyCmdWrongKeyTypeError
  | SophieKeyCmdBccAddressSigningKeyFileError
      !(FileError BccAddressSigningKeyConversionError)
  | SophieKeyCmdNonLegacyKey !FilePath
  deriving Show

renderSophieKeyCmdError :: SophieKeyCmdError -> Text
renderSophieKeyCmdError err =
  case err of
    SophieKeyCmdReadFileError fileErr -> Text.pack (displayError fileErr)
    SophieKeyCmdReadKeyFileError fileErr -> Text.pack (displayError fileErr)
    SophieKeyCmdWriteFileError fileErr -> Text.pack (displayError fileErr)
    SophieKeyCmdColeKeyFailure e -> Cole.renderColeKeyFailure e
    SophieKeyCmdColeKeyParseError errTxt -> errTxt
    SophieKeyCmdItnKeyConvError convErr -> renderConversionError convErr
    SophieKeyCmdWrongKeyTypeError -> Text.pack "Please use a signing key file \
                                   \when converting ITN BIP32 or Extended keys"
    SophieKeyCmdBccAddressSigningKeyFileError fileErr ->
      Text.pack (displayError fileErr)
    SophieKeyCmdNonLegacyKey fp -> "Signing key at: " <> Text.pack fp <> " is not a legacy Cole signing key and should \
                                    \ not need to be converted."

runKeyCmd :: KeyCmd -> ExceptT SophieKeyCmdError IO ()
runKeyCmd cmd =
  case cmd of
    KeyGetVerificationKey skf vkf ->
      runGetVerificationKey skf vkf

    KeyNonExtendedKey evkf vkf ->
      runNonExtendedKey evkf vkf

    KeyConvertColeKey mPassword keytype skfOld skfNew ->
      runConvertColeKey mPassword keytype skfOld skfNew

    KeyConvertColeGenesisVKey oldVk newVkf ->
      runConvertColeGenesisVerificationKey oldVk newVkf
    
    KeyConvertColeVestedVKey oldVk newVkf ->
      runConvertColeVestedVerificationKey oldVk newVkf

    KeyConvertITNStakeKey itnKeyFile outFile ->
      runConvertITNStakeKey itnKeyFile outFile
    KeyConvertITNExtendedToStakeKey itnPrivKeyFile outFile ->
      runConvertITNExtendedToStakeKey itnPrivKeyFile outFile
    KeyConvertITNBip32ToStakeKey itnPrivKeyFile outFile ->
      runConvertITNBip32ToStakeKey itnPrivKeyFile outFile

    KeyConvertBccAddressSigningKey keyType skfOld skfNew ->
      runConvertBccAddressSigningKey keyType skfOld skfNew

runGetVerificationKey :: SigningKeyFile
                      -> VerificationKeyFile
                      -> ExceptT SophieKeyCmdError IO ()
runGetVerificationKey skf (VerificationKeyFile vkf) = do
    ssk <- firstExceptT SophieKeyCmdReadKeyFileError $
             readSigningKeyFile skf
    withSomeSigningKey ssk $ \sk ->
      let vk = getVerificationKey sk in
      firstExceptT SophieKeyCmdWriteFileError . newExceptT $
        writeFileTextEnvelope vkf Nothing vk


data SomeSigningKey
  = AColeSigningKey                           (SigningKey ColeKey)
  | APaymentSigningKey                        (SigningKey PaymentKey)
  | APaymentExtendedSigningKey                (SigningKey PaymentExtendedKey)
  | AStakeSigningKey                          (SigningKey StakeKey)
  | AStakeExtendedSigningKey                  (SigningKey StakeExtendedKey)
  | AStakePoolSigningKey                      (SigningKey StakePoolKey)
  | AGenesisSigningKey                        (SigningKey GenesisKey)
  | AGenesisExtendedSigningKey                (SigningKey GenesisExtendedKey)
  | AGenesisDelegateSigningKey                (SigningKey GenesisDelegateKey)
  | AGenesisDelegateExtendedSigningKey        (SigningKey GenesisDelegateExtendedKey)
  | AGenesisVestedSigningKey                  (SigningKey GenesisVestedKey)
  | AGenesisVestedExtendedSigningKey          (SigningKey GenesisVestedExtendedKey)
  | AGenesisVestedDelegateSigningKey          (SigningKey GenesisVestedDelegateKey)
  | AGenesisVestedDelegateExtendedSigningKey  (SigningKey GenesisVestedDelegateExtendedKey)
  | AGenesisUTxOSigningKey                    (SigningKey GenesisUTxOKey)
  | AVestedSigningKey                         (SigningKey VestedKey)
  | AVestedExtendedSigningKey                 (SigningKey VestedExtendedKey)
  | AVestedDelegateSigningKey                 (SigningKey VestedDelegateKey)
  | AVestedDelegateExtendedSigningKey         (SigningKey VestedDelegateExtendedKey)
  | AVestedUTxOSigningKey                     (SigningKey VestedUTxOKey)
  | AVrfSigningKey                            (SigningKey VrfKey)
  | AKesSigningKey                            (SigningKey KesKey)

withSomeSigningKey :: SomeSigningKey
                   -> (forall keyrole. Key keyrole => SigningKey keyrole -> a)
                   -> a
withSomeSigningKey ssk f =
    case ssk of
      AColeSigningKey                           sk -> f sk 
      APaymentSigningKey                        sk -> f sk
      APaymentExtendedSigningKey                sk -> f sk
      AStakeSigningKey                          sk -> f sk
      AStakeExtendedSigningKey                  sk -> f sk
      AStakePoolSigningKey                      sk -> f sk
      AGenesisSigningKey                        sk -> f sk
      AGenesisExtendedSigningKey                sk -> f sk
      AGenesisDelegateSigningKey                sk -> f sk
      AGenesisDelegateExtendedSigningKey        sk -> f sk
      AGenesisVestedSigningKey                  sk -> f sk
      AGenesisVestedExtendedSigningKey          sk -> f sk
      AGenesisVestedDelegateSigningKey          sk -> f sk
      AGenesisVestedDelegateExtendedSigningKey  sk -> f sk
      AGenesisUTxOSigningKey                    sk -> f sk
      AVestedSigningKey                         sk -> f sk
      AVestedExtendedSigningKey                 sk -> f sk
      AVestedDelegateSigningKey                 sk -> f sk
      AVestedDelegateExtendedSigningKey         sk -> f sk
      AVestedUTxOSigningKey                     sk -> f sk
      AVrfSigningKey                            sk -> f sk
      AKesSigningKey                            sk -> f sk

readSigningKeyFile
  :: SigningKeyFile
  -> ExceptT (FileError InputDecodeError) IO SomeSigningKey
readSigningKeyFile skFile =
    newExceptT $
      readSigningKeyFileAnyOf bech32FileTypes textEnvFileTypes skFile
  where
    textEnvFileTypes =
      [ FromSomeType (AsSigningKey AsColeKey)
                      AColeSigningKey
      , FromSomeType (AsSigningKey AsPaymentKey)
                      APaymentSigningKey
      , FromSomeType (AsSigningKey AsPaymentExtendedKey)
                      APaymentExtendedSigningKey
      , FromSomeType (AsSigningKey AsStakeKey)
                      AStakeSigningKey
      , FromSomeType (AsSigningKey AsStakeExtendedKey)
                      AStakeExtendedSigningKey
      , FromSomeType (AsSigningKey AsStakePoolKey)
                      AStakePoolSigningKey
      , FromSomeType (AsSigningKey AsGenesisKey)
                      AGenesisSigningKey
      , FromSomeType (AsSigningKey AsGenesisExtendedKey)
                      AGenesisExtendedSigningKey
      , FromSomeType (AsSigningKey AsGenesisDelegateKey)
                      AGenesisDelegateSigningKey
      , FromSomeType (AsSigningKey AsGenesisDelegateExtendedKey)
                      AGenesisDelegateExtendedSigningKey
      , FromSomeType (AsSigningKey AsGenesisVestedKey)
                      AGenesisVestedSigningKey
      , FromSomeType (AsSigningKey AsGenesisVestedExtendedKey)
                      AGenesisVestedExtendedSigningKey
      , FromSomeType (AsSigningKey AsGenesisVestedDelegateKey)
                      AGenesisVestedDelegateSigningKey
      , FromSomeType (AsSigningKey AsGenesisVestedDelegateExtendedKey)
                      AGenesisVestedDelegateExtendedSigningKey
      , FromSomeType (AsSigningKey AsGenesisUTxOKey)
                      AGenesisUTxOSigningKey
      , FromSomeType (AsSigningKey AsVestedKey)
                      AVestedSigningKey
      , FromSomeType (AsSigningKey AsVestedExtendedKey)
                      AVestedExtendedSigningKey
      , FromSomeType (AsSigningKey AsVestedDelegateKey)
                      AVestedDelegateSigningKey
      , FromSomeType (AsSigningKey AsVestedDelegateExtendedKey)
                      AVestedDelegateExtendedSigningKey
      , FromSomeType (AsSigningKey AsVestedUTxOKey)
                      AVestedUTxOSigningKey
      , FromSomeType (AsSigningKey AsVrfKey)
                      AVrfSigningKey
      , FromSomeType (AsSigningKey AsKesKey)
                      AKesSigningKey
      ]

    bech32FileTypes =
      [ FromSomeType (AsSigningKey AsPaymentKey)
                      APaymentSigningKey
      , FromSomeType (AsSigningKey AsPaymentExtendedKey)
                      APaymentExtendedSigningKey
      , FromSomeType (AsSigningKey AsStakeKey)
                      AStakeSigningKey
      , FromSomeType (AsSigningKey AsStakeExtendedKey)
                      AStakeExtendedSigningKey
      , FromSomeType (AsSigningKey AsStakePoolKey)
                      AStakePoolSigningKey
      , FromSomeType (AsSigningKey AsVrfKey)
                      AVrfSigningKey
      , FromSomeType (AsSigningKey AsKesKey)
                      AKesSigningKey
      ]


runNonExtendedKey :: VerificationKeyFile
                  -> VerificationKeyFile
                  -> ExceptT SophieKeyCmdError IO ()
runNonExtendedKey evkf (VerificationKeyFile vkf) = do
    evk <- firstExceptT SophieKeyCmdReadFileError $
             readExtendedVerificationKeyFile evkf
    withNonExtendedKey evk $ \vk ->
      firstExceptT SophieKeyCmdWriteFileError . newExceptT $
        writeFileTextEnvelope vkf Nothing vk

withNonExtendedKey :: SomeExtendedVerificationKey
                   -> (forall keyrole. Key keyrole => VerificationKey keyrole -> a)
                   -> a
withNonExtendedKey (APaymentExtendedVerificationKey vk) f =
    f (castVerificationKey vk :: VerificationKey PaymentKey)

withNonExtendedKey (AStakeExtendedVerificationKey vk) f =
    f (castVerificationKey vk :: VerificationKey StakeKey)

withNonExtendedKey (AGenesisExtendedVerificationKey vk) f =
    f (castVerificationKey vk :: VerificationKey GenesisKey)

withNonExtendedKey (AGenesisDelegateExtendedVerificationKey vk) f =
    f (castVerificationKey vk :: VerificationKey GenesisDelegateKey)

withNonExtendedKey (AGenesisVestedExtendedVerificationKey vk) f =
    f (castVerificationKey vk :: VerificationKey GenesisVestedKey)

withNonExtendedKey (AGenesisVestedDelegateExtendedVerificationKey vk) f =
    f (castVerificationKey vk :: VerificationKey GenesisVestedDelegateKey)

withNonExtendedKey (AVestedExtendedVerificationKey vk) f =
    f (castVerificationKey vk :: VerificationKey VestedKey)

withNonExtendedKey (AVestedDelegateExtendedVerificationKey vk) f =
    f (castVerificationKey vk :: VerificationKey VestedDelegateKey)

data SomeExtendedVerificationKey
  = APaymentExtendedVerificationKey               (VerificationKey PaymentExtendedKey)
  | AStakeExtendedVerificationKey                 (VerificationKey StakeExtendedKey)
  | AGenesisExtendedVerificationKey               (VerificationKey GenesisExtendedKey)
  | AGenesisDelegateExtendedVerificationKey       (VerificationKey GenesisDelegateExtendedKey)
  | AGenesisVestedExtendedVerificationKey         (VerificationKey GenesisVestedExtendedKey)
  | AGenesisVestedDelegateExtendedVerificationKey (VerificationKey GenesisVestedDelegateExtendedKey)
  | AVestedExtendedVerificationKey                (VerificationKey VestedExtendedKey)
  | AVestedDelegateExtendedVerificationKey        (VerificationKey VestedDelegateExtendedKey)

readExtendedVerificationKeyFile
  :: VerificationKeyFile
  -> ExceptT (FileError TextEnvelopeError) IO SomeExtendedVerificationKey
readExtendedVerificationKeyFile (VerificationKeyFile evkfile) =
    newExceptT $ readFileTextEnvelopeAnyOf fileTypes evkfile
  where
    fileTypes =
      [ FromSomeType (AsVerificationKey AsPaymentExtendedKey)
                      APaymentExtendedVerificationKey
      , FromSomeType (AsVerificationKey AsStakeExtendedKey)
                      AStakeExtendedVerificationKey
      , FromSomeType (AsVerificationKey AsGenesisExtendedKey)
                      AGenesisExtendedVerificationKey
      , FromSomeType (AsVerificationKey AsGenesisDelegateExtendedKey)
                      AGenesisDelegateExtendedVerificationKey
      , FromSomeType (AsVerificationKey AsGenesisVestedExtendedKey)
                      AGenesisVestedExtendedVerificationKey
      , FromSomeType (AsVerificationKey AsGenesisVestedDelegateExtendedKey)
                      AGenesisVestedDelegateExtendedVerificationKey
      , FromSomeType (AsVerificationKey AsVestedExtendedKey)
                      AVestedExtendedVerificationKey
      , FromSomeType (AsVerificationKey AsVestedDelegateExtendedKey)
                      AVestedDelegateExtendedVerificationKey
      ]


runConvertColeKey
  :: Maybe Text      -- ^ Password (if applicable)
  -> ColeKeyType
  -> SomeKeyFile     -- ^ Input file: old format
  -> OutputFile      -- ^ Output file: new format
  -> ExceptT SophieKeyCmdError IO ()
runConvertColeKey mPwd (ColePaymentKey format) (ASigningKeyFile skeyPathOld) =
    convertColeSigningKey mPwd format convert skeyPathOld
  where
    convert :: Cole.SigningKey -> SigningKey ColeKey
    convert = ColeSigningKey

runConvertColeKey mPwd (ColeGenesisKey format) (ASigningKeyFile skeyPathOld) =
    convertColeSigningKey mPwd format convert skeyPathOld
  where
    convert :: Cole.SigningKey -> SigningKey GenesisExtendedKey
    convert (Cole.SigningKey xsk) = GenesisExtendedSigningKey xsk

runConvertColeKey mPwd (ColeGenesisVestedKey format) (ASigningKeyFile skeyPathOld) =
    convertColeSigningKey mPwd format convert skeyPathOld
  where
    convert :: Cole.SigningKey -> SigningKey GenesisVestedExtendedKey
    convert (Cole.SigningKey xsk) = GenesisVestedExtendedSigningKey xsk

runConvertColeKey mPwd (ColeVestedKey format) (ASigningKeyFile skeyPathOld) =
    convertColeSigningKey mPwd format convert skeyPathOld
  where
    convert :: Cole.SigningKey -> SigningKey VestedExtendedKey
    convert (Cole.SigningKey xsk) = VestedExtendedSigningKey xsk 

runConvertColeKey mPwd (ColeDelegateKey format) (ASigningKeyFile skeyPathOld) =
    convertColeSigningKey mPwd format convert skeyPathOld
  where
    convert :: Cole.SigningKey -> SigningKey GenesisDelegateExtendedKey
    convert (Cole.SigningKey xsk) = GenesisDelegateExtendedSigningKey xsk

runConvertColeKey mPwd (ColeVestedDelegateKey format) (ASigningKeyFile skeyPathOld) =
    convertColeSigningKey mPwd format convert skeyPathOld
  where
    convert :: Cole.SigningKey -> SigningKey VestedDelegateExtendedKey
    convert (Cole.SigningKey xsk) = VestedDelegateExtendedSigningKey xsk

runConvertColeKey _ (ColePaymentKey NonLegacyColeKeyFormat)
                     (AVerificationKeyFile vkeyPathOld) =
    convertColeVerificationKey convert vkeyPathOld
  where
    convert :: Cole.VerificationKey -> VerificationKey ColeKey
    convert = ColeVerificationKey

runConvertColeKey _ (ColeGenesisKey NonLegacyColeKeyFormat)
                     (AVerificationKeyFile vkeyPathOld) =
    convertColeVerificationKey convert vkeyPathOld
  where
    convert :: Cole.VerificationKey -> VerificationKey GenesisExtendedKey
    convert (Cole.VerificationKey xvk) = GenesisExtendedVerificationKey xvk

runConvertColeKey _ (ColeGenesisVestedKey NonLegacyColeKeyFormat)
                     (AVerificationKeyFile vkeyPathOld) =
    convertColeVerificationKey convert vkeyPathOld
  where
    convert :: Cole.VerificationKey -> VerificationKey GenesisVestedExtendedKey
    convert (Cole.VerificationKey xvk) = GenesisVestedExtendedVerificationKey xvk

runConvertColeKey _ (ColeVestedKey NonLegacyColeKeyFormat)
                     (AVerificationKeyFile vkeyPathOld) =
    convertColeVerificationKey convert vkeyPathOld
  where
    convert :: Cole.VerificationKey -> VerificationKey VestedExtendedKey
    convert (Cole.VerificationKey xvk) = VestedExtendedVerificationKey xvk

runConvertColeKey _ (ColeDelegateKey NonLegacyColeKeyFormat)
                     (AVerificationKeyFile vkeyPathOld) =
    convertColeVerificationKey convert vkeyPathOld
  where
    convert :: Cole.VerificationKey
            -> VerificationKey GenesisDelegateExtendedKey
    convert (Cole.VerificationKey xvk) =
      GenesisDelegateExtendedVerificationKey xvk

runConvertColeKey _ (ColeVestedDelegateKey NonLegacyColeKeyFormat)
                     (AVerificationKeyFile vkeyPathOld) =
    convertColeVerificationKey convert vkeyPathOld
  where
    convert :: Cole.VerificationKey
            -> VerificationKey VestedDelegateExtendedKey
    convert (Cole.VerificationKey xvk) =
      VestedDelegateExtendedVerificationKey xvk

runConvertColeKey _ (ColePaymentKey  LegacyColeKeyFormat)
                      AVerificationKeyFile{} =
    const legacyVerificationKeysNotSupported

runConvertColeKey _ (ColeGenesisKey  LegacyColeKeyFormat)
                      AVerificationKeyFile{} =
    const legacyVerificationKeysNotSupported

runConvertColeKey _ (ColeGenesisVestedKey  LegacyColeKeyFormat)
                      AVerificationKeyFile{} =
    const legacyVerificationKeysNotSupported

runConvertColeKey _ (ColeVestedKey  LegacyColeKeyFormat)
                      AVerificationKeyFile{} =
    const legacyVerificationKeysNotSupported

runConvertColeKey _ (ColeDelegateKey LegacyColeKeyFormat)
                      AVerificationKeyFile{} =
    const legacyVerificationKeysNotSupported

runConvertColeKey _ (ColeVestedDelegateKey LegacyColeKeyFormat)
                      AVerificationKeyFile{} =
    const legacyVerificationKeysNotSupported

legacyVerificationKeysNotSupported :: ExceptT e IO a
legacyVerificationKeysNotSupported =
    liftIO $ do
      putStrLn $ "convert keys: cole legacy format not supported for "
              ++ "verification keys. Convert the signing key and then get the "
              ++ "verification key."
      exitFailure


convertColeSigningKey
  :: forall keyrole.
     Key keyrole
  => Maybe Text          -- ^ Password (if applicable)
  -> ColeKeyFormat
  -> (Cole.SigningKey -> SigningKey keyrole)
  -> SigningKeyFile      -- ^ Input file: old format
  -> OutputFile          -- ^ Output file: new format
  -> ExceptT SophieKeyCmdError IO ()
convertColeSigningKey mPwd coleFormat convert
                       skeyPathOld
                       (OutputFile skeyPathNew) = do


    sKey <- firstExceptT SophieKeyCmdColeKeyFailure
              $ Cole.readColeSigningKey coleFormat skeyPathOld

    -- Account for password protected legacy Cole keys
    unprotectedSk <- case sKey of
                       ColeApi.AColeSigningKeyLegacy (ColeSigningKeyLegacy sk@(Crypto.SigningKey xprv)) ->
                         case mPwd of
                           -- Change password to empty string
                           Just pwd -> return . Crypto.SigningKey
                                         $ Crypto.xPrvChangePass (encodeUtf8 pwd) (encodeUtf8 "") xprv
                           Nothing -> return sk
                       ColeApi.AColeSigningKey (ColeSigningKey sk) -> return sk


    let sk' :: SigningKey keyrole
        sk' = convert unprotectedSk

    firstExceptT SophieKeyCmdWriteFileError . newExceptT $
      writeFileTextEnvelope skeyPathNew Nothing sk'

convertColeVerificationKey
  :: forall keyrole.
     Key keyrole
  => (Cole.VerificationKey -> VerificationKey keyrole)
  -> VerificationKeyFile -- ^ Input file: old format
  -> OutputFile          -- ^ Output file: new format
  -> ExceptT SophieKeyCmdError IO ()
convertColeVerificationKey convert
                            (VerificationKeyFile vkeyPathOld)
                            (OutputFile vkeyPathNew) = do

    vk <- firstExceptT SophieKeyCmdColeKeyFailure $
            Cole.readPaymentVerificationKey (Cole.VerificationKeyFile vkeyPathOld)

    let vk' :: VerificationKey keyrole
        vk' = convert vk

    firstExceptT SophieKeyCmdWriteFileError . newExceptT $
      writeFileTextEnvelope vkeyPathNew Nothing vk'


runConvertColeGenesisVerificationKey
  :: VerificationKeyBase64  -- ^ Input key raw old format
  -> OutputFile             -- ^ Output file: new format
  -> ExceptT SophieKeyCmdError IO ()
runConvertColeGenesisVerificationKey (VerificationKeyBase64 b64ColeVKey)
                                      (OutputFile vkeyPathNew) = do

    vk <- firstExceptT (SophieKeyCmdColeKeyParseError . show)
        . hoistEither
        . Cole.Crypto.parseFullVerificationKey
        . Text.pack
        $ b64ColeVKey

    let vk' :: VerificationKey GenesisKey
        vk' = convert vk

    firstExceptT SophieKeyCmdWriteFileError . newExceptT $
      writeFileTextEnvelope vkeyPathNew Nothing vk'
  where
    convert :: Cole.VerificationKey -> VerificationKey GenesisKey
    convert (Cole.VerificationKey xvk) =
      castVerificationKey (GenesisExtendedVerificationKey xvk)

runConvertColeVestedVerificationKey
  :: VerificationKeyBase64  -- ^ Input key raw old format
  -> OutputFile             -- ^ Output file: new format
  -> ExceptT SophieKeyCmdError IO ()
runConvertColeVestedVerificationKey (VerificationKeyBase64 b64ColeVKey)
                                      (OutputFile vkeyPathNew) = do

    vk <- firstExceptT (SophieKeyCmdColeKeyParseError . show)
        . hoistEither
        . Cole.Crypto.parseFullVerificationKey
        . Text.pack
        $ b64ColeVKey

    let vk' :: VerificationKey VestedKey
        vk' = convert vk

    firstExceptT SophieKeyCmdWriteFileError . newExceptT $
      writeFileTextEnvelope vkeyPathNew Nothing vk'
  where
    convert :: Cole.VerificationKey -> VerificationKey VestedKey
    convert (Cole.VerificationKey xvk) =
      castVerificationKey (VestedExtendedVerificationKey xvk)


--------------------------------------------------------------------------------
-- ITN verification/signing key conversion to Haskell verficiation/signing keys
--------------------------------------------------------------------------------

runConvertITNStakeKey
  :: SomeKeyFile
  -> OutputFile
  -> ExceptT SophieKeyCmdError IO ()
runConvertITNStakeKey (AVerificationKeyFile (VerificationKeyFile vk)) (OutputFile outFile) = do
  bech32publicKey <- firstExceptT SophieKeyCmdItnKeyConvError . newExceptT $
                     readFileITNKey vk
  vkey <- hoistEither
    . first SophieKeyCmdItnKeyConvError
    $ convertITNVerificationKey bech32publicKey
  firstExceptT SophieKeyCmdWriteFileError . newExceptT $
    writeFileTextEnvelope outFile Nothing vkey

runConvertITNStakeKey (ASigningKeyFile (SigningKeyFile sk)) (OutputFile outFile) = do
  bech32privateKey <- firstExceptT SophieKeyCmdItnKeyConvError . newExceptT $
                      readFileITNKey sk
  skey <- hoistEither
    . first SophieKeyCmdItnKeyConvError
    $ convertITNSigningKey bech32privateKey
  firstExceptT SophieKeyCmdWriteFileError . newExceptT $
    writeFileTextEnvelope outFile Nothing skey

runConvertITNExtendedToStakeKey :: SomeKeyFile -> OutputFile -> ExceptT SophieKeyCmdError IO ()
runConvertITNExtendedToStakeKey (AVerificationKeyFile _) _ = left SophieKeyCmdWrongKeyTypeError
runConvertITNExtendedToStakeKey (ASigningKeyFile (SigningKeyFile sk)) (OutputFile outFile) = do
  bech32privateKey <- firstExceptT SophieKeyCmdItnKeyConvError . newExceptT $ readFileITNKey sk
  skey <- hoistEither . first SophieKeyCmdItnKeyConvError
            $ convertITNExtendedSigningKey bech32privateKey
  firstExceptT SophieKeyCmdWriteFileError . newExceptT
    $ writeFileTextEnvelope outFile Nothing skey

runConvertITNBip32ToStakeKey :: SomeKeyFile -> OutputFile -> ExceptT SophieKeyCmdError IO ()
runConvertITNBip32ToStakeKey (AVerificationKeyFile _) _ = left SophieKeyCmdWrongKeyTypeError
runConvertITNBip32ToStakeKey (ASigningKeyFile (SigningKeyFile sk)) (OutputFile outFile) = do
  bech32privateKey <- firstExceptT SophieKeyCmdItnKeyConvError . newExceptT $ readFileITNKey sk
  skey <- hoistEither . first SophieKeyCmdItnKeyConvError
            $ convertITNBIP32SigningKey bech32privateKey
  firstExceptT SophieKeyCmdWriteFileError . newExceptT
    $ writeFileTextEnvelope outFile Nothing skey

-- | An error that can occur while converting an Incentivized Testnet (ITN)
-- key.
data ItnKeyConversionError
  = ItnKeyBech32DecodeError !Bech32DecodeError
  | ItnReadBech32FileError !FilePath !IOException
  | ItnSigningKeyDeserialisationError !ByteString
  | ItnVerificationKeyDeserialisationError !ByteString
  deriving Show

-- | Render an error message for an 'ItnKeyConversionError'.
renderConversionError :: ItnKeyConversionError -> Text
renderConversionError err =
  case err of
    ItnKeyBech32DecodeError decErr ->
      "Error decoding Bech32 key: " <> Text.pack (displayError decErr)
    ItnReadBech32FileError fp readErr ->
      "Error reading Bech32 key at: " <> textShow fp
                        <> " Error: " <> Text.pack (displayException readErr)
    ItnSigningKeyDeserialisationError _sKey ->
      -- Sensitive data, such as the signing key, is purposely not included in
      -- the error message.
      "Error deserialising signing key."
    ItnVerificationKeyDeserialisationError vKey ->
      "Error deserialising verification key: " <> textShow (BSC.unpack vKey)

-- | Convert public ed25519 key to a Sophie stake verification key
convertITNVerificationKey :: Text -> Either ItnKeyConversionError (VerificationKey StakeKey)
convertITNVerificationKey pubKey = do
  (_, _, keyBS) <- first ItnKeyBech32DecodeError (decodeBech32 pubKey)
  case DSIGN.rawDeserialiseVerKeyDSIGN keyBS of
    Just verKey -> Right . StakeVerificationKey $ Sophie.VKey verKey
    Nothing -> Left $ ItnVerificationKeyDeserialisationError keyBS

-- | Convert private ed22519 key to a Sophie signing key.
convertITNSigningKey :: Text -> Either ItnKeyConversionError (SigningKey StakeKey)
convertITNSigningKey privKey = do
  (_, _, keyBS) <- first ItnKeyBech32DecodeError (decodeBech32 privKey)
  case DSIGN.rawDeserialiseSignKeyDSIGN keyBS of
    Just signKey -> Right $ StakeSigningKey signKey
    Nothing -> Left $ ItnSigningKeyDeserialisationError keyBS

-- | Convert extended private ed22519 key to a Sophie signing key
-- Extended private key = 64 bytes,
-- Public key = 32 bytes.
convertITNExtendedSigningKey :: Text -> Either ItnKeyConversionError (SigningKey StakeExtendedKey)
convertITNExtendedSigningKey privKey = do
  (_, _, privkeyBS) <- first ItnKeyBech32DecodeError (decodeBech32 privKey)
  let dummyChainCode = BS.replicate 32 0
  case xPrvFromBytes $ BS.concat [privkeyBS, dummyChainCode] of
    Just xprv -> Right $ StakeExtendedSigningKey xprv
    Nothing -> Left $ ItnSigningKeyDeserialisationError privkeyBS

-- BIP32 Private key = 96 bytes (64 bytes extended private key + 32 bytes chaincode)
-- BIP32 Public Key = 64 Bytes
convertITNBIP32SigningKey :: Text -> Either ItnKeyConversionError (SigningKey StakeExtendedKey)
convertITNBIP32SigningKey privKey = do
  (_, _, privkeyBS) <- first ItnKeyBech32DecodeError (decodeBech32 privKey)
  case xPrvFromBytes privkeyBS of
    Just xprv -> Right $ StakeExtendedSigningKey xprv
    Nothing -> Left $ ItnSigningKeyDeserialisationError privkeyBS

readFileITNKey :: FilePath -> IO (Either ItnKeyConversionError Text)
readFileITNKey fp = do
  eStr <- Exception.try $ readFile fp
  case eStr of
    Left e -> return . Left $ ItnReadBech32FileError fp e
    Right str -> return . Right . Text.concat $ Text.words str

--------------------------------------------------------------------------------
-- `bcc-address` extended signing key conversions
--------------------------------------------------------------------------------

runConvertBccAddressSigningKey
  :: BccAddressKeyType
  -> SigningKeyFile
  -> OutputFile
  -> ExceptT SophieKeyCmdError IO ()
runConvertBccAddressSigningKey keyType skFile (OutputFile outFile) = do
  sKey <- firstExceptT SophieKeyCmdBccAddressSigningKeyFileError
    . newExceptT
    $ readSomeBccAddressSigningKeyFile keyType skFile
  firstExceptT SophieKeyCmdWriteFileError . newExceptT
    $ writeSomeBccAddressSigningKeyFile outFile sKey

-- | Some kind of signing key that was converted from a @bcc-address@
-- signing key.
data SomeBccAddressSigningKey
  = ABccAddrSophiePaymentSigningKey !(SigningKey PaymentExtendedKey)
  | ABccAddrSophieStakeSigningKey !(SigningKey StakeExtendedKey)
  | ABccAddrColeSigningKey !(SigningKey ColeKey)

-- | An error that can occur while converting a @bcc-address@ extended
-- signing key.
data BccAddressSigningKeyConversionError
  = BccAddressSigningKeyBech32DecodeError !Bech32DecodeError
  -- ^ There was an error in decoding the string as Bech32.
  | BccAddressSigningKeyDeserialisationError !ByteString
  -- ^ There was an error in converting the @bcc-address@ extended signing
  -- key.
  deriving (Show, Eq)

instance Error BccAddressSigningKeyConversionError where
  displayError = Text.unpack . renderBccAddressSigningKeyConversionError

-- | Render an error message for a 'BccAddressSigningKeyConversionError'.
renderBccAddressSigningKeyConversionError
  :: BccAddressSigningKeyConversionError
  -> Text
renderBccAddressSigningKeyConversionError err =
  case err of
    BccAddressSigningKeyBech32DecodeError decErr ->
      Text.pack (displayError decErr)
    BccAddressSigningKeyDeserialisationError _bs ->
      -- Sensitive data, such as the signing key, is purposely not included in
      -- the error message.
      "Error deserialising bcc-address signing key."

-- | Decode a Bech32-encoded string.
decodeBech32
  :: Text
  -> Either Bech32DecodeError (Bech32.HumanReadablePart, Bech32.DataPart, ByteString)
decodeBech32 bech32Str =
  case Bech32.decodeLenient bech32Str of
    Left err -> Left (Bech32DecodingError err)
    Right (hrPart, dataPart) ->
      case Bech32.dataPartToBytes dataPart of
        Nothing ->
          Left $ Bech32DataPartToBytesError (Bech32.dataPartToText dataPart)
        Just bs -> Right (hrPart, dataPart, bs)

-- | Convert a Ed25519 BIP32 extended signing key (96 bytes) to a @bcc-crypto@
-- style extended signing key.
--
-- Note that both the ITN and @bcc-address@ use this key format.
convertBip32SigningKey
  :: ByteString
  -> Either BccAddressSigningKeyConversionError Crypto.XPrv
convertBip32SigningKey signingKeyBs =
  case xPrvFromBytes signingKeyBs of
    Just xPrv -> Right xPrv
    Nothing ->
      Left $ BccAddressSigningKeyDeserialisationError signingKeyBs

-- | Read a file containing a Bech32-encoded Ed25519 BIP32 extended signing
-- key.
readBech32Bip32SigningKeyFile
  :: SigningKeyFile
  -> IO (Either (FileError BccAddressSigningKeyConversionError) Crypto.XPrv)
readBech32Bip32SigningKeyFile (SigningKeyFile fp) = do
  eStr <- Exception.try $ readFile fp
  case eStr of
    Left e -> pure . Left $ FileIOError fp e
    Right str ->
      case decodeBech32 (Text.concat $ Text.words str) of
        Left err ->
          pure $ Left $
            FileError fp (BccAddressSigningKeyBech32DecodeError err)
        Right (_hrPart, _dataPart, bs) ->
          pure $ first (FileError fp) (convertBip32SigningKey bs)

-- | Read a file containing a Bech32-encoded @bcc-address@ extended
-- signing key.
readSomeBccAddressSigningKeyFile
  :: BccAddressKeyType
  -> SigningKeyFile
  -> IO (Either (FileError BccAddressSigningKeyConversionError) SomeBccAddressSigningKey)
readSomeBccAddressSigningKeyFile keyType skFile = do
    xPrv <- readBech32Bip32SigningKeyFile skFile
    pure (toSomeBccAddressSigningKey <$> xPrv)
  where
    toSomeBccAddressSigningKey :: Crypto.XPrv -> SomeBccAddressSigningKey
    toSomeBccAddressSigningKey xPrv =
      case keyType of
        BccAddressSophiePaymentKey ->
          ABccAddrSophiePaymentSigningKey
            (PaymentExtendedSigningKey xPrv)
        BccAddressSophieStakeKey ->
          ABccAddrSophieStakeSigningKey (StakeExtendedSigningKey xPrv)
        BccAddressIcarusPaymentKey ->
          ABccAddrColeSigningKey $
            ColeSigningKey (Cole.SigningKey xPrv)
        BccAddressColePaymentKey ->
          ABccAddrColeSigningKey $
            ColeSigningKey (Cole.SigningKey xPrv)

-- | Write a text envelope formatted file containing a @bcc-address@
-- extended signing key, but converted to a format supported by @bcc-cli@.
writeSomeBccAddressSigningKeyFile
  :: FilePath
  -> SomeBccAddressSigningKey
  -> IO (Either (FileError ()) ())
writeSomeBccAddressSigningKeyFile outFile skey =
  case skey of
    ABccAddrSophiePaymentSigningKey sk ->
      writeFileTextEnvelope outFile Nothing sk
    ABccAddrSophieStakeSigningKey sk ->
      writeFileTextEnvelope outFile Nothing sk
    ABccAddrColeSigningKey sk ->
      writeFileTextEnvelope outFile Nothing sk
