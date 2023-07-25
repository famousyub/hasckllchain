{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bcc.CLI.Sophie.Run.Address
  ( SophieAddressCmdError(..)
  , SomeAddressVerificationKey(..)
  , buildSophieAddress
  , renderSophieAddressCmdError
  , runAddressCmd
  , runAddressKeyGen
  , readAddressVerificationKeyTextOrFile
  ) where

import           Bcc.Prelude hiding (putStrLn)


import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import           Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)

import           Bcc.Api
import           Bcc.Api.Sophie

import           Bcc.CLI.Helpers
import           Bcc.CLI.Sophie.Key (InputDecodeError, PaymentVerifier (..),
                   StakeVerifier (..), VerificationKeyTextOrFile,
                   VerificationKeyTextOrFileError (..), readVerificationKeyOrFile,
                   readVerificationKeyTextOrFileAnyOf, renderVerificationKeyTextOrFileError)
import           Bcc.CLI.Sophie.Parsers (AddressCmd (..), AddressKeyType (..), OutputFile (..))
import           Bcc.CLI.Sophie.Run.Address.Info (SophieAddressInfoError, runAddressInfo)
import           Bcc.CLI.Sophie.Script
import           Bcc.CLI.Types

data SophieAddressCmdError
  = SophieAddressCmdAddressInfoError !SophieAddressInfoError
  | SophieAddressCmdReadKeyFileError !(FileError InputDecodeError)
  | SophieAddressCmdReadScriptFileError !(FileError ScriptDecodeError)
  | SophieAddressCmdVerificationKeyTextOrFileError !VerificationKeyTextOrFileError
  | SophieAddressCmdWriteFileError !(FileError ())
  deriving Show

renderSophieAddressCmdError :: SophieAddressCmdError -> Text
renderSophieAddressCmdError err =
  case err of
    SophieAddressCmdAddressInfoError addrInfoErr ->
      Text.pack (displayError addrInfoErr)
    SophieAddressCmdReadKeyFileError fileErr ->
      Text.pack (displayError fileErr)
    SophieAddressCmdVerificationKeyTextOrFileError vkTextOrFileErr ->
      renderVerificationKeyTextOrFileError vkTextOrFileErr
    SophieAddressCmdReadScriptFileError fileErr ->
      Text.pack (displayError fileErr)
    SophieAddressCmdWriteFileError fileErr -> Text.pack (displayError fileErr)

runAddressCmd :: AddressCmd -> ExceptT SophieAddressCmdError IO ()
runAddressCmd cmd =
  case cmd of
    AddressKeyGen kt vkf skf -> runAddressKeyGen kt vkf skf
    AddressKeyHash vkf mOFp -> runAddressKeyHash vkf mOFp
    AddressBuild paymentVerifier mbStakeVerifier nw mOutFp -> runAddressBuild paymentVerifier mbStakeVerifier nw mOutFp
    AddressBuildMultiSig sFp nId mOutFp -> runAddressBuildScript sFp nId mOutFp
    AddressInfo txt mOFp -> firstExceptT SophieAddressCmdAddressInfoError $ runAddressInfo txt mOFp

runAddressKeyGen :: AddressKeyType
                 -> VerificationKeyFile
                 -> SigningKeyFile
                 -> ExceptT SophieAddressCmdError IO ()
runAddressKeyGen kt (VerificationKeyFile vkeyPath) (SigningKeyFile skeyPath) =
    case kt of
      AddressKeySophie         -> generateAndWriteKeyFiles AsPaymentKey
      AddressKeySophieExtended -> generateAndWriteKeyFiles AsPaymentExtendedKey
      AddressKeyCole           -> generateAndWriteKeyFiles AsColeKey
  where
    generateAndWriteKeyFiles asType = do
      skey <- liftIO $ generateSigningKey asType
      let vkey = getVerificationKey skey
      firstExceptT SophieAddressCmdWriteFileError
        . newExceptT
        $ writeFileTextEnvelope skeyPath (Just skeyDesc) skey
      firstExceptT SophieAddressCmdWriteFileError
        . newExceptT
        $ writeFileTextEnvelope vkeyPath (Just vkeyDesc) vkey

    skeyDesc, vkeyDesc :: TextEnvelopeDescr
    skeyDesc = "Payment Signing Key"
    vkeyDesc = "Payment Verification Key"


runAddressKeyHash :: VerificationKeyTextOrFile
                  -> Maybe OutputFile
                  -> ExceptT SophieAddressCmdError IO ()
runAddressKeyHash vkeyTextOrFile mOutputFp = do
  vkey <- firstExceptT SophieAddressCmdVerificationKeyTextOrFileError $
            readAddressVerificationKeyTextOrFile vkeyTextOrFile

  let hexKeyHash = foldSomeAddressVerificationKey
                     (serialiseToRawBytesHex . verificationKeyHash) vkey

  case mOutputFp of
    Just (OutputFile fpath) -> liftIO $ BS.writeFile fpath hexKeyHash
    Nothing -> liftIO $ BS.putStrLn hexKeyHash


runAddressBuild :: PaymentVerifier
                -> Maybe StakeVerifier
                -> NetworkId
                -> Maybe OutputFile
                -> ExceptT SophieAddressCmdError IO ()
runAddressBuild paymentVerifier mbStakeVerifier nw mOutFp = do
  outText <- case paymentVerifier of
    PaymentVerifierKey payVkeyTextOrFile -> do
      payVKey <- firstExceptT SophieAddressCmdVerificationKeyTextOrFileError $
        readAddressVerificationKeyTextOrFile payVkeyTextOrFile

      addr <- case payVKey of
        AColeVerificationKey vk ->
          return (AddressCole (makeColeAddress nw vk))

        APaymentVerificationKey vk ->
          AddressSophie <$> buildSophieAddress vk mbStakeVerifier nw

        APaymentExtendedVerificationKey vk ->
          AddressSophie <$> buildSophieAddress (castVerificationKey vk) mbStakeVerifier nw

        AGenesisUTxOVerificationKey vk ->
          AddressSophie <$> buildSophieAddress (castVerificationKey vk) mbStakeVerifier nw

      return $ serialiseAddress (addr :: AddressAny)

    PaymentVerifierScriptFile (ScriptFile fp) -> do
      ScriptInAnyLang _lang script <-
        firstExceptT SophieAddressCmdReadScriptFileError $
          readFileScriptInAnyLang fp

      let payCred = PaymentCredentialByScript (hashScript script)

      serialiseAddress . makeSophieAddress nw payCred <$> makeStakeAddressRef mbStakeVerifier

  case mOutFp of
    Just (OutputFile fpath) -> liftIO $ Text.writeFile fpath outText
    Nothing                 -> liftIO $ Text.putStr          outText

makeStakeAddressRef
  :: Maybe StakeVerifier
  -> ExceptT SophieAddressCmdError IO StakeAddressReference
makeStakeAddressRef mbStakeVerifier = do
  case mbStakeVerifier of
    Nothing -> pure NoStakeAddress
    Just stakeVerifier -> case stakeVerifier of
      StakeVerifierKey stkVkeyOrFile -> do
        mstakeVKey <- firstExceptT SophieAddressCmdReadKeyFileError $
          fmap Just $ newExceptT $ readVerificationKeyOrFile AsStakeKey stkVkeyOrFile

        return $ maybe NoStakeAddress
          (StakeAddressByValue . StakeCredentialByKey . verificationKeyHash)
          mstakeVKey

      StakeVerifierScriptFile (ScriptFile fp) -> do
        ScriptInAnyLang _lang script <-
          firstExceptT SophieAddressCmdReadScriptFileError $
            readFileScriptInAnyLang fp

        let stakeCred = StakeCredentialByScript (hashScript script)
        return (StakeAddressByValue stakeCred)

buildSophieAddress
  :: VerificationKey PaymentKey
  -> Maybe StakeVerifier
  -> NetworkId
  -> ExceptT SophieAddressCmdError IO (Address SophieAddr)
buildSophieAddress vkey mbStakeVerifier nw =
  makeSophieAddress nw (PaymentCredentialByKey (verificationKeyHash vkey)) <$> makeStakeAddressRef mbStakeVerifier


--
-- Handling the variety of address key types
--

-- TODO: if we could make unions like this an instance of the Key class then
-- it would simplify some of the code above
data SomeAddressVerificationKey
  = AColeVerificationKey           (VerificationKey ColeKey)
  | APaymentVerificationKey         (VerificationKey PaymentKey)
  | APaymentExtendedVerificationKey (VerificationKey PaymentExtendedKey)
  | AGenesisUTxOVerificationKey     (VerificationKey GenesisUTxOKey)
  deriving (Show)

foldSomeAddressVerificationKey :: (forall keyrole. Key keyrole =>
                                   VerificationKey keyrole -> a)
                               -> SomeAddressVerificationKey -> a
foldSomeAddressVerificationKey f (AColeVerificationKey           vk) = f vk
foldSomeAddressVerificationKey f (APaymentVerificationKey         vk) = f vk
foldSomeAddressVerificationKey f (APaymentExtendedVerificationKey vk) = f vk
foldSomeAddressVerificationKey f (AGenesisUTxOVerificationKey     vk) = f vk

readAddressVerificationKeyTextOrFile
  :: VerificationKeyTextOrFile
  -> ExceptT VerificationKeyTextOrFileError IO SomeAddressVerificationKey
readAddressVerificationKeyTextOrFile vkTextOrFile =
    newExceptT $
      readVerificationKeyTextOrFileAnyOf bech32Types textEnvTypes vkTextOrFile
  where
    bech32Types =
      [ FromSomeType (AsVerificationKey AsPaymentKey)
                     APaymentVerificationKey
      , FromSomeType (AsVerificationKey AsPaymentExtendedKey)
                     APaymentExtendedVerificationKey
      ]

    textEnvTypes =
      [ FromSomeType (AsVerificationKey AsColeKey)
                     AColeVerificationKey
      , FromSomeType (AsVerificationKey AsPaymentKey)
                     APaymentVerificationKey
      , FromSomeType (AsVerificationKey AsPaymentExtendedKey)
                     APaymentExtendedVerificationKey
      , FromSomeType (AsVerificationKey AsGenesisUTxOKey)
                     AGenesisUTxOVerificationKey
      ]

--
-- Multisig addresses
--

runAddressBuildScript
  :: ScriptFile
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT SophieAddressCmdError IO ()
runAddressBuildScript scriptFile networkId mOutputFile = do
  liftIO $ deprecationWarning "'address build'"
  runAddressBuild (PaymentVerifierScriptFile scriptFile) Nothing networkId mOutputFile

