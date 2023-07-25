module Bcc.CLI.Sophie.Run.StakeAddress
  ( SophieStakeAddressCmdError(SophieStakeAddressCmdReadKeyFileError)
  , renderSophieStakeAddressCmdError
  , runStakeAddressCmd
  , runStakeAddressKeyGen
  ) where

import           Bcc.Prelude

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import           Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)

import           Bcc.Api
import           Bcc.Api.Sophie

import           Bcc.CLI.Sophie.Key (InputDecodeError, StakeVerifier (..),
                   VerificationKeyOrFile, VerificationKeyOrHashOrFile, readVerificationKeyOrFile,
                   readVerificationKeyOrHashOrFile)
import           Bcc.CLI.Sophie.Parsers
import           Bcc.CLI.Sophie.Script (ScriptDecodeError, readFileScriptInAnyLang)
import           Bcc.CLI.Types

data SophieStakeAddressCmdError
  = SophieStakeAddressCmdReadKeyFileError !(FileError InputDecodeError)
  | SophieStakeAddressCmdReadScriptFileError !(FileError ScriptDecodeError)
  | SophieStakeAddressCmdWriteFileError !(FileError ())
  deriving Show

renderSophieStakeAddressCmdError :: SophieStakeAddressCmdError -> Text
renderSophieStakeAddressCmdError err =
  case err of
    SophieStakeAddressCmdReadKeyFileError fileErr -> Text.pack (displayError fileErr)
    SophieStakeAddressCmdWriteFileError fileErr -> Text.pack (displayError fileErr)
    SophieStakeAddressCmdReadScriptFileError fileErr -> Text.pack (displayError fileErr)

runStakeAddressCmd :: StakeAddressCmd -> ExceptT SophieStakeAddressCmdError IO ()
runStakeAddressCmd (StakeAddressKeyGen vk sk) = runStakeAddressKeyGen vk sk
runStakeAddressCmd (StakeAddressKeyHash vk mOutputFp) = runStakeAddressKeyHash vk mOutputFp
runStakeAddressCmd (StakeAddressBuild vk nw mOutputFp) = runStakeAddressBuild vk nw mOutputFp
runStakeAddressCmd (StakeRegistrationCert stakeVerifier outputFp) =
  runStakeCredentialRegistrationCert stakeVerifier outputFp
runStakeAddressCmd (StakeCredentialDelegationCert stakeVerifier stkPoolVerKeyHashOrFp outputFp) =
  runStakeCredentialDelegationCert stakeVerifier stkPoolVerKeyHashOrFp outputFp
runStakeAddressCmd (StakeCredentialDeRegistrationCert stakeVerifier outputFp) =
  runStakeCredentialDeRegistrationCert stakeVerifier outputFp


--
-- Stake address command implementations
--

runStakeAddressKeyGen :: VerificationKeyFile -> SigningKeyFile -> ExceptT SophieStakeAddressCmdError IO ()
runStakeAddressKeyGen (VerificationKeyFile vkFp) (SigningKeyFile skFp) = do
    skey <- liftIO $ generateSigningKey AsStakeKey
    let vkey = getVerificationKey skey
    firstExceptT SophieStakeAddressCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope skFp (Just skeyDesc) skey
    firstExceptT SophieStakeAddressCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope vkFp (Just vkeyDesc) vkey
  where
    skeyDesc, vkeyDesc :: TextEnvelopeDescr
    skeyDesc = "Stake Signing Key"
    vkeyDesc = "Stake Verification Key"

runStakeAddressKeyHash
  :: VerificationKeyOrFile StakeKey
  -> Maybe OutputFile
  -> ExceptT SophieStakeAddressCmdError IO ()
runStakeAddressKeyHash stakeVerKeyOrFile mOutputFp = do
  vkey <- firstExceptT SophieStakeAddressCmdReadKeyFileError
    . newExceptT
    $ readVerificationKeyOrFile AsStakeKey stakeVerKeyOrFile

  let hexKeyHash = serialiseToRawBytesHex (verificationKeyHash vkey)

  case mOutputFp of
    Just (OutputFile fpath) -> liftIO $ BS.writeFile fpath hexKeyHash
    Nothing -> liftIO $ BS.putStrLn hexKeyHash

runStakeAddressBuild :: VerificationKeyOrFile StakeKey -> NetworkId -> Maybe OutputFile
                     -> ExceptT SophieStakeAddressCmdError IO ()
runStakeAddressBuild stakeVerKeyOrFile network mOutputFp = do
    stakeVerKey <- firstExceptT SophieStakeAddressCmdReadKeyFileError
      . newExceptT
      $ readVerificationKeyOrFile AsStakeKey stakeVerKeyOrFile

    let stakeCred = StakeCredentialByKey (verificationKeyHash stakeVerKey)
        stakeAddr = makeStakeAddress network stakeCred
        stakeAddrText = serialiseAddress stakeAddr

    case mOutputFp of
      Just (OutputFile fpath) -> liftIO $ Text.writeFile fpath stakeAddrText
      Nothing -> liftIO $ Text.putStrLn stakeAddrText


runStakeCredentialRegistrationCert
  :: StakeVerifier
  -> OutputFile
  -> ExceptT SophieStakeAddressCmdError IO ()
runStakeCredentialRegistrationCert stakeVerifier (OutputFile oFp) =
  case stakeVerifier of
    StakeVerifierScriptFile (ScriptFile sFile) -> do
      ScriptInAnyLang _ script <- firstExceptT SophieStakeAddressCmdReadScriptFileError
                                    $ readFileScriptInAnyLang sFile
      let stakeCred = StakeCredentialByScript $ hashScript script
      writeRegistrationCert stakeCred
    StakeVerifierKey stakeVerKeyOrFile -> do
      stakeVerKey <- firstExceptT SophieStakeAddressCmdReadKeyFileError
        . newExceptT
        $ readVerificationKeyOrFile AsStakeKey stakeVerKeyOrFile
      let stakeCred = StakeCredentialByKey (verificationKeyHash stakeVerKey)
      writeRegistrationCert stakeCred
 where
  writeRegistrationCert
    :: StakeCredential
    -> ExceptT SophieStakeAddressCmdError IO ()
  writeRegistrationCert sCred = do
    let deRegCert = makeStakeAddressRegistrationCertificate sCred
    firstExceptT SophieStakeAddressCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope oFp (Just regCertDesc) deRegCert

  regCertDesc :: TextEnvelopeDescr
  regCertDesc = "Stake Address Registration Certificate"


runStakeCredentialDelegationCert
  :: StakeVerifier
  -- ^ Delegator stake verification key, verification key file or script file.
  -> VerificationKeyOrHashOrFile StakePoolKey
  -- ^ Delegatee stake pool verification key or verification key file or
  -- verification key hash.
  -> OutputFile
  -> ExceptT SophieStakeAddressCmdError IO ()
runStakeCredentialDelegationCert stakeVerifier poolVKeyOrHashOrFile (OutputFile outFp) = do
  poolStakeVKeyHash <-
    firstExceptT
      SophieStakeAddressCmdReadKeyFileError
      (newExceptT $ readVerificationKeyOrHashOrFile AsStakePoolKey poolVKeyOrHashOrFile)

  case stakeVerifier of
    StakeVerifierScriptFile (ScriptFile sFile) -> do
      ScriptInAnyLang _ script <- firstExceptT SophieStakeAddressCmdReadScriptFileError
                                    $ readFileScriptInAnyLang sFile
      let stakeCred = StakeCredentialByScript $ hashScript script
      writeDelegationCert stakeCred poolStakeVKeyHash
    StakeVerifierKey stakeVerKeyOrFile -> do
      stakeVkey <- firstExceptT SophieStakeAddressCmdReadKeyFileError
        . newExceptT
        $ readVerificationKeyOrFile AsStakeKey stakeVerKeyOrFile
      let stakeCred = StakeCredentialByKey (verificationKeyHash stakeVkey)
      writeDelegationCert stakeCred poolStakeVKeyHash
  where
    writeDelegationCert
      :: StakeCredential
      -> Hash StakePoolKey
      -> ExceptT SophieStakeAddressCmdError IO ()
    writeDelegationCert sCred poolStakeVKeyHash = do
      let delegCert = makeStakeAddressDelegationCertificate sCred poolStakeVKeyHash
      firstExceptT SophieStakeAddressCmdWriteFileError
        . newExceptT
        $ writeFileTextEnvelope outFp (Just delegCertDesc) delegCert

    delegCertDesc :: TextEnvelopeDescr
    delegCertDesc = "Stake Address Delegation Certificate"


runStakeCredentialDeRegistrationCert
  :: StakeVerifier
  -> OutputFile
  -> ExceptT SophieStakeAddressCmdError IO ()
runStakeCredentialDeRegistrationCert stakeVerifier (OutputFile oFp) =
  case stakeVerifier of
    StakeVerifierScriptFile (ScriptFile sFile) -> do
      ScriptInAnyLang _ script <- firstExceptT SophieStakeAddressCmdReadScriptFileError
                                    $ readFileScriptInAnyLang sFile
      let stakeCred = StakeCredentialByScript $ hashScript script
      writeDeregistrationCert stakeCred
    StakeVerifierKey stakeVerKeyOrFile -> do
      stakeVkey <- firstExceptT SophieStakeAddressCmdReadKeyFileError
        . newExceptT
        $ readVerificationKeyOrFile AsStakeKey stakeVerKeyOrFile
      let stakeCred = StakeCredentialByKey (verificationKeyHash stakeVkey)
      writeDeregistrationCert stakeCred
  where
    writeDeregistrationCert
      :: StakeCredential
      -> ExceptT SophieStakeAddressCmdError IO ()
    writeDeregistrationCert sCred = do
      let deRegCert = makeStakeAddressDeregistrationCertificate sCred
      firstExceptT SophieStakeAddressCmdWriteFileError
        . newExceptT
        $ writeFileTextEnvelope oFp (Just deregCertDesc) deRegCert

    deregCertDesc :: TextEnvelopeDescr
    deregCertDesc = "Stake Address Deregistration Certificate"
