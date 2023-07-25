module Bcc.CLI.Sophie.Run.Node
  ( SophieNodeCmdError(SophieNodeCmdReadFileError)
  , renderSophieNodeCmdError
  , runNodeCmd
  , runNodeIssueOpCert
  , runNodeKeyGenCold
  , runNodeKeyGenKES
  , runNodeKeyGenVRF
  ) where

import           Bcc.Prelude hiding ((<.>))
import           Prelude (id)

import qualified Data.ByteString.Char8 as BS
import           Data.String (fromString)
import qualified Data.Text as Text

import           Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither, newExceptT)

import           Bcc.Api
import           Bcc.Api.Sophie

import           Bcc.CLI.Sophie.Commands
import           Bcc.CLI.Sophie.Key (InputDecodeError, VerificationKeyOrFile,
                     readSigningKeyFileAnyOf, readVerificationKeyOrFile)
import           Bcc.CLI.Types (SigningKeyFile (..), VerificationKeyFile (..))

{- HLINT ignore "Reduce duplication" -}

data SophieNodeCmdError
  = SophieNodeCmdReadFileError !(FileError TextEnvelopeError)
  | SophieNodeCmdReadKeyFileError !(FileError InputDecodeError)
  | SophieNodeCmdWriteFileError !(FileError ())
  | SophieNodeCmdOperationalCertificateIssueError !OperationalCertIssueError
  | SophieNodeCmdVrfSigningKeyCreationError
      FilePath
      -- ^ Target path
      FilePath
      -- ^ Temp path
  deriving Show

renderSophieNodeCmdError :: SophieNodeCmdError -> Text
renderSophieNodeCmdError err =
  case err of
    SophieNodeCmdVrfSigningKeyCreationError targetPath tempPath ->
      Text.pack $ "Error creating VRF signing key file. Target path: " <> targetPath
      <> " Temporary path: " <> tempPath

    SophieNodeCmdReadFileError fileErr -> Text.pack (displayError fileErr)

    SophieNodeCmdReadKeyFileError fileErr -> Text.pack (displayError fileErr)

    SophieNodeCmdWriteFileError fileErr -> Text.pack (displayError fileErr)

    SophieNodeCmdOperationalCertificateIssueError issueErr ->
      Text.pack (displayError issueErr)


runNodeCmd :: NodeCmd -> ExceptT SophieNodeCmdError IO ()
runNodeCmd (NodeKeyGenCold vk sk ctr) = runNodeKeyGenCold vk sk ctr
runNodeCmd (NodeKeyGenKES  vk sk)     = runNodeKeyGenKES  vk sk
runNodeCmd (NodeKeyGenVRF  vk sk)     = runNodeKeyGenVRF  vk sk
runNodeCmd (NodeKeyHashVRF vk mOutFp) = runNodeKeyHashVRF vk mOutFp
runNodeCmd (NodeNewCounter vk ctr out) = runNodeNewCounter vk ctr out
runNodeCmd (NodeIssueOpCert vk sk ctr p out) =
  runNodeIssueOpCert vk sk ctr p out



--
-- Node command implementations
--

runNodeKeyGenCold :: VerificationKeyFile
                  -> SigningKeyFile
                  -> OpCertCounterFile
                  -> ExceptT SophieNodeCmdError IO ()
runNodeKeyGenCold (VerificationKeyFile vkeyPath) (SigningKeyFile skeyPath)
                  (OpCertCounterFile ocertCtrPath) = do
    skey <- liftIO $ generateSigningKey AsStakePoolKey
    let vkey = getVerificationKey skey
    firstExceptT SophieNodeCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope skeyPath (Just skeyDesc) skey
    firstExceptT SophieNodeCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope vkeyPath (Just vkeyDesc) vkey
    firstExceptT SophieNodeCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope ocertCtrPath (Just ocertCtrDesc)
      $ OperationalCertificateIssueCounter initialCounter vkey
  where
    skeyDesc, vkeyDesc, ocertCtrDesc :: TextEnvelopeDescr
    skeyDesc = "Stake Pool Operator Signing Key"
    vkeyDesc = "Stake Pool Operator Verification Key"
    ocertCtrDesc = "Next certificate issue number: "
                <> fromString (show initialCounter)

    initialCounter :: Word64
    initialCounter = 0


runNodeKeyGenKES :: VerificationKeyFile
                 -> SigningKeyFile
                 -> ExceptT SophieNodeCmdError IO ()
runNodeKeyGenKES (VerificationKeyFile vkeyPath) (SigningKeyFile skeyPath) = do
    skey <- liftIO $ generateSigningKey AsKesKey
    let vkey = getVerificationKey skey
    firstExceptT SophieNodeCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope skeyPath (Just skeyDesc) skey
    firstExceptT SophieNodeCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope vkeyPath (Just vkeyDesc) vkey
  where
    skeyDesc, vkeyDesc :: TextEnvelopeDescr
    skeyDesc = "KES Signing Key"
    vkeyDesc = "KES Verification Key"

runNodeKeyGenVRF :: VerificationKeyFile -> SigningKeyFile
                 -> ExceptT SophieNodeCmdError IO ()
runNodeKeyGenVRF (VerificationKeyFile vkeyPath) (SigningKeyFile skeyPath) = do
    skey <- liftIO $ generateSigningKey AsVrfKey
    let vkey = getVerificationKey skey
    firstExceptT SophieNodeCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelopeWithOwnerPermissions skeyPath (Just skeyDesc) skey
    firstExceptT SophieNodeCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope vkeyPath (Just vkeyDesc) vkey
  where
    skeyDesc, vkeyDesc :: TextEnvelopeDescr
    skeyDesc = "VRF Signing Key"
    vkeyDesc = "VRF Verification Key"

runNodeKeyHashVRF :: VerificationKeyOrFile VrfKey
                  -> Maybe OutputFile
                  -> ExceptT SophieNodeCmdError IO ()
runNodeKeyHashVRF verKeyOrFile mOutputFp = do
  vkey <- firstExceptT SophieNodeCmdReadKeyFileError
    . newExceptT
    $ readVerificationKeyOrFile AsVrfKey verKeyOrFile

  let hexKeyHash = serialiseToRawBytesHex (verificationKeyHash vkey)

  case mOutputFp of
    Just (OutputFile fpath) -> liftIO $ BS.writeFile fpath hexKeyHash
    Nothing -> liftIO $ BS.putStrLn hexKeyHash


runNodeNewCounter :: ColdVerificationKeyOrFile
                  -> Word
                  -> OpCertCounterFile
                  -> ExceptT SophieNodeCmdError IO ()
runNodeNewCounter coldVerKeyOrFile counter
                  (OpCertCounterFile ocertCtrPath) = do

    vkey <- firstExceptT SophieNodeCmdReadFileError . newExceptT $
      readColdVerificationKeyOrFile coldVerKeyOrFile

    let ocertIssueCounter =
          OperationalCertificateIssueCounter (fromIntegral counter) vkey

    firstExceptT SophieNodeCmdWriteFileError . newExceptT $
      writeFileTextEnvelope ocertCtrPath Nothing ocertIssueCounter


runNodeIssueOpCert :: VerificationKeyOrFile KesKey
                   -- ^ This is the hot KES verification key.
                   -> SigningKeyFile
                   -- ^ This is the cold signing key.
                   -> OpCertCounterFile
                   -- ^ Counter that establishes the precedence
                   -- of the operational certificate.
                   -> KESPeriod
                   -- ^ Start of the validity period for this certificate.
                   -> OutputFile
                   -> ExceptT SophieNodeCmdError IO ()
runNodeIssueOpCert kesVerKeyOrFile
                   stakePoolSKeyFile
                   (OpCertCounterFile ocertCtrPath)
                   kesPeriod
                   (OutputFile certFile) = do

    ocertIssueCounter <- firstExceptT SophieNodeCmdReadFileError
      . newExceptT
      $ readFileTextEnvelope AsOperationalCertificateIssueCounter ocertCtrPath

    verKeyKes <- firstExceptT SophieNodeCmdReadKeyFileError
      . newExceptT
      $ readVerificationKeyOrFile AsKesKey kesVerKeyOrFile

    signKey <- firstExceptT SophieNodeCmdReadKeyFileError
      . newExceptT
      $ readSigningKeyFileAnyOf
          bech32PossibleBlockIssuers
          textEnvPossibleBlockIssuers
          stakePoolSKeyFile

    (ocert, nextOcertCtr) <-
      firstExceptT SophieNodeCmdOperationalCertificateIssueError
        . hoistEither
        $ issueOperationalCertificate
            verKeyKes
            signKey
            kesPeriod
            ocertIssueCounter

    -- Write the counter first, to reduce the chance of ending up with
    -- a new cert but without updating the counter.
    firstExceptT SophieNodeCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope
        ocertCtrPath
        (Just $ ocertCtrDesc $ getCounter nextOcertCtr)
        nextOcertCtr

    firstExceptT SophieNodeCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope certFile Nothing ocert
  where
    getCounter :: OperationalCertificateIssueCounter -> Word64
    getCounter (OperationalCertificateIssueCounter n _) = n

    ocertCtrDesc :: Word64 -> TextEnvelopeDescr
    ocertCtrDesc n = "Next certificate issue number: " <> fromString (show n)

    textEnvPossibleBlockIssuers
      :: [FromSomeType HasTextEnvelope
                       (Either (SigningKey StakePoolKey)
                               (SigningKey GenesisDelegateExtendedKey))]
    textEnvPossibleBlockIssuers =
      [ FromSomeType (AsSigningKey AsStakePoolKey)        Left
      , FromSomeType (AsSigningKey AsGenesisDelegateKey) (Left . castSigningKey)
      , FromSomeType (AsSigningKey AsGenesisDelegateExtendedKey) Right
      ]
    bech32PossibleBlockIssuers
      :: [FromSomeType SerialiseAsBech32
                       (Either (SigningKey StakePoolKey)
                               (SigningKey GenesisDelegateExtendedKey))]
    bech32PossibleBlockIssuers =
      [FromSomeType (AsSigningKey AsStakePoolKey) Left]

-- | Read a cold verification key or file.
--
-- If a filepath is provided, it will be interpreted as a text envelope
-- formatted file.
readColdVerificationKeyOrFile
  :: ColdVerificationKeyOrFile
  -> IO (Either (FileError TextEnvelopeError) (VerificationKey StakePoolKey))
readColdVerificationKeyOrFile coldVerKeyOrFile =
  case coldVerKeyOrFile of
    ColdStakePoolVerificationKey vk -> pure (Right vk)
    ColdGenesisDelegateVerificationKey vk -> pure $ Right (castVerificationKey vk)
    ColdGenesisVestedDelegateVerificationKey vk -> pure $ Right (castVerificationKey vk)
    ColdVestedDelegateVerificationKey vk -> pure $ Right (castVerificationKey vk)
    ColdVerificationKeyFile (VerificationKeyFile fp) ->
      readFileTextEnvelopeAnyOf
        [ FromSomeType (AsVerificationKey AsStakePoolKey) id
        , FromSomeType (AsVerificationKey AsGenesisDelegateKey) castVerificationKey
        , FromSomeType (AsVerificationKey AsGenesisVestedDelegateKey) castVerificationKey
        , FromSomeType (AsVerificationKey AsVestedDelegateKey) castVerificationKey
        ]
        fp
        