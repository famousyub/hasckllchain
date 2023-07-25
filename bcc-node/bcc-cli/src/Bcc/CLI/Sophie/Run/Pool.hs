module Bcc.CLI.Sophie.Run.Pool
  ( SophiePoolCmdError(SophiePoolCmdReadFileError)
  , renderSophiePoolCmdError
  , runPoolCmd
  ) where

import           Bcc.Prelude

import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither,
                     newExceptT)

import qualified Data.ByteString.Char8 as BS

import           Bcc.Api
import           Bcc.Api.Sophie
import           Bcc.CLI.Sophie.Commands
import           Bcc.CLI.Sophie.Key (InputDecodeError, VerificationKeyOrFile,
                     readVerificationKeyOrFile)
import           Bcc.CLI.Types (OutputFormat (..))

import qualified Bcc.Ledger.Slot as Sophie

data SophiePoolCmdError
  = SophiePoolCmdReadFileError !(FileError TextEnvelopeError)
  | SophiePoolCmdReadKeyFileError !(FileError InputDecodeError)
  | SophiePoolCmdWriteFileError !(FileError ())
  | SophiePoolCmdMetadataValidationError !StakePoolMetadataValidationError
  deriving Show

renderSophiePoolCmdError :: SophiePoolCmdError -> Text
renderSophiePoolCmdError err =
  case err of
    SophiePoolCmdReadFileError fileErr -> Text.pack (displayError fileErr)
    SophiePoolCmdReadKeyFileError fileErr -> Text.pack (displayError fileErr)
    SophiePoolCmdWriteFileError fileErr -> Text.pack (displayError fileErr)
    SophiePoolCmdMetadataValidationError validationErr ->
      "Error validating stake pool metadata: " <> Text.pack (displayError validationErr)



runPoolCmd :: PoolCmd -> ExceptT SophiePoolCmdError IO ()
runPoolCmd (PoolRegistrationCert sPvkey vrfVkey pldg pCost pMrgn rwdVerFp ownerVerFps relays mbMetadata network outfp) =
  runStakePoolRegistrationCert sPvkey vrfVkey pldg pCost pMrgn rwdVerFp ownerVerFps relays mbMetadata network outfp
runPoolCmd (PoolRetirementCert sPvkeyFp retireEpoch outfp) =
  runStakePoolRetirementCert sPvkeyFp retireEpoch outfp
runPoolCmd (PoolGetId sPvkey outputFormat) = runPoolId sPvkey outputFormat
runPoolCmd (PoolMetadataHash poolMdFile mOutFile) = runPoolMetadataHash poolMdFile mOutFile


--
-- Stake pool command implementations
--

-- | Create a stake pool registration cert.
-- TODO: Metadata and more stake pool relay support to be
-- added in the future.
runStakePoolRegistrationCert
  :: VerificationKeyOrFile StakePoolKey
  -- ^ Stake pool verification key.
  -> VerificationKeyOrFile VrfKey
  -- ^ VRF Verification key.
  -> Entropic
  -- ^ Pool pledge.
  -> Entropic
  -- ^ Pool cost.
  -> Rational
  -- ^ Pool margin.
  -> VerificationKeyOrFile StakeKey
  -- ^ Stake verification key for reward account.
  -> [VerificationKeyOrFile StakeKey]
  -- ^ Pool owner stake verification key(s).
  -> [StakePoolRelay]
  -- ^ Stake pool relays.
  -> Maybe StakePoolMetadataReference
  -- ^ Stake pool metadata.
  -> NetworkId
  -> OutputFile
  -> ExceptT SophiePoolCmdError IO ()
runStakePoolRegistrationCert
  stakePoolVerKeyOrFile
  vrfVerKeyOrFile
  pldg
  pCost
  pMrgn
  rwdStakeVerKeyOrFile
  ownerStakeVerKeyOrFiles
  relays
  mbMetadata
  network
  (OutputFile outfp) = do
    -- Pool verification key
    stakePoolVerKey <- firstExceptT SophiePoolCmdReadKeyFileError
      . newExceptT
      $ readVerificationKeyOrFile AsStakePoolKey stakePoolVerKeyOrFile
    let stakePoolId' = verificationKeyHash stakePoolVerKey

    -- VRF verification key
    vrfVerKey <- firstExceptT SophiePoolCmdReadKeyFileError
      . newExceptT
      $ readVerificationKeyOrFile AsVrfKey vrfVerKeyOrFile
    let vrfKeyHash' = verificationKeyHash vrfVerKey

    -- Pool reward account
    rwdStakeVerKey <- firstExceptT SophiePoolCmdReadKeyFileError
      . newExceptT
      $ readVerificationKeyOrFile AsStakeKey rwdStakeVerKeyOrFile
    let stakeCred = StakeCredentialByKey (verificationKeyHash rwdStakeVerKey)
        rewardAccountAddr = makeStakeAddress network stakeCred

    -- Pool owner(s)
    sPoolOwnerVkeys <-
      mapM
        (firstExceptT SophiePoolCmdReadKeyFileError
          . newExceptT
          . readVerificationKeyOrFile AsStakeKey
        )
        ownerStakeVerKeyOrFiles
    let stakePoolOwners' = map verificationKeyHash sPoolOwnerVkeys

    let stakePoolParams =
          StakePoolParameters
            { stakePoolId = stakePoolId'
            , stakePoolVRF = vrfKeyHash'
            , stakePoolCost = pCost
            , stakePoolMargin = pMrgn
            , stakePoolRewardAccount = rewardAccountAddr
            , stakePoolPledge = pldg
            , stakePoolOwners = stakePoolOwners'
            , stakePoolRelays = relays
            , stakePoolMetadata = mbMetadata
            }

    let registrationCert = makeStakePoolRegistrationCertificate stakePoolParams

    firstExceptT SophiePoolCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope outfp (Just registrationCertDesc) registrationCert
  where
    registrationCertDesc :: TextEnvelopeDescr
    registrationCertDesc = "Stake Pool Registration Certificate"

runStakePoolRetirementCert
  :: VerificationKeyOrFile StakePoolKey
  -> Sophie.EpochNo
  -> OutputFile
  -> ExceptT SophiePoolCmdError IO ()
runStakePoolRetirementCert stakePoolVerKeyOrFile retireEpoch (OutputFile outfp) = do
    -- Pool verification key
    stakePoolVerKey <- firstExceptT SophiePoolCmdReadKeyFileError
      . newExceptT
      $ readVerificationKeyOrFile AsStakePoolKey stakePoolVerKeyOrFile

    let stakePoolId' = verificationKeyHash stakePoolVerKey
        retireCert = makeStakePoolRetirementCertificate stakePoolId' retireEpoch

    firstExceptT SophiePoolCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope outfp (Just retireCertDesc) retireCert
  where
    retireCertDesc :: TextEnvelopeDescr
    retireCertDesc = "Stake Pool Retirement Certificate"

runPoolId
  :: VerificationKeyOrFile StakePoolKey
  -> OutputFormat
  -> ExceptT SophiePoolCmdError IO ()
runPoolId verKeyOrFile outputFormat = do
    stakePoolVerKey <- firstExceptT SophiePoolCmdReadKeyFileError
      . newExceptT
      $ readVerificationKeyOrFile AsStakePoolKey verKeyOrFile
    liftIO $
      case outputFormat of
        OutputFormatHex ->
          BS.putStrLn $ serialiseToRawBytesHex (verificationKeyHash stakePoolVerKey)
        OutputFormatBech32 ->
          Text.putStrLn $ serialiseToBech32 (verificationKeyHash stakePoolVerKey)

runPoolMetadataHash :: PoolMetadataFile -> Maybe OutputFile -> ExceptT SophiePoolCmdError IO ()
runPoolMetadataHash (PoolMetadataFile poolMDPath) mOutFile = do
  metadataBytes <- handleIOExceptT (SophiePoolCmdReadFileError . FileIOError poolMDPath) $
    BS.readFile poolMDPath
  (_metadata, metadataHash) <-
      firstExceptT SophiePoolCmdMetadataValidationError
    . hoistEither
    $ validateAndHashStakePoolMetadata metadataBytes
  case mOutFile of
    Nothing -> liftIO $ BS.putStrLn (serialiseToRawBytesHex metadataHash)
    Just (OutputFile fpath) ->
      handleIOExceptT (SophiePoolCmdWriteFileError . FileIOError fpath)
        $ BS.writeFile fpath (serialiseToRawBytesHex metadataHash)
