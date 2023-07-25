{-# LANGUAGE NamedFieldPuns #-}

module Bcc.Node.Protocol.Cole
  ( mkSomeConsensusProtocolCole
    -- * Errors
  , ColeProtocolInstantiationError(..)

    -- * Reusable parts
  , readGenesis
  , readLeaderCredentials
  ) where


import           Bcc.Prelude
import           Control.Monad.Trans.Except.Extra (bimapExceptT, firstExceptT, hoistEither,
                   hoistMaybe, left)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as Text

import           Bcc.Api.Cole
import qualified Bcc.Api.Protocol.Types as Protocol

import qualified Bcc.Crypto.Hash as Crypto

import qualified Bcc.Crypto.Hashing as Cole.Crypto

import qualified Bcc.Chain.Genesis as Genesis
import qualified Bcc.Chain.UTxO as UTxO
import qualified Bcc.Chain.Update as Update
import           Bcc.Crypto.ProtocolMagic (RequiresNetworkMagic)

import           Shardagnostic.Consensus.Bcc
import qualified Shardagnostic.Consensus.Bcc as Consensus
import qualified Shardagnostic.Consensus.Mempool.TxLimits as TxLimits

import           Bcc.Node.Types

import           Bcc.Node.Protocol.Types
import           Bcc.Tracing.OrphanInstances.Cole ()
import           Bcc.Tracing.OrphanInstances.HardFork ()
import           Bcc.Tracing.OrphanInstances.Sophie ()


------------------------------------------------------------------------------
-- Cole protocol
--

-- | Make 'SomeConsensusProtocol' using the Cole instance.
--
-- This lets us handle multiple protocols in a generic way.
--
-- This also serves a purpose as a sanity check that we have all the necessary
-- type class instances available.
--
mkSomeConsensusProtocolCole
  :: NodeColeProtocolConfiguration
  -> Maybe ProtocolFilepaths
  -> ExceptT ColeProtocolInstantiationError IO SomeConsensusProtocol
mkSomeConsensusProtocolCole NodeColeProtocolConfiguration {
                           npcColeGenesisFile,
                           npcColeGenesisFileHash,
                           npcColeReqNetworkMagic,
                           npcColePbftSignatureThresh,
                           npcColeApplicationName,
                           npcColeApplicationVersion,
                           npcColeSupportedProtocolVersionMajor,
                           npcColeSupportedProtocolVersionSentry
                         }
                         files = do
    genesisConfig <- readGenesis npcColeGenesisFile
                                 npcColeGenesisFileHash
                                 npcColeReqNetworkMagic

    optionalLeaderCredentials <- readLeaderCredentials genesisConfig files

    return $ SomeConsensusProtocol Protocol.ColeBlockType $ Protocol.ProtocolInfoArgsCole $ Consensus.ProtocolParamsCole {
        coleGenesis = genesisConfig,
        colePbftSignatureThreshold =
          PBftSignatureThreshold <$> npcColePbftSignatureThresh,
        coleProtocolVersion =
          Update.ProtocolVersion
            npcColeSupportedProtocolVersionMajor
            npcColeSupportedProtocolVersionSentry,
        coleSoftwareVersion =
          Update.SoftwareVersion
            npcColeApplicationName
            npcColeApplicationVersion,
        coleLeaderCredentials =
          optionalLeaderCredentials,
        coleMaxTxCapacityOverrides =
          TxLimits.mkOverrides TxLimits.noOverridesMeasure
        }

readGenesis :: GenesisFile
            -> Maybe GenesisHash
            -> RequiresNetworkMagic
            -> ExceptT ColeProtocolInstantiationError IO
                       Genesis.Config
readGenesis (GenesisFile file) mbExpectedGenesisHash ncReqNetworkMagic = do
    (genesisData, genesisHash) <- firstExceptT (GenesisReadError file) $
                                  Genesis.readGenesisData file
    checkExpectedGenesisHash genesisHash
    return Genesis.Config {
      Genesis.configGenesisData       = genesisData,
      Genesis.configGenesisHash       = genesisHash,
      Genesis.configReqNetMagic       = ncReqNetworkMagic,
      Genesis.configUTxOConfiguration = UTxO.defaultUTxOConfiguration
      --TODO: add config support for the UTxOConfiguration if needed
    }
  where
    checkExpectedGenesisHash :: Genesis.GenesisHash
                             -> ExceptT ColeProtocolInstantiationError IO ()
    checkExpectedGenesisHash actual' =
      case mbExpectedGenesisHash of
        Just expected | actual /= expected ->
            throwError (GenesisHashMismatch actual expected)
          where
            actual = fromColeGenesisHash actual'

        _ -> return ()

    fromColeGenesisHash :: Genesis.GenesisHash -> GenesisHash
    fromColeGenesisHash (Genesis.GenesisHash h) =
        GenesisHash
      . fromMaybe impossible
      . Crypto.hashFromBytes
      . Cole.Crypto.hashToBytes
      $ h
      where
        impossible =
          panic "fromColeGenesisHash: old and new crypto libs disagree on hash size"



readLeaderCredentials :: Genesis.Config
                      -> Maybe ProtocolFilepaths
                      -> ExceptT ColeProtocolInstantiationError IO
                                 (Maybe ColeLeaderCredentials)
readLeaderCredentials _ Nothing = return Nothing
readLeaderCredentials genesisConfig
                      (Just ProtocolFilepaths {
                        coleCertFile,
                        coleKeyFile
                      }) =
  case (coleCertFile, coleKeyFile) of
    (Nothing, Nothing) -> pure Nothing
    (Just _, Nothing) -> left SigningKeyFilepathNotSpecified
    (Nothing, Just _) -> left DelegationCertificateFilepathNotSpecified
    (Just delegCertFile, Just signingKeyFile) -> do

         signingKeyFileBytes <- liftIO $ LB.readFile signingKeyFile
         delegCertFileBytes <- liftIO $ LB.readFile delegCertFile
         ColeSigningKey signingKey <- hoistMaybe (SigningKeyDeserialiseFailure signingKeyFile)
                         $ deserialiseFromRawBytes (AsSigningKey AsColeKey) $ LB.toStrict signingKeyFileBytes
         delegCert  <- firstExceptT (CanonicalDecodeFailure delegCertFile)
                         . hoistEither
                         $ canonicalDecodePretty delegCertFileBytes

         bimapExceptT CredentialsError Just
           . hoistEither
           $ mkColeLeaderCredentials genesisConfig signingKey delegCert "Cole"



------------------------------------------------------------------------------
-- Cole Errors
--

data ColeProtocolInstantiationError =
    CanonicalDecodeFailure !FilePath !Text
  | GenesisHashMismatch !GenesisHash !GenesisHash -- actual, expected
  | DelegationCertificateFilepathNotSpecified
  | GenesisConfigurationError !FilePath !Genesis.ConfigurationError
  | GenesisReadError !FilePath !Genesis.GenesisDataError
  | CredentialsError !ColeLeaderCredentialsError
  | SigningKeyDeserialiseFailure !FilePath
  | SigningKeyFilepathNotSpecified
  deriving Show

instance Error ColeProtocolInstantiationError where
  displayError (CanonicalDecodeFailure fp failure) =
        "Canonical decode failure in " <> fp
     <> " Canonical failure: " <> Text.unpack failure
  displayError (GenesisHashMismatch actual expected) =
        "Wrong Cole genesis file: the actual hash is " <> show actual
     <> ", but the expected Cole genesis hash given in the node configuration "
     <> "file is " <> show expected
  displayError DelegationCertificateFilepathNotSpecified =
        "Delegation certificate filepath not specified"
    --TODO: Implement configuration error render function in bcc-ledger
  displayError (GenesisConfigurationError fp genesisConfigError) =
        "Genesis configuration error in: " <> toS fp
     <> " Error: " <> show genesisConfigError
  displayError (GenesisReadError fp err) =
        "There was an error parsing the genesis file: " <> toS fp
     <> " Error: " <> show err
    -- TODO: Implement ColeLeaderCredentialsError render function in shardagnostic-network
  displayError (CredentialsError coleLeaderCredentialsError) =
        "Cole leader credentials error: " <> show coleLeaderCredentialsError
  displayError (SigningKeyDeserialiseFailure fp) =
        "Signing key deserialisation error in: " <> toS fp
  displayError SigningKeyFilepathNotSpecified =
        "Signing key filepath not specified"
