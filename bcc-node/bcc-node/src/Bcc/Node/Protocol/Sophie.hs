{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bcc.Node.Protocol.Sophie
  ( mkSomeConsensusProtocolSophie

    -- * Errors
  , SophieProtocolInstantiationError(..)
  , GenesisReadError(..)
  , GenesisValidationError(..)
  , OptimumLeaderCredentialsError(..)

    -- * Reusable parts
  , readGenesis
  , readGenesisAny
  , readLeaderCredentials
  , genesisHashToOptimumNonce
  , validateGenesis
  ) where

import           Bcc.Prelude
import           Prelude (String, id)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.Text as T

import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither,
                   newExceptT)

import qualified Bcc.Crypto.Hash.Class as Crypto
import           Bcc.Ledger.Crypto (StandardCrypto)
import           Bcc.Ledger.Keys (coerceKeyRole)

import qualified Shardagnostic.Consensus.Bcc as Consensus
import qualified Shardagnostic.Consensus.Mempool.TxLimits as TxLimits
import           Shardagnostic.Consensus.Sophie.Eras (StandardSophie)
import           Shardagnostic.Consensus.Sophie.Node (Nonce (..), ProtocolParamsSophie (..),
                   ProtocolParamsSophieBased (..), TOptimumLeaderCredentials (..))
import           Shardagnostic.Consensus.Sophie.Protocol (TOptimumCanBeLeader (..))

import qualified Sophie.Spec.Ledger.Genesis as Sophie
import           Sophie.Spec.Ledger.PParams (ProtVer (..))

import qualified Bcc.Api as Api (FileError (..))
import           Bcc.Api.Orphans ()
import qualified Bcc.Api.Protocol.Types as Protocol
import           Bcc.Api.Sophie hiding (FileError)


import           Bcc.Node.Types

import           Bcc.Tracing.OrphanInstances.HardFork ()
import           Bcc.Tracing.OrphanInstances.Sophie ()

import           Bcc.Node.Protocol.Types

------------------------------------------------------------------------------
-- Sophie protocol
--

-- | Make 'SomeConsensusProtocol' using the Sophie instance.
--
-- This lets us handle multiple protocols in a generic way.
--
-- This also serves a purpose as a sanity check that we have all the necessary
-- type class instances available.
mkSomeConsensusProtocolSophie
  :: NodeSophieProtocolConfiguration
  -> Maybe ProtocolFilepaths
  -> ExceptT SophieProtocolInstantiationError IO SomeConsensusProtocol
mkSomeConsensusProtocolSophie NodeSophieProtocolConfiguration {
                                  npcSophieGenesisFile,
                                  npcSophieGenesisFileHash
                                }
                          files = do
    (genesis, genesisHash) <- firstExceptT GenesisReadError $
                              readGenesis npcSophieGenesisFile
                                          npcSophieGenesisFileHash
    firstExceptT GenesisValidationError $ validateGenesis genesis
    leaderCredentials <- firstExceptT OptimumLeaderCredentialsError $
                         readLeaderCredentials files

    return $ SomeConsensusProtocol Protocol.SophieBlockType $ Protocol.ProtocolInfoArgsSophie
      Consensus.ProtocolParamsSophieBased {
        sophieBasedGenesis = genesis,
        sophieBasedInitialNonce = genesisHashToOptimumNonce genesisHash,
        sophieBasedLeaderCredentials =
            leaderCredentials
      }
      Consensus.ProtocolParamsSophie {
        sophieProtVer =
          ProtVer 2 0,
        sophieMaxTxCapacityOverrides =
          TxLimits.mkOverrides TxLimits.noOverridesMeasure
      }

genesisHashToOptimumNonce :: GenesisHash -> Nonce
genesisHashToOptimumNonce (GenesisHash h) = Nonce (Crypto.castHash h)

readGenesis :: GenesisFile
            -> Maybe GenesisHash
            -> ExceptT GenesisReadError IO
                       (SophieGenesis StandardSophie, GenesisHash)
readGenesis = readGenesisAny

readGenesisAny :: FromJSON genesis
               => GenesisFile
               -> Maybe GenesisHash
               -> ExceptT GenesisReadError IO (genesis, GenesisHash)
readGenesisAny (GenesisFile file) mbExpectedGenesisHash = do
    content <- handleIOExceptT (GenesisReadFileError file) $
                 BS.readFile file
    let genesisHash = GenesisHash (Crypto.hashWith id content)
    checkExpectedGenesisHash genesisHash
    genesis <- firstExceptT (GenesisDecodeError file) $ hoistEither $
                 Aeson.eitherDecodeStrict' content
    return (genesis, genesisHash)
  where
    checkExpectedGenesisHash :: GenesisHash
                             -> ExceptT GenesisReadError IO ()
    checkExpectedGenesisHash actual =
      case mbExpectedGenesisHash of
        Just expected | actual /= expected
          -> throwError (GenesisHashMismatch actual expected)
        _ -> return ()

validateGenesis :: SophieGenesis StandardSophie
                -> ExceptT GenesisValidationError IO ()
validateGenesis genesis =
    firstExceptT GenesisValidationErrors . hoistEither $
      Sophie.validateGenesis genesis

readLeaderCredentials :: Maybe ProtocolFilepaths
                      -> ExceptT OptimumLeaderCredentialsError IO
                                 [TOptimumLeaderCredentials StandardCrypto]
readLeaderCredentials Nothing = return []
readLeaderCredentials (Just pfp) =
  -- The set of credentials is a sum total of what comes from the CLI,
  -- as well as what's in the bulk credentials file.
  (<>) <$> readLeaderCredentialsSingleton pfp
       <*> readLeaderCredentialsBulk      pfp

readLeaderCredentialsSingleton ::
     ProtocolFilepaths ->
     ExceptT OptimumLeaderCredentialsError IO
             [TOptimumLeaderCredentials StandardCrypto]
-- It's OK to supply none of the files on the CLI
readLeaderCredentialsSingleton
   ProtocolFilepaths
     { sophieCertFile      = Nothing,
       sophieVRFFile       = Nothing,
       sophieKESFile       = Nothing
     } = pure []
-- Or to supply all of the files
readLeaderCredentialsSingleton
   ProtocolFilepaths
     { sophieCertFile      = Just certFile,
       sophieVRFFile       = Just vrfFile,
       sophieKESFile       = Just kesFile
     } =
     fmap (:[]) $
     mkOptimumLeaderCredentials
       <$> firstExceptT FileError (newExceptT $ readFileTextEnvelope AsOperationalCertificate certFile)
       <*> firstExceptT FileError (newExceptT $ readFileTextEnvelope (AsSigningKey AsVrfKey) vrfFile)
       <*> firstExceptT FileError (newExceptT $ readFileTextEnvelope (AsSigningKey AsKesKey) kesFile)
-- But not OK to supply some of the files without the others.
readLeaderCredentialsSingleton ProtocolFilepaths {sophieCertFile = Nothing} =
     throwError OCertNotSpecified
readLeaderCredentialsSingleton ProtocolFilepaths {sophieVRFFile = Nothing} =
     throwError VRFKeyNotSpecified
readLeaderCredentialsSingleton ProtocolFilepaths {sophieKESFile = Nothing} =
     throwError KESKeyNotSpecified

data SophieCredentials
  = SophieCredentials
    { scCert :: (TextEnvelope, FilePath)
    , scVrf  :: (TextEnvelope, FilePath)
    , scKes  :: (TextEnvelope, FilePath)
    }

readLeaderCredentialsBulk ::
     ProtocolFilepaths
  -> ExceptT OptimumLeaderCredentialsError IO
             [TOptimumLeaderCredentials StandardCrypto]
readLeaderCredentialsBulk ProtocolFilepaths { sophieBulkCredsFile = mfp } =
  mapM parseSophieCredentials =<< readBulkFile mfp
 where
   parseSophieCredentials ::
        SophieCredentials
     -> ExceptT OptimumLeaderCredentialsError IO
                (TOptimumLeaderCredentials StandardCrypto)
   parseSophieCredentials SophieCredentials { scCert, scVrf, scKes } = do
     mkOptimumLeaderCredentials
       <$> parseEnvelope AsOperationalCertificate scCert
       <*> parseEnvelope (AsSigningKey AsVrfKey) scVrf
       <*> parseEnvelope (AsSigningKey AsKesKey) scKes

   readBulkFile :: Maybe FilePath
                -> ExceptT OptimumLeaderCredentialsError IO
                           [SophieCredentials]
   readBulkFile Nothing = pure []
   readBulkFile (Just fp) = do
     content <- handleIOExceptT (CredentialsReadError fp) $
                  BS.readFile fp
     envelopes <- firstExceptT (EnvelopeParseError fp) $ hoistEither $
                    Aeson.eitherDecodeStrict' content
     pure $ uncurry mkCredentials <$> zip [0..] envelopes
    where
      mkCredentials :: Int -> (TextEnvelope, TextEnvelope, TextEnvelope)
                    -> SophieCredentials
      mkCredentials ix (teCert, teVrf, teKes) =
       let loc ty = fp <> "." <> show ix <> ty
       in SophieCredentials (teCert, loc "cert")
                             (teVrf,  loc "vrf")
                             (teKes,  loc "kes")

mkOptimumLeaderCredentials ::
     OperationalCertificate
  -> SigningKey VrfKey
  -> SigningKey KesKey
  -> TOptimumLeaderCredentials StandardCrypto
mkOptimumLeaderCredentials
    (OperationalCertificate opcert (StakePoolVerificationKey vkey))
    (VrfSigningKey vrfKey)
    (KesSigningKey kesKey) =
    TOptimumLeaderCredentials
    { toptimumLeaderCredentialsCanBeLeader =
        TOptimumCanBeLeader {
        toptimumCanBeLeaderOpCert     = opcert,
          toptimumCanBeLeaderColdVerKey = coerceKeyRole vkey,
          toptimumCanBeLeaderSignKeyVRF = vrfKey
        },
      toptimumLeaderCredentialsInitSignKey = kesKey,
      toptimumLeaderCredentialsLabel = "Sophie"
    }

parseEnvelope ::
     HasTextEnvelope a
  => AsType a
  -> (TextEnvelope, String)
  -> ExceptT OptimumLeaderCredentialsError IO a
parseEnvelope as (te, loc) =
  firstExceptT (FileError . Api.FileError loc) . hoistEither $
    deserialiseFromTextEnvelope as te


------------------------------------------------------------------------------
-- Errors
--

data SophieProtocolInstantiationError =
       GenesisReadError GenesisReadError
     | GenesisValidationError GenesisValidationError
     | OptimumLeaderCredentialsError OptimumLeaderCredentialsError
  deriving Show

instance Error SophieProtocolInstantiationError where
  displayError (GenesisReadError err) = displayError err
  displayError (GenesisValidationError err) = displayError err
  displayError (OptimumLeaderCredentialsError err) = displayError err


data GenesisReadError =
       GenesisReadFileError !FilePath !IOException
     | GenesisHashMismatch !GenesisHash !GenesisHash -- actual, expected
     | GenesisDecodeError !FilePath !String
  deriving Show

instance Error GenesisReadError where
  displayError (GenesisReadFileError fp err) =
        "There was an error reading the genesis file: "
     <> toS fp <> " Error: " <> show err

  displayError (GenesisHashMismatch actual expected) =
        "Wrong genesis file: the actual hash is " <> show actual
     <> ", but the expected genesis hash given in the node "
     <> "configuration file is " <> show expected

  displayError (GenesisDecodeError fp err) =
        "There was an error parsing the genesis file: "
     <> toS fp <> " Error: " <> show err


newtype GenesisValidationError = GenesisValidationErrors [Sophie.ValidationErr]
  deriving Show

instance Error GenesisValidationError where
  displayError (GenesisValidationErrors vErrs) =
    T.unpack (unlines (map Sophie.describeValidationErr vErrs))


data OptimumLeaderCredentialsError =
       CredentialsReadError !FilePath !IOException
     | EnvelopeParseError !FilePath !String
     | FileError !(Api.FileError TextEnvelopeError)
--TODO: pick a less generic constructor than FileError

     | OCertNotSpecified
     | VRFKeyNotSpecified
     | KESKeyNotSpecified
  deriving Show

instance Error OptimumLeaderCredentialsError where
  displayError (CredentialsReadError fp err) =
        "There was an error reading a credentials file: "
     <> toS fp <> " Error: " <> show err

  displayError (EnvelopeParseError fp err) =
        "There was an error parsing a credentials envelope: "
     <> toS fp <> " Error: " <> show err

  displayError (FileError fileErr) = displayError fileErr

  displayError OCertNotSpecified  = missingFlagMessage "sophie-operational-certificate"
  displayError VRFKeyNotSpecified = missingFlagMessage "sophie-vrf-key"
  displayError KESKeyNotSpecified = missingFlagMessage "sophie-kes-key"

missingFlagMessage :: String -> String
missingFlagMessage flag =
  "To create blocks, the --" <> flag <> " must also be specified"
