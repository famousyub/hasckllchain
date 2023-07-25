{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Bcc.CLI.Cole.Key
  ( -- * Keys
    ColeKeyFailure(..)
  , NewSigningKeyFile(..)
  , NewVerificationKeyFile(..)
  , VerificationKeyFile(..)
  , prettyPublicKey
  , readColeSigningKey
  , readPaymentVerificationKey
  , renderColeKeyFailure
  , coleWitnessToVerKey
  )
where

import           Bcc.Prelude hiding (option, show, trace, (%))
import           Prelude (show)

import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither, left,
                     right)
import qualified Data.ByteString as SB
import qualified Data.ByteString.UTF8 as UTF8
import           Data.String (fromString)
import qualified Data.Text as T
import           Formatting (build, sformat, (%))

import           Bcc.Api.Cole

import qualified Bcc.Chain.Common as Common
import           Bcc.CLI.Helpers (textShow)
import           Bcc.CLI.Sophie.Commands (ColeKeyFormat (..))
import           Bcc.CLI.Types
import qualified Bcc.Crypto.Signing as Crypto


data ColeKeyFailure
  = ReadSigningKeyFailure !FilePath !Text
  | ReadVerificationKeyFailure !FilePath !Text
  | LegacySigningKeyDeserialisationFailed !FilePath
  | SigningKeyDeserialisationFailed !FilePath
  | VerificationKeyDeserialisationFailed !FilePath !Text
  | CannotMigrateFromNonLegacySigningKey !FilePath
  deriving Show

renderColeKeyFailure :: ColeKeyFailure -> Text
renderColeKeyFailure err =
  case err of
    CannotMigrateFromNonLegacySigningKey fp ->
      "Migrate from non-legacy Cole key unnecessary: " <> textShow fp
    ReadSigningKeyFailure sKeyFp readErr ->
      "Error reading signing key at: " <> textShow sKeyFp <> " Error: " <> textShow readErr
    ReadVerificationKeyFailure vKeyFp readErr ->
      "Error reading verification key at: " <> textShow vKeyFp <> " Error: " <> textShow readErr
    LegacySigningKeyDeserialisationFailed fp ->
      "Error attempting to deserialise a legacy signing key at: " <> textShow fp
    SigningKeyDeserialisationFailed sKeyFp  ->
      "Error deserialising signing key at: " <> textShow sKeyFp
    VerificationKeyDeserialisationFailed vKeyFp deSerError ->
      "Error deserialising verification key at: " <> textShow vKeyFp <> " Error: " <> textShow deSerError

newtype NewSigningKeyFile =
  NewSigningKeyFile FilePath
  deriving (Eq, Ord, Show, IsString)

newtype NewVerificationKeyFile =
  NewVerificationKeyFile FilePath
   deriving (Eq, Ord, Show, IsString)

-- | Print some invariant properties of a public key:
--   its hash and formatted view.
prettyPublicKey :: VerificationKey ColeKey-> Text
prettyPublicKey (ColeVerificationKey vk) =
  sformat (  "    public key hash: "% build %
           "\npublic key (base64): "% Crypto.fullVerificationKeyF %
           "\n   public key (hex): "% Crypto.fullVerificationKeyHexF)
    (Common.addressHash vk) vk vk

coleWitnessToVerKey :: SomeColeSigningKey -> VerificationKey ColeKey
coleWitnessToVerKey (AColeSigningKeyLegacy sKeyLeg) = castVerificationKey $ getVerificationKey sKeyLeg
coleWitnessToVerKey (AColeSigningKey sKeyNonLeg) = getVerificationKey sKeyNonLeg

-- TODO:  we need to support password-protected secrets.
-- | Read signing key from a file.
readColeSigningKey :: ColeKeyFormat -> SigningKeyFile -> ExceptT ColeKeyFailure IO SomeColeSigningKey
readColeSigningKey bKeyFormat (SigningKeyFile fp) = do
  sK <- handleIOExceptT (ReadSigningKeyFailure fp . T.pack . displayException) $ SB.readFile fp
  case bKeyFormat of
    LegacyColeKeyFormat ->
      case deserialiseFromRawBytes (AsSigningKey AsColeKeyLegacy) sK of
        Just legKey -> right $ AColeSigningKeyLegacy legKey
        Nothing -> left $ LegacySigningKeyDeserialisationFailed fp
    NonLegacyColeKeyFormat ->
      case deserialiseFromRawBytes (AsSigningKey AsColeKey) sK of
        Just nonLegSKey -> right $ AColeSigningKey nonLegSKey
        Nothing -> left $ SigningKeyDeserialisationFailed fp

-- | Read verification key from a file.  Throw an error if the file can't be read
-- or the key fails to deserialise.
readPaymentVerificationKey :: VerificationKeyFile -> ExceptT ColeKeyFailure IO Crypto.VerificationKey
readPaymentVerificationKey (VerificationKeyFile fp) = do
  vkB <- handleIOExceptT (ReadVerificationKeyFailure fp . T.pack . displayException) (SB.readFile fp)
  -- Verification Key
  let eVk = hoistEither . Crypto.parseFullVerificationKey . fromString $ UTF8.toString vkB
  -- Convert error to 'CliError'
  firstExceptT (VerificationKeyDeserialisationFailed fp . T.pack . show) eVk

