{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bcc.CLI.Cole.Delegation
  ( ColeDelegationError(..)
  , checkColeGenesisDelegation
  , issueColeGenesisDelegation
  , renderColeDelegationError
  , serialiseDelegationCert
  , serialiseColeWitness
  )
where

import           Bcc.Prelude hiding (option, show, trace)

import           Control.Monad.Trans.Except.Extra (left)
import qualified Data.ByteString.Lazy as LB
import           Formatting (Format, sformat)

import           Bcc.Api.Cole

import           Bcc.Binary (Annotated (..), serialize')
import qualified Bcc.Chain.Delegation as Dlg
import           Bcc.Chain.Slotting (EpochNumber)
import           Bcc.Crypto (ProtocolMagicId)
import qualified Bcc.Crypto as Crypto

import           Bcc.CLI.Cole.Key (ColeKeyFailure, renderColeKeyFailure)
import           Bcc.CLI.Helpers (textShow)
import           Bcc.CLI.Types (CertificateFile (..))

data ColeDelegationError
  = CertificateValidationErrors !FilePath ![Text]
  | DlgCertificateDeserialisationFailed !FilePath !Text
  | ColeDelegationKeyError !ColeKeyFailure
  deriving Show

renderColeDelegationError :: ColeDelegationError -> Text
renderColeDelegationError err =
  case err of
    CertificateValidationErrors certFp errs ->
      "Certificate validation error(s) at: " <> textShow certFp <> " Errors: " <> textShow errs
    DlgCertificateDeserialisationFailed certFp deSererr ->
      "Certificate deserialisation error at: " <> textShow certFp <> " Error: " <> textShow deSererr
    ColeDelegationKeyError kerr -> renderColeKeyFailure kerr

-- TODO:  we need to support password-protected secrets.
-- | Issue a certificate for genesis delegation to a delegate key, signed by the
--   issuer key, for a given protocol magic and coming into effect at given epoch.
issueColeGenesisDelegation
  :: ProtocolMagicId
  -> EpochNumber
  -> Crypto.SigningKey
  -> Crypto.VerificationKey
  -> Dlg.Certificate
issueColeGenesisDelegation magic epoch issuerSK delegateVK =
  Dlg.signCertificate magic delegateVK epoch $
  Crypto.noPassSafeSigner issuerSK

-- | Verify that a certificate signifies genesis delegation by assumed genesis key
--   to a delegate key, for a given protocol magic.
--   If certificate fails validation, throw an error.
checkColeGenesisDelegation
  :: CertificateFile
  -> ProtocolMagicId
  -> Crypto.VerificationKey
  -> Crypto.VerificationKey
  -> ExceptT ColeDelegationError IO ()
checkColeGenesisDelegation (CertificateFile certF) magic issuer delegate = do
  ecert <- liftIO $ canonicalDecodePretty <$> LB.readFile certF
  case ecert of
    Left e -> left $ DlgCertificateDeserialisationFailed certF e
    Right (cert :: Dlg.Certificate) -> do
      let issues = checkDlgCert cert magic issuer delegate
      unless (null issues) $
        left $ CertificateValidationErrors certF issues

checkDlgCert
  :: Dlg.ACertificate a
  -> ProtocolMagicId
  -> Crypto.VerificationKey
  -> Crypto.VerificationKey -> [Text]
checkDlgCert cert magic issuerVK' delegateVK' =
  mconcat
  [ [ sformat "Certificate does not have a valid signature."
      | not (Dlg.isValid magic' cert')
    ]
  , [ sformat ("Certificate issuer ".vkF." doesn't match expected: ".vkF)
      ( Dlg.issuerVK cert) issuerVK'
      | Dlg.issuerVK cert /= issuerVK'
    ]
  , [ sformat ("Certificate delegate ".vkF." doesn't match expected: ".vkF)
      ( Dlg.delegateVK cert) delegateVK'
      | Dlg.delegateVK cert /= delegateVK'
    ]
  ]
  where
    magic' :: Annotated ProtocolMagicId ByteString
    magic' = Annotated magic (serialize' magic)

    epoch :: EpochNumber
    epoch = unAnnotated $ Dlg.aEpoch cert

    cert' :: Dlg.ACertificate ByteString
    cert' =
      let unannotated = cert { Dlg.aEpoch = Annotated epoch ()
                             , Dlg.annotation = () }
      in unannotated { Dlg.annotation = serialize' unannotated
                     , Dlg.aEpoch = Annotated epoch (serialize' epoch) }

    vkF :: forall r. Format r (Crypto.VerificationKey -> r)
    vkF = Crypto.fullVerificationKeyF


serialiseDelegationCert :: Dlg.Certificate -> ByteString
serialiseDelegationCert = LB.toStrict . canonicalEncodePretty

serialiseColeWitness :: SomeColeSigningKey -> ByteString
serialiseColeWitness sk =
  case sk of
    AColeSigningKeyLegacy bSkey -> serialiseToRawBytes bSkey
    AColeSigningKey legBKey -> serialiseToRawBytes legBKey

