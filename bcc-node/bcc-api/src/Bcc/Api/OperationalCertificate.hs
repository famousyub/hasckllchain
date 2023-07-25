{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

-- | Operational certificates
--
module Bcc.Api.OperationalCertificate (
    OperationalCertificate(..),
    OperationalCertificateIssueCounter(..),
    Sophie.KESPeriod(..),
    OperationalCertIssueError(..),
    issueOperationalCertificate,

    -- * Data family instances
    AsType(..)
  ) where

import           Prelude

import           Data.Word

import           Bcc.Ledger.Crypto (StandardCrypto)
import qualified Bcc.Ledger.Keys as Sophie
import qualified Bcc.Ledger.Serialization as CBOR (CBORGroup (..))

import           Bcc.Api.Address
import           Bcc.Api.Certificate
import           Bcc.Api.Error
import           Bcc.Api.HasTypeProxy
import           Bcc.Api.Key
import           Bcc.Api.KeysCole
import           Bcc.Api.KeysOptimum
import           Bcc.Api.KeysSophie
import           Bcc.Api.ProtocolParameters
import           Bcc.Api.SerialiseCBOR
import           Bcc.Api.SerialiseTextEnvelope
import           Bcc.Api.Tx

import qualified Bcc.Protocol.TOptimum.OCert as Sophie

-- ----------------------------------------------------------------------------
-- Operational certificates
--

data OperationalCertificate =
     OperationalCertificate
       !(Sophie.OCert StandardCrypto)
       !(VerificationKey StakePoolKey)
  deriving (Eq, Show)
  deriving anyclass SerialiseAsCBOR

data OperationalCertificateIssueCounter =
     OperationalCertificateIssueCounter
       !Word64
       !(VerificationKey StakePoolKey) -- For consistency checking
  deriving (Eq, Show)
  deriving anyclass SerialiseAsCBOR

instance ToCBOR OperationalCertificate where
    toCBOR (OperationalCertificate ocert vkey) =
      toCBOR (CBOR.CBORGroup ocert, vkey)

instance FromCBOR OperationalCertificate where
    fromCBOR = do
      (CBOR.CBORGroup ocert, vkey) <- fromCBOR
      return (OperationalCertificate ocert vkey)

instance ToCBOR OperationalCertificateIssueCounter where
    toCBOR (OperationalCertificateIssueCounter counter vkey) =
      toCBOR (counter, vkey)

instance FromCBOR OperationalCertificateIssueCounter where
    fromCBOR = do
      (counter, vkey) <- fromCBOR
      return (OperationalCertificateIssueCounter counter vkey)

instance HasTypeProxy OperationalCertificate where
    data AsType OperationalCertificate = AsOperationalCertificate
    proxyToAsType _ = AsOperationalCertificate

instance HasTypeProxy OperationalCertificateIssueCounter where
    data AsType OperationalCertificateIssueCounter = AsOperationalCertificateIssueCounter
    proxyToAsType _ = AsOperationalCertificateIssueCounter

instance HasTextEnvelope OperationalCertificate where
    textEnvelopeType _ = "NodeOperationalCertificate"

instance HasTextEnvelope OperationalCertificateIssueCounter where
    textEnvelopeType _ = "NodeOperationalCertificateIssueCounter"

data OperationalCertIssueError =
       -- | The stake pool verification key expected for the
       -- 'OperationalCertificateIssueCounter' does not match the signing key
       -- supplied for signing.
       --
       -- Order: pool vkey expected, pool skey supplied
       --
       OperationalCertKeyMismatch (VerificationKey StakePoolKey)
                                  (VerificationKey StakePoolKey)
  deriving Show

instance Error OperationalCertIssueError where
    displayError (OperationalCertKeyMismatch _counterKey _signingKey) =
      "Key mismatch: the signing key does not match the one that goes with the counter"
      --TODO: include key ids

issueOperationalCertificate :: VerificationKey KesKey
                            -> Either (SigningKey StakePoolKey)
                                      (SigningKey GenesisDelegateExtendedKey)
                               --TODO: this may be better with a type that
                               -- captured the three (four?) choices, stake pool
                               -- or genesis delegate, extended or normal.
                            -> Sophie.KESPeriod
                            -> OperationalCertificateIssueCounter
                            -> Either OperationalCertIssueError
                                      (OperationalCertificate,
                                      OperationalCertificateIssueCounter)
issueOperationalCertificate (KesVerificationKey kesVKey)
                            skey
                            kesPeriod
                            (OperationalCertificateIssueCounter counter poolVKey)
    | poolVKey /= poolVKey'
    = Left (OperationalCertKeyMismatch poolVKey poolVKey')

    | otherwise
    = Right (OperationalCertificate ocert poolVKey,
            OperationalCertificateIssueCounter (succ counter) poolVKey)
  where
    poolVKey' :: VerificationKey StakePoolKey
    poolVKey' = either getVerificationKey (convert . getVerificationKey) skey
      where
        convert :: VerificationKey GenesisDelegateExtendedKey
                -> VerificationKey StakePoolKey
        convert = (castVerificationKey :: VerificationKey GenesisDelegateKey
                                       -> VerificationKey StakePoolKey)
                . (castVerificationKey :: VerificationKey GenesisDelegateExtendedKey
                                       -> VerificationKey GenesisDelegateKey)

    ocert     :: Sophie.OCert StandardCrypto
    ocert     = Sophie.OCert kesVKey counter kesPeriod signature

    signature :: Sophie.SignedDSIGN
                   StandardCrypto
                   (Sophie.OCertSignable StandardCrypto)
    signature = makeSophieSignature
                  (Sophie.OCertSignable kesVKey counter kesPeriod)
                  skey'
      where
        skey' :: SophieSigningKey
        skey' = case skey of
                  Left (StakePoolSigningKey poolSKey) ->
                    SophieNormalSigningKey poolSKey
                  Right (GenesisDelegateExtendedSigningKey delegSKey) ->
                    SophieExtendedSigningKey delegSKey
