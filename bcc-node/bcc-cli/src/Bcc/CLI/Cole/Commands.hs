{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Bcc.CLI.Cole.Commands
  ( ColeCommand (..)
  , NodeCmd (..)
  , VerificationKeyFile (..)
  , NewVerificationKeyFile (..)
  , CertificateFile (..)
  , NewCertificateFile (..)
  ) where

import           Bcc.Prelude

import           Bcc.Chain.Update (InstallerHash (..), ProtocolVersion (..),
                     SoftwareVersion (..), SystemTag (..))

import           Bcc.Api (NetworkId, TxIn)
import           Bcc.Api.Cole (Address (..), ColeAddr, ColeEra,
                     ColeProtocolParametersUpdate (..), TxOut)

import           Bcc.CLI.Cole.Genesis
import           Bcc.CLI.Cole.Key
import           Bcc.CLI.Cole.Tx
import           Bcc.CLI.Types

import           Bcc.CLI.Sophie.Commands (ColeKeyFormat)

data ColeCommand =

  --- Node Related Commands ---
    NodeCmd NodeCmd

  --- Genesis Related Commands ---
  | Genesis
        NewDirectory
        GenesisParameters

  | PrintGenesisHash
        GenesisFile

  --- Key Related Commands ---
  | Keygen
        NewSigningKeyFile

  | ToVerification
        ColeKeyFormat
        SigningKeyFile
        NewVerificationKeyFile

  | PrettySigningKeyPublic
        ColeKeyFormat
        SigningKeyFile

  | MigrateDelegateKeyFrom
        SigningKeyFile
        -- ^ Old key
        NewSigningKeyFile
        -- ^ New Key

  | PrintSigningKeyAddress
        ColeKeyFormat
        NetworkId
        SigningKeyFile

  | GetLocalNodeTip
        NetworkId

    -----------------------------------

  | SubmitTx
        NetworkId
        TxFile
        -- ^ Filepath of transaction to submit.

  | SpendGenesisUTxO
        GenesisFile
        NetworkId
        ColeKeyFormat
        NewTxFile
        -- ^ Filepath of the newly created transaction.
        SigningKeyFile
        -- ^ Signing key of genesis UTxO owner.
        (Address ColeAddr)
        -- ^ Genesis UTxO address.
        [TxOut ColeEra]
        -- ^ Tx output.
  | SpendUTxO
        NetworkId
        ColeKeyFormat
        NewTxFile
        -- ^ Filepath of the newly created transaction.
        SigningKeyFile
        -- ^ Signing key of Tx underwriter.
        [TxIn]
        -- ^ Inputs available for spending to the Tx underwriter's key.
        [TxOut ColeEra]
        -- ^ Genesis UTxO output Address.

  | GetTxId TxFile

    --- Misc Commands ---

  | ValidateCBOR
        CBORObject
        -- ^ Type of the CBOR object
        FilePath

  | PrettyPrintCBOR
        FilePath
  deriving Show


data NodeCmd = CreateVote
               NetworkId
               SigningKeyFile
               FilePath -- filepath to update proposal
               Bool
               FilePath
             | UpdateProposal
               NetworkId
               SigningKeyFile
               ProtocolVersion
               SoftwareVersion
               SystemTag
               InstallerHash
               FilePath
               ColeProtocolParametersUpdate
             | SubmitUpdateProposal
               NetworkId
               FilePath
               -- ^ Update proposal filepath.
             | SubmitVote
               NetworkId
               FilePath
               -- ^ Vote filepath.
              deriving Show


newtype NewCertificateFile
  = NewCertificateFile { nFp :: FilePath }
  deriving (Eq, Show, IsString)
