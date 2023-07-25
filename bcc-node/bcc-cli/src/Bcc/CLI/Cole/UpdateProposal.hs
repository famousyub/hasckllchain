{-# LANGUAGE GADTs #-}

module Bcc.CLI.Cole.UpdateProposal
  ( ColeUpdateProposalError(..)
  , runProposalCreation
  , readColeUpdateProposal
  , renderColeUpdateProposalError
  , submitColeUpdateProposal
  ) where

import           Bcc.Prelude

import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither)
import           Control.Tracer (stdoutTracer, traceWith)
import qualified Data.ByteString as BS

import           Bcc.Chain.Update (InstallerHash (..), ProtocolVersion (..),
                     SoftwareVersion (..), SystemTag (..))

import           Shardagnostic.Consensus.Ledger.SupportsMempool (txId)
import           Shardagnostic.Consensus.Util.Condense (condense)

import           Bcc.Api (NetworkId, SerialiseAsRawBytes (..))
import           Bcc.Api.Cole (AsType (AsColeUpdateProposal), ColeProtocolParametersUpdate,
                     ColeUpdateProposal, makeColeUpdateProposal, toColeLedgerUpdateProposal)

import           Bcc.CLI.Cole.Genesis (ColeGenesisError)
import           Bcc.CLI.Cole.Key (ColeKeyFailure, readColeSigningKey)
import           Bcc.CLI.Cole.Tx (ColeTxError, nodeSubmitTx)
import           Bcc.CLI.Helpers (HelpersError, ensureNewFileLBS, renderHelpersError, textShow)
import           Bcc.CLI.Sophie.Commands (ColeKeyFormat (..))
import           Bcc.CLI.Types

data ColeUpdateProposalError
  = ColeReadUpdateProposalFileFailure !FilePath !Text
  | ColeUpdateProposalWriteError !HelpersError
  | ColeUpdateProposalGenesisReadError !FilePath !ColeGenesisError
  | ColeUpdateProposalTxError !ColeTxError
  | ReadSigningKeyFailure !FilePath !ColeKeyFailure
  | UpdateProposalDecodingError !FilePath
  deriving Show

renderColeUpdateProposalError :: ColeUpdateProposalError -> Text
renderColeUpdateProposalError err =
  case err of
    ColeReadUpdateProposalFileFailure fp rErr ->
      "Error reading update proposal at " <> textShow fp <> " Error: " <> textShow rErr
    ColeUpdateProposalWriteError hErr ->
      "Error writing update proposal: " <> renderHelpersError hErr
    ColeUpdateProposalGenesisReadError fp rErr ->
      "Error reading update proposal at: " <> textShow fp <> " Error: " <> textShow rErr
    ColeUpdateProposalTxError txErr ->
      "Error submitting update proposal: " <> textShow txErr
    ReadSigningKeyFailure fp rErr ->
      "Error reading signing key at: " <> textShow fp <> " Error: " <> textShow rErr
    UpdateProposalDecodingError fp ->
      "Error decoding update proposal at: " <> textShow fp

runProposalCreation
  :: NetworkId
  -> SigningKeyFile
  -> ProtocolVersion
  -> SoftwareVersion
  -> SystemTag
  -> InstallerHash
  -> FilePath
  -> ColeProtocolParametersUpdate
  -> ExceptT ColeUpdateProposalError IO ()
runProposalCreation nw sKey@(SigningKeyFile sKeyfp) pVer sVer
                    sysTag insHash outputFp params = do
  sK <- firstExceptT (ReadSigningKeyFailure sKeyfp) $ readColeSigningKey NonLegacyColeKeyFormat sKey
  let proposal = makeColeUpdateProposal nw pVer sVer sysTag insHash sK params
  firstExceptT ColeUpdateProposalWriteError $
    ensureNewFileLBS outputFp $ serialiseToRawBytes proposal

readColeUpdateProposal :: FilePath -> ExceptT ColeUpdateProposalError IO ColeUpdateProposal
readColeUpdateProposal fp = do
  proposalBs <- handleIOExceptT (ColeReadUpdateProposalFileFailure fp . toS . displayException)
                  $ BS.readFile fp
  let mProposal = deserialiseFromRawBytes AsColeUpdateProposal proposalBs
  hoistEither $ maybe (Left $ UpdateProposalDecodingError fp) Right mProposal

submitColeUpdateProposal
  :: NetworkId
  -> FilePath
  -> ExceptT ColeUpdateProposalError IO ()
submitColeUpdateProposal network proposalFp = do
    proposal  <- readColeUpdateProposal proposalFp
    let genTx = toColeLedgerUpdateProposal proposal
    traceWith stdoutTracer $
      "Update proposal TxId: " ++ condense (txId genTx)
    firstExceptT ColeUpdateProposalTxError $ nodeSubmitTx network genTx

