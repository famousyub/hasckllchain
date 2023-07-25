{-# LANGUAGE GADTs #-}

module Bcc.CLI.Cole.Vote
  ( ColeVoteError(..)
  , readColeVote
  , renderColeVoteError
  , runVoteCreation
  , submitColeVote
  ) where

import           Bcc.Prelude

import           Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither)
import           Control.Tracer (stdoutTracer, traceWith)
import qualified Data.ByteString as BS
import qualified Data.Text as Text


import qualified Bcc.Binary as Binary
import           Bcc.CLI.Cole.UpdateProposal (ColeUpdateProposalError,
                     readColeUpdateProposal)
import           Shardagnostic.Consensus.Ledger.SupportsMempool (txId)
import           Shardagnostic.Consensus.Util.Condense (condense)

import           Bcc.Api.Cole

import           Bcc.CLI.Cole.Genesis (ColeGenesisError)
import           Bcc.CLI.Cole.Key (ColeKeyFailure, readColeSigningKey)
import           Bcc.CLI.Cole.Tx (ColeTxError, nodeSubmitTx)
import           Bcc.CLI.Helpers (HelpersError, ensureNewFileLBS)
import           Bcc.CLI.Sophie.Commands (ColeKeyFormat (..))
import           Bcc.CLI.Types


data ColeVoteError
  = ColeVoteDecodingError !FilePath
  | ColeVoteGenesisReadError !ColeGenesisError
  | ColeVoteKeyReadFailure !ColeKeyFailure
  | ColeVoteReadFileFailure !FilePath !Text
  | ColeVoteTxSubmissionError !ColeTxError
  | ColeVoteUpdateProposalFailure !ColeUpdateProposalError
  | ColeVoteUpdateProposalDecodingError !Binary.DecoderError
  | ColeVoteUpdateHelperError !HelpersError
  deriving Show

renderColeVoteError :: ColeVoteError -> Text
renderColeVoteError bVerr =
  case bVerr of
    ColeVoteDecodingError fp -> "Error decoding Cole vote at " <>  Text.pack fp
    ColeVoteGenesisReadError genErr -> "Error reading the genesis file:" <> Text.pack (show genErr)
    ColeVoteReadFileFailure fp err -> "Error reading Cole vote at " <> Text.pack fp <> " Error: " <> err
    ColeVoteTxSubmissionError txErr -> "Error submitting the transaction: " <> Text.pack (show txErr)
    ColeVoteUpdateProposalDecodingError err -> "Error decoding Cole update proposal: " <> Text.pack (show err)
    ColeVoteUpdateProposalFailure err -> "Error reading the update proposal: " <> Text.pack (show err)
    ColeVoteUpdateHelperError err ->"Error creating the vote: " <> Text.pack (show err)
    ColeVoteKeyReadFailure err -> "Error reading the signing key: " <> Text.pack (show err)


runVoteCreation
  :: NetworkId
  -> SigningKeyFile
  -> FilePath
  -> Bool
  -> FilePath
  -> ExceptT ColeVoteError IO ()
runVoteCreation nw sKey upPropFp voteBool outputFp = do
  sK <- firstExceptT ColeVoteKeyReadFailure $ readColeSigningKey NonLegacyColeKeyFormat sKey
  proposal <- firstExceptT ColeVoteUpdateProposalFailure $ readColeUpdateProposal upPropFp
  let vote = makeColeVote nw sK proposal voteBool
  firstExceptT ColeVoteUpdateHelperError . ensureNewFileLBS outputFp
    $ serialiseToRawBytes vote

submitColeVote
  :: NetworkId
  -> FilePath
  -> ExceptT ColeVoteError IO ()
submitColeVote network voteFp = do
    vote <- readColeVote voteFp
    let genTx = toColeLedgertoColeVote vote
    traceWith stdoutTracer ("Vote TxId: " ++ condense (txId genTx))
    firstExceptT ColeVoteTxSubmissionError $ nodeSubmitTx network genTx

readColeVote :: FilePath -> ExceptT ColeVoteError IO ColeVote
readColeVote fp = do
  voteBs <- liftIO $ BS.readFile fp
  let mVote = deserialiseFromRawBytes AsColeVote voteBs
  hoistEither $ maybe (Left $ ColeVoteDecodingError fp) Right mVote
