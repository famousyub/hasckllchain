{-# LANGUAGE GADTs #-}

module Bcc.CLI.Cole.Run
  ( ColeClientCmdError
  , renderColeClientCmdError
  , runColeClientCommand
  ) where

import           Bcc.Prelude

import           Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither, left)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.IO as TL
import qualified Formatting as F

import qualified Bcc.Chain.Genesis as Genesis

import qualified Bcc.Crypto.Hashing as Crypto
import qualified Bcc.Crypto.Signing as Crypto

import           Bcc.Api hiding (UpdateProposal, GenesisParameters)
import           Bcc.Api.Cole (SomeColeSigningKey (..), Tx (..), VerificationKey (..))

import           Shardagnostic.Consensus.Cole.Ledger (ColeBlock)
import           Shardagnostic.Consensus.Ledger.SupportsMempool (ApplyTxErr)

import           Bcc.CLI.Cole.Commands
import           Bcc.CLI.Cole.Delegation
import           Bcc.CLI.Cole.Genesis
import           Bcc.CLI.Cole.Key
import           Bcc.CLI.Cole.Query
import           Bcc.CLI.Cole.Tx
import           Bcc.CLI.Cole.UpdateProposal
import           Bcc.CLI.Cole.Vote
import           Bcc.CLI.Helpers
import           Bcc.CLI.Sophie.Commands (ColeKeyFormat (..))
import           Bcc.CLI.Types

-- | Data type that encompasses all the possible errors of the
-- Cole client.
data ColeClientCmdError
  = ColeCmdDelegationError !ColeDelegationError
  | ColeCmdGenesisError !ColeGenesisError
  | ColeCmdHelpersError !HelpersError
  | ColeCmdKeyFailure !ColeKeyFailure
  | ColeCmdQueryError !ColeQueryError
  | ColeCmdTxError !ColeTxError
  | ColeCmdTxSubmitError !(ApplyTxErr ColeBlock)
  | ColeCmdUpdateProposalError !ColeUpdateProposalError
  | ColeCmdVoteError !ColeVoteError
  deriving Show

renderColeClientCmdError :: ColeClientCmdError -> Text
renderColeClientCmdError err =
  case err of
    ColeCmdDelegationError e -> renderColeDelegationError e
    ColeCmdGenesisError e -> renderColeGenesisError e
    ColeCmdHelpersError e -> renderHelpersError e
    ColeCmdKeyFailure e -> renderColeKeyFailure e
    ColeCmdQueryError e -> renderColeQueryError e
    ColeCmdTxError e -> renderColeTxError e
    ColeCmdTxSubmitError e ->
      "Error while submitting Cole tx: " <> Text.pack (show e)
    ColeCmdUpdateProposalError e -> renderColeUpdateProposalError e
    ColeCmdVoteError e -> renderColeVoteError e

runColeClientCommand :: ColeCommand -> ExceptT ColeClientCmdError IO ()
runColeClientCommand c =
  case c of
    NodeCmd bc -> runNodeCmd bc
    Genesis outDir params -> runGenesisCommand outDir params
    GetLocalNodeTip network -> firstExceptT ColeCmdQueryError $ runGetLocalNodeTip network
    ValidateCBOR cborObject fp -> runValidateCBOR cborObject fp
    PrettyPrintCBOR fp -> runPrettyPrintCBOR fp
    PrettySigningKeyPublic bKeyFormat skF -> runPrettySigningKeyPublic bKeyFormat skF
    MigrateDelegateKeyFrom oldKey nskf ->
       runMigrateDelegateKeyFrom oldKey nskf
    PrintGenesisHash genFp -> runPrintGenesisHash genFp
    PrintSigningKeyAddress bKeyFormat networkid skF -> runPrintSigningKeyAddress bKeyFormat networkid skF
    Keygen nskf -> runKeygen nskf
    ToVerification bKeyFormat skFp nvkFp -> runToVerification bKeyFormat skFp nvkFp
    SubmitTx network fp -> runSubmitTx network fp
    GetTxId fp -> runGetTxId fp
    SpendGenesisUTxO genFp nw era nftx ctKey genRichAddr outs ->
      runSpendGenesisUTxO genFp nw era nftx ctKey genRichAddr outs
    SpendUTxO nw era nftx ctKey ins outs ->
      runSpendUTxO nw era nftx ctKey ins outs


runNodeCmd :: NodeCmd -> ExceptT ColeClientCmdError IO ()
runNodeCmd (CreateVote nw sKey upPropFp voteBool outputFp) =
  firstExceptT ColeCmdVoteError $ runVoteCreation nw sKey upPropFp voteBool outputFp

runNodeCmd (SubmitUpdateProposal network proposalFp) =
    firstExceptT ColeCmdUpdateProposalError
      $ submitColeUpdateProposal network proposalFp

runNodeCmd (SubmitVote network voteFp) =
    firstExceptT ColeCmdVoteError $ submitColeVote network voteFp

runNodeCmd (UpdateProposal nw sKey pVer sVer sysTag insHash outputFp params) =
  firstExceptT ColeCmdUpdateProposalError
    $ runProposalCreation nw sKey pVer sVer sysTag insHash outputFp params

runGenesisCommand :: NewDirectory -> GenesisParameters -> ExceptT ColeClientCmdError IO ()
runGenesisCommand outDir params = do
  (genData, genSecrets) <- firstExceptT ColeCmdGenesisError $ mkGenesis params
  firstExceptT ColeCmdGenesisError $ dumpGenesis outDir genData genSecrets

runValidateCBOR :: CBORObject -> FilePath -> ExceptT ColeClientCmdError IO ()
runValidateCBOR cborObject fp = do
  bs <- firstExceptT ColeCmdHelpersError $ readCBOR fp
  res <- hoistEither . first ColeCmdHelpersError $ validateCBOR cborObject bs
  liftIO $ putTextLn res

runPrettyPrintCBOR :: FilePath -> ExceptT ColeClientCmdError IO ()
runPrettyPrintCBOR fp = do
  bs <- firstExceptT ColeCmdHelpersError $ readCBOR fp
  firstExceptT ColeCmdHelpersError $ pPrintCBOR bs

runPrettySigningKeyPublic :: ColeKeyFormat -> SigningKeyFile -> ExceptT ColeClientCmdError IO ()
runPrettySigningKeyPublic bKeyFormat skF = do
  sK <- firstExceptT ColeCmdKeyFailure $ readColeSigningKey bKeyFormat skF
  liftIO . putTextLn . prettyPublicKey $ coleWitnessToVerKey sK

runMigrateDelegateKeyFrom
  :: SigningKeyFile
  -- ^ Legacy Cole signing key
  -> NewSigningKeyFile
  -> ExceptT ColeClientCmdError IO ()
runMigrateDelegateKeyFrom oldKey@(SigningKeyFile fp) (NewSigningKeyFile newKey) = do
  sk <- firstExceptT ColeCmdKeyFailure $ readColeSigningKey LegacyColeKeyFormat oldKey
  migratedWitness <- case sk of
                       AColeSigningKeyLegacy (ColeSigningKeyLegacy sKey) ->
                         return . AColeSigningKey $ ColeSigningKey sKey
                       AColeSigningKey _ ->
                         left . ColeCmdKeyFailure $ CannotMigrateFromNonLegacySigningKey fp
  firstExceptT ColeCmdHelpersError . ensureNewFileLBS newKey $ serialiseColeWitness migratedWitness

runPrintGenesisHash :: GenesisFile -> ExceptT ColeClientCmdError IO ()
runPrintGenesisHash genFp = do
    genesis <- firstExceptT ColeCmdGenesisError $
                 readGenesis genFp dummyNetwork
    liftIO . putTextLn $ formatter genesis
  where
    -- For this purpose of getting the hash, it does not matter what network
    -- value we use here.
    dummyNetwork :: NetworkId
    dummyNetwork = Mainnet

    formatter :: Genesis.Config -> Text
    formatter = F.sformat Crypto.hashHexF
              . Genesis.unGenesisHash
              . Genesis.configGenesisHash

runPrintSigningKeyAddress
  :: ColeKeyFormat
  -> NetworkId
  -> SigningKeyFile
  -> ExceptT ColeClientCmdError IO ()
runPrintSigningKeyAddress bKeyFormat networkid skF = do
  sK <- firstExceptT ColeCmdKeyFailure $ readColeSigningKey bKeyFormat skF
  let sKeyAddr = prettyAddress . makeColeAddress networkid $ coleWitnessToVerKey sK
  liftIO $ putTextLn sKeyAddr

runKeygen :: NewSigningKeyFile -> ExceptT ColeClientCmdError IO ()
runKeygen (NewSigningKeyFile skF)  = do
  sK <- liftIO $ generateSigningKey AsColeKey
  firstExceptT ColeCmdHelpersError . ensureNewFileLBS skF $ serialiseToRawBytes sK

runToVerification :: ColeKeyFormat -> SigningKeyFile -> NewVerificationKeyFile -> ExceptT ColeClientCmdError IO ()
runToVerification bKeyFormat skFp (NewVerificationKeyFile vkFp) = do
  sk <- firstExceptT ColeCmdKeyFailure $ readColeSigningKey bKeyFormat skFp
  let ColeVerificationKey vK = coleWitnessToVerKey sk
  let vKey = Builder.toLazyText $ Crypto.formatFullVerificationKey vK
  firstExceptT ColeCmdHelpersError $ ensureNewFile TL.writeFile vkFp vKey

runSubmitTx :: NetworkId -> TxFile -> ExceptT ColeClientCmdError IO ()
runSubmitTx network fp = do
    tx <- firstExceptT ColeCmdTxError $ readColeTx fp
    firstExceptT ColeCmdTxError $
      nodeSubmitTx network (normalColeTxToGenTx tx)

runGetTxId :: TxFile -> ExceptT ColeClientCmdError IO ()
runGetTxId fp = firstExceptT ColeCmdTxError $ do
    tx <- readColeTx fp
    let txbody = getTxBody (ColeTx tx)
        txid   = getTxId txbody
    liftIO $ BS.putStrLn $ serialiseToRawBytesHex txid

runSpendGenesisUTxO
  :: GenesisFile
  -> NetworkId
  -> ColeKeyFormat
  -> NewTxFile
  -> SigningKeyFile
  -> Address ColeAddr
  -> [TxOut ColeEra]
  -> ExceptT ColeClientCmdError IO ()
runSpendGenesisUTxO genesisFile nw bKeyFormat (NewTxFile ctTx) ctKey genRichAddr outs = do
    genesis <- firstExceptT ColeCmdGenesisError $ readGenesis genesisFile nw
    sk <- firstExceptT ColeCmdKeyFailure $ readColeSigningKey bKeyFormat ctKey

    let tx = txSpendGenesisUTxOColePBFT genesis nw sk genRichAddr outs
    firstExceptT ColeCmdHelpersError . ensureNewFileLBS ctTx $ serialiseToCBOR tx

runSpendUTxO
  :: NetworkId
  -> ColeKeyFormat
  -> NewTxFile
  -> SigningKeyFile
  -> [TxIn]
  -> [TxOut ColeEra]
  -> ExceptT ColeClientCmdError IO ()
runSpendUTxO nw bKeyFormat (NewTxFile ctTx) ctKey ins outs = do
    sk <- firstExceptT ColeCmdKeyFailure $ readColeSigningKey bKeyFormat ctKey

    let gTx = txSpendUTxOColePBFT nw sk ins outs
    firstExceptT ColeCmdHelpersError . ensureNewFileLBS ctTx $ serialiseToCBOR gTx
