{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module Bcc.CLI.Cole.Tx
  ( ColeTxError(..)
  , TxFile(..)
  , NewTxFile(..)
  , prettyAddress
  , readColeTx
  , normalColeTxToGenTx
  , txSpendGenesisUTxOColePBFT
  , txSpendUTxOColePBFT
  , nodeSubmitTx
  , renderColeTxError

    --TODO: remove when they are exported from the ledger
  , fromCborTxAux
  , toCborTxAux

  , ScriptValidity(..)
  )
where

import           Bcc.Prelude hiding (option, trace, (%))
import           Prelude (error)

import           Control.Monad.Trans.Except.Extra (firstExceptT, left)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text as Text
import           Formatting (sformat, (%))

import           Bcc.Api

import qualified Bcc.Binary as Binary

import qualified Bcc.Chain.Common as Common
import           Bcc.Chain.Genesis as Genesis
import qualified Bcc.Chain.UTxO as UTxO
import qualified Bcc.Crypto.Signing as Crypto

import           Bcc.Api.Cole
import           Bcc.CLI.Cole.Key (coleWitnessToVerKey)
import           Bcc.CLI.Environment
import           Bcc.CLI.Helpers (textShow)
import           Bcc.CLI.Types (SocketPath (..))
import           Shardagnostic.Consensus.Cole.Ledger (ColeBlock, GenTx (..))
import qualified Shardagnostic.Consensus.Cole.Ledger as Cole
import           Shardagnostic.Consensus.Bcc.Block (EraMismatch (..))
import qualified Shardagnostic.Network.Protocol.LocalTxSubmission.Client as Net.Tx

data ColeTxError
  = TxDeserialisationFailed !FilePath !Binary.DecoderError
  | ColeTxSubmitError !Text
  | ColeTxSubmitErrorEraMismatch !EraMismatch
  | EnvSocketError !EnvSocketError
  deriving Show

renderColeTxError :: ColeTxError -> Text
renderColeTxError err =
  case err of
    ColeTxSubmitError res -> "Error while submitting tx: " <> res
    ColeTxSubmitErrorEraMismatch EraMismatch{ledgerEraName, otherEraName} ->
      "The era of the node and the tx do not match. " <>
      "The node is running in the " <> ledgerEraName <>
      " era, but the transaction is for the " <> otherEraName <> " era."
    TxDeserialisationFailed txFp decErr ->
      "Transaction deserialisation failed at " <> textShow txFp <> " Error: " <> textShow decErr
    EnvSocketError envSockErr -> renderEnvSocketError envSockErr


newtype TxFile =
  TxFile FilePath
  deriving (Eq, Ord, Show, IsString)

newtype NewTxFile =
  NewTxFile FilePath
  deriving (Eq, Ord, Show, IsString)


-- | Pretty-print an address in its Base58 form, and also
--   its full structure.
prettyAddress :: Address ColeAddr -> Text
prettyAddress (ColeAddress addr) = sformat
  (Common.addressF %"\n"%Common.addressDetailedF)
  addr addr

readColeTx :: TxFile -> ExceptT ColeTxError IO (UTxO.ATxAux ByteString)
readColeTx (TxFile fp) = do
  txBS <- liftIO $ LB.readFile fp
  case fromCborTxAux txBS of
    Left e -> left $ TxDeserialisationFailed fp e
    Right tx -> pure tx

-- | The 'GenTx' is all the kinds of transactions that can be submitted
-- and \"normal\" Cole transactions are just one of the kinds.
normalColeTxToGenTx :: UTxO.ATxAux ByteString -> GenTx ColeBlock
normalColeTxToGenTx tx' = Cole.ColeTx (Cole.coleIdTx tx') tx'

-- | Given a genesis, and a pair of a genesis public key and address,
--   reconstruct a TxIn corresponding to the genesis UTxO entry.
genesisUTxOTxIn :: Genesis.Config -> Crypto.VerificationKey -> Common.Address -> UTxO.TxIn
genesisUTxOTxIn gc vk genAddr =
  handleMissingAddr $ fst <$> Map.lookup genAddr initialUtxo
  where
    initialUtxo :: Map Common.Address (UTxO.TxIn, UTxO.TxOut)
    initialUtxo =
          Map.fromList
        . mapMaybe (\(inp, out) -> mkEntry inp genAddr <$> keyMatchesUTxO vk out)
        . fromCompactTxInTxOutList
        . Map.toList
        . UTxO.unUTxO
        . UTxO.genesisUtxo
        $ gc
      where
        mkEntry :: UTxO.TxIn
                -> Common.Address
                -> UTxO.TxOut
                -> (Common.Address, (UTxO.TxIn, UTxO.TxOut))
        mkEntry inp addr out = (addr, (inp, out))

    fromCompactTxInTxOutList :: [(UTxO.CompactTxIn, UTxO.CompactTxOut)]
                             -> [(UTxO.TxIn, UTxO.TxOut)]
    fromCompactTxInTxOutList =
        map (bimap UTxO.fromCompactTxIn UTxO.fromCompactTxOut)

    keyMatchesUTxO :: Crypto.VerificationKey -> UTxO.TxOut -> Maybe UTxO.TxOut
    keyMatchesUTxO key out =
      if Common.checkVerKeyAddress key (UTxO.txOutAddress out)
      then Just out else Nothing

    handleMissingAddr :: Maybe UTxO.TxIn -> UTxO.TxIn
    handleMissingAddr  = fromMaybe . error
      $  "\nGenesis UTxO has no address\n"
      <> T.unpack (prettyAddress (ColeAddress genAddr))
      <> "\n\nIt has the following, though:\n\n"
      <> Bcc.Prelude.concat (T.unpack . prettyAddress <$> map ColeAddress (Map.keys initialUtxo))

-- | Generate a transaction spending genesis UTxO at a given address,
--   to given outputs, signed by the given key.
txSpendGenesisUTxOColePBFT
  :: Genesis.Config
  -> NetworkId
  -> SomeColeSigningKey
  -> Address ColeAddr
  -> [TxOut ColeEra]
  -> Tx ColeEra
txSpendGenesisUTxOColePBFT gc nId sk (ColeAddress bAddr) outs = do
    let txBodyCont =
          TxBodyContent
            [ (fromColeTxIn txIn
              , BuildTxWith (KeyWitness KeyWitnessForSpending))
            ]
            TxInsCollateralNone
            outs
            (TxFeeImplicit TxFeesImplicitInColeEra)
            ( TxValidityNoLowerBound
            , TxValidityNoUpperBound ValidityNoUpperBoundInColeEra
            )
            TxMetadataNone
            TxAuxScriptsNone
            (BuildTxWith TxExtraScriptDataNone)
            TxExtraKeyWitnessesNone
            (BuildTxWith Nothing)
            TxWithdrawalsNone
            TxCertificatesNone
            TxUpdateProposalNone
            TxMintNone
            TxScriptValidityNone
    case makeTransactionBody txBodyCont of
      Left err -> error $ "Error occured while creating a Cole genesis based UTxO transaction: " <> show err
      Right txBody -> let bWit = fromColeWitness sk nId txBody
                      in makeSignedTransaction [bWit] txBody
  where
    ColeVerificationKey vKey = coleWitnessToVerKey sk

    txIn :: UTxO.TxIn
    txIn  = genesisUTxOTxIn gc vKey bAddr

-- | Generate a transaction from given Tx inputs to outputs,
--   signed by the given key.
txSpendUTxOColePBFT
  :: NetworkId
  -> SomeColeSigningKey
  -> [TxIn]
  -> [TxOut ColeEra]
  -> Tx ColeEra
txSpendUTxOColePBFT nId sk txIns outs = do
  let txBodyCont = TxBodyContent
                     [ ( txIn
                       , BuildTxWith (KeyWitness KeyWitnessForSpending)
                       ) | txIn <- txIns
                     ]
                     TxInsCollateralNone
                     outs
                     (TxFeeImplicit TxFeesImplicitInColeEra)
                     ( TxValidityNoLowerBound
                     , TxValidityNoUpperBound ValidityNoUpperBoundInColeEra
                     )
                     TxMetadataNone
                     TxAuxScriptsNone
                     (BuildTxWith TxExtraScriptDataNone)
                     TxExtraKeyWitnessesNone
                     (BuildTxWith Nothing)
                     TxWithdrawalsNone
                     TxCertificatesNone
                     TxUpdateProposalNone
                     TxMintNone
                     TxScriptValidityNone
  case makeTransactionBody txBodyCont of
    Left err -> error $ "Error occured while creating a Cole genesis based UTxO transaction: " <> show err
    Right txBody -> let bWit = fromColeWitness sk nId txBody
                    in makeSignedTransaction [bWit] txBody

fromColeWitness :: SomeColeSigningKey -> NetworkId -> TxBody ColeEra -> KeyWitness ColeEra
fromColeWitness bw nId txBody =
  case bw of
    AColeSigningKeyLegacy sk -> makeColeKeyWitness nId txBody sk
    AColeSigningKey sk' -> makeColeKeyWitness nId txBody sk'

-- | Submit a transaction to a node specified by topology info.
nodeSubmitTx
  :: NetworkId
  -> GenTx ColeBlock
  -> ExceptT ColeTxError IO ()
nodeSubmitTx network gentx = do
    SocketPath socketPath <- firstExceptT EnvSocketError readEnvSocketPath
    let connctInfo =
          LocalNodeConnectInfo {
            localNodeSocketPath = socketPath,
            localNodeNetworkId = network,
            localConsensusModeParams = BccModeParams (EpochSlots 21600)
          }
    res <- liftIO $ submitTxToNodeLocal connctInfo (TxInColeSpecial gentx ColeEraInBccMode)
    case res of
      Net.Tx.SubmitSuccess -> liftIO $ putTextLn "Transaction successfully submitted."
      Net.Tx.SubmitFail reason ->
        case reason of
          TxValidationErrorInMode err _eraInMode -> left . ColeTxSubmitError . Text.pack $ show err
          TxValidationEraMismatch mismatchErr -> left $ ColeTxSubmitErrorEraMismatch mismatchErr

    return ()


--TODO: remove these local definitions when the updated ledger lib is available
fromCborTxAux :: LB.ByteString ->  Either Binary.DecoderError (UTxO.ATxAux B.ByteString)
fromCborTxAux lbs =
    fmap (annotationBytes lbs)
      $ Binary.decodeFullDecoder "Bcc.Chain.UTxO.TxAux.fromCborTxAux"
                                 Binary.fromCBOR lbs
  where
    annotationBytes :: Functor f => LB.ByteString -> f Binary.ByteSpan -> f B.ByteString
    annotationBytes bytes = fmap (LB.toStrict . Binary.slice bytes)

toCborTxAux :: UTxO.ATxAux ByteString -> LB.ByteString
toCborTxAux = LB.fromStrict . UTxO.aTaAnnotation -- The ByteString anotation is the CBOR encoded version.
