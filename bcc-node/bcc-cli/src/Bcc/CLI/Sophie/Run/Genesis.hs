{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Bcc.CLI.Sophie.Run.Genesis
  ( SophieGenesisCmdError(..)
  , readSophieGenesis
  , readAurumGenesis
  , renderSophieGenesisCmdError
  , runGenesisCmd
  ) where

import           Bcc.Prelude
import           Prelude (id)

import           Data.Aeson
import qualified Data.Aeson as Aeson
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Binary.Get as Bin
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Coerce (coerce)
import qualified Data.List as List
import qualified Data.List.Split as List
import qualified Data.Map.Strict as Map

import qualified Data.Sequence.Strict as Seq
import           Data.String (fromString)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)

import           Bcc.Binary (ToCBOR (..))

import           Bcc.Crypto.Hash (HashAlgorithm)
import qualified Bcc.Crypto.Hash as Hash
import qualified Bcc.Crypto.Random as Crypto
import           Crypto.Random as Crypto

import           System.Directory (createDirectoryIfMissing, listDirectory)
import           System.FilePath (takeExtension, takeExtensions, (</>))
import           System.IO.Error (isDoesNotExistError)

import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither, left,
                   newExceptT)

import qualified Bcc.Crypto.Hash as Crypto

import           Bcc.Api
import           Bcc.Api.Sophie

import           Shardagnostic.Consensus.BlockchainTime (SystemStart (..))
import           Shardagnostic.Consensus.Sophie.Eras (StandardSophie)
import           Shardagnostic.Consensus.Sophie.Node (SophieGenesisStaking (..))

import qualified Bcc.Ledger.Aurum.Genesis as Aurum
import qualified Bcc.Ledger.Aurum.Language as Aurum
import qualified Bcc.Ledger.Aurum.Scripts as Aurum
import qualified Bcc.Ledger.BaseTypes as Ledger
import           Bcc.Ledger.Coin (Coin (..))
import qualified Bcc.Ledger.Keys as Ledger
import qualified Sophie.Spec.Ledger.API as Ledger
import qualified Sophie.Spec.Ledger.PParams as Sophie

import           Bcc.Ledger.Crypto (ADDRHASH, Crypto, StandardCrypto)
import           Bcc.Ledger.Era ()

import           Bcc.CLI.Helpers (textShow)
import           Bcc.CLI.Sophie.Commands
import           Bcc.CLI.Sophie.Key
import           Bcc.CLI.Sophie.Orphans ()
import           Bcc.CLI.Sophie.Parsers (renderTxIn)
import           Bcc.CLI.Sophie.Run.Address
import           Bcc.CLI.Sophie.Run.Node (SophieNodeCmdError (..), renderSophieNodeCmdError,
                   runNodeIssueOpCert, runNodeKeyGenCold, runNodeKeyGenKES, runNodeKeyGenVRF)
import           Bcc.CLI.Sophie.Run.Pool (SophiePoolCmdError (..), renderSophiePoolCmdError)
import           Bcc.CLI.Sophie.Run.StakeAddress (SophieStakeAddressCmdError (..),
                   renderSophieStakeAddressCmdError, runStakeAddressKeyGen)
import           Bcc.CLI.Types
import           Zerepoch.V1.Ledger.Api (defaultCostModelParams)

{- HLINT ignore "Reduce duplication" -}

data SophieGenesisCmdError
  = SophieGenesisCmdAesonDecodeError !FilePath !Text
  | SophieGenesisCmdGenesisFileError !(FileError ())
  | SophieGenesisCmdFileError !(FileError ())
  | SophieGenesisCmdMismatchedGenesisKeyFiles [Int] [Int] [Int]
  | SophieGenesisCmdMismatchedVestedKeyFiles [Int] [Int] [Int]
  | SophieGenesisCmdFilesNoIndex [FilePath]
  | SophieGenesisCmdFilesDupIndex [FilePath]
  | SophieGenesisCmdTextEnvReadFileError !(FileError TextEnvelopeError)
  | SophieGenesisCmdUnexpectedAddressVerificationKey !VerificationKeyFile !Text !SomeAddressVerificationKey
  | SophieGenesisCmdTooFewPoolsForBulkCreds !Word !Word !Word
  | SophieGenesisCmdAddressCmdError !SophieAddressCmdError
  | SophieGenesisCmdNodeCmdError !SophieNodeCmdError
  | SophieGenesisCmdPoolCmdError !SophiePoolCmdError
  | SophieGenesisCmdStakeAddressCmdError !SophieStakeAddressCmdError
  | SophieGenesisCmdCostModelsError !FilePath
  deriving Show

renderSophieGenesisCmdError :: SophieGenesisCmdError -> Text
renderSophieGenesisCmdError err =
  case err of
    SophieGenesisCmdAesonDecodeError fp decErr ->
      "Error while decoding Sophie genesis at: " <> textShow fp <> " Error: " <> textShow decErr
    SophieGenesisCmdGenesisFileError fe -> Text.pack $ displayError fe
    SophieGenesisCmdFileError fe -> Text.pack $ displayError fe
    SophieGenesisCmdMismatchedGenesisKeyFiles gfiles dfiles vfiles ->
      "Mismatch between the files found:\n"
        <> "Genesis key file indexes:      " <> textShow gfiles <> "\n"
        <> "Delegate key file indexes:     " <> textShow dfiles <> "\n"
        <> "Delegate VRF key file indexes: " <> textShow vfiles
    SophieGenesisCmdMismatchedVestedKeyFiles afiles adfiles avfiles ->
      "Mismatch between the files found:\n"
        <> "Vested key file indexes:      " <> textShow afiles <> "\n"
        <> "VestedDelegate key file indexes:     " <> textShow adfiles <> "\n"
        <> "VestedDelegate VRF key file indexes: " <> textShow avfiles
    SophieGenesisCmdFilesNoIndex files ->
      "The genesis keys files are expected to have a numeric index but these do not:\n"
        <> Text.unlines (map Text.pack files)
    SophieGenesisCmdFilesDupIndex files ->
      "The genesis keys files are expected to have a unique numeric index but these do not:\n"
        <> Text.unlines (map Text.pack files)
    SophieGenesisCmdTextEnvReadFileError fileErr -> Text.pack $ displayError fileErr
    SophieGenesisCmdUnexpectedAddressVerificationKey (VerificationKeyFile file) expect got -> mconcat
      [ "Unexpected address verification key type in file ", Text.pack file
      , ", expected: ", expect, ", got: ", textShow got
      ]
    SophieGenesisCmdTooFewPoolsForBulkCreds pools files perPool -> mconcat
      [ "Number of pools requested for generation (", textShow pools
      , ") is insufficient to fill ", textShow files
      , " bulk files, with ", textShow perPool, " pools per file."
      ]
    SophieGenesisCmdAddressCmdError e -> renderSophieAddressCmdError e
    SophieGenesisCmdNodeCmdError e -> renderSophieNodeCmdError e
    SophieGenesisCmdPoolCmdError e -> renderSophiePoolCmdError e
    SophieGenesisCmdStakeAddressCmdError e -> renderSophieStakeAddressCmdError e
    SophieGenesisCmdCostModelsError fp ->
      "Cost model is invalid: " <> Text.pack fp

runGenesisCmd :: GenesisCmd -> ExceptT SophieGenesisCmdError IO ()
runGenesisCmd (GenesisKeyGenGenesis vk sk) = runGenesisKeyGenGenesis vk sk
runGenesisCmd (GenesisKeyGenDelegate vk sk ctr) = runGenesisKeyGenDelegate vk sk ctr
runGenesisCmd (GenesisKeyGenVested vk sk) = runGenesisKeyGenVested vk sk
runGenesisCmd (GenesisKeyGenVestedDelegate vk sk ctr) = runGenesisKeyGenVestedDelegate vk sk ctr
runGenesisCmd (GenesisKeyGenUTxO vk sk) = runGenesisKeyGenUTxO vk sk
runGenesisCmd (GenesisCmdKeyHash vk) = runGenesisKeyHash vk
runGenesisCmd (GenesisVerKey vk sk) = runGenesisVerKey vk sk
runGenesisCmd (GenesisTxIn vk nw mOutFile) = runGenesisTxIn vk nw mOutFile
runGenesisCmd (GenesisAddr vk nw mOutFile) = runGenesisAddr vk nw mOutFile
runGenesisCmd (GenesisCreate gd gn vn un ms am nw) = runGenesisCreate gd gn vn un ms am nw
runGenesisCmd (GenesisCreateStaked gd gn gp gl vn un ms am ds nw bf bp su) = runGenesisCreateStaked gd gn gp gl vn un ms am ds nw bf bp su
runGenesisCmd (GenesisHashFile gf) = runGenesisHashFile gf

--
-- Genesis command implementations
--

runGenesisKeyGenGenesis :: VerificationKeyFile -> SigningKeyFile
                        -> ExceptT SophieGenesisCmdError IO ()
runGenesisKeyGenGenesis (VerificationKeyFile vkeyPath)
                        (SigningKeyFile skeyPath) = do
    skey <- liftIO $ generateSigningKey AsGenesisKey
    let vkey = getVerificationKey skey
    firstExceptT SophieGenesisCmdGenesisFileError
      . newExceptT
      $ writeFileTextEnvelope skeyPath (Just skeyDesc) skey
    firstExceptT SophieGenesisCmdGenesisFileError
      . newExceptT
      $ writeFileTextEnvelope vkeyPath (Just vkeyDesc) vkey
  where
    skeyDesc, vkeyDesc :: TextEnvelopeDescr
    skeyDesc = "Genesis Signing Key"
    vkeyDesc = "Genesis Verification Key"


runGenesisKeyGenDelegate :: VerificationKeyFile
                         -> SigningKeyFile
                         -> OpCertCounterFile
                         -> ExceptT SophieGenesisCmdError IO ()
runGenesisKeyGenDelegate (VerificationKeyFile vkeyPath)
                         (SigningKeyFile skeyPath)
                         (OpCertCounterFile ocertCtrPath) = do
    skey <- liftIO $ generateSigningKey AsGenesisDelegateKey
    let vkey = getVerificationKey skey
    firstExceptT SophieGenesisCmdGenesisFileError
      . newExceptT
      $ writeFileTextEnvelope skeyPath (Just skeyDesc) skey
    firstExceptT SophieGenesisCmdGenesisFileError
      . newExceptT
      $ writeFileTextEnvelope vkeyPath (Just vkeyDesc) vkey
    firstExceptT SophieGenesisCmdGenesisFileError
      . newExceptT
      $ writeFileTextEnvelope ocertCtrPath (Just certCtrDesc)
      $ OperationalCertificateIssueCounter
          initialCounter
          (castVerificationKey vkey)  -- Cast to a 'StakePoolKey'
  where
    skeyDesc, vkeyDesc, certCtrDesc :: TextEnvelopeDescr
    skeyDesc = "Genesis delegate operator key"
    vkeyDesc = "Genesis delegate operator key"
    certCtrDesc = "Next certificate issue number: "
               <> fromString (show initialCounter)

    initialCounter :: Word64
    initialCounter = 0


runGenesisKeyGenDelegateVRF :: VerificationKeyFile -> SigningKeyFile
                            -> ExceptT SophieGenesisCmdError IO ()
runGenesisKeyGenDelegateVRF (VerificationKeyFile vkeyPath)
                            (SigningKeyFile skeyPath) = do
    skey <- liftIO $ generateSigningKey AsVrfKey
    let vkey = getVerificationKey skey
    firstExceptT SophieGenesisCmdGenesisFileError
      . newExceptT
      $ writeFileTextEnvelope skeyPath (Just skeyDesc) skey
    firstExceptT SophieGenesisCmdGenesisFileError
      . newExceptT
      $ writeFileTextEnvelope vkeyPath (Just vkeyDesc) vkey
  where
    skeyDesc, vkeyDesc :: TextEnvelopeDescr
    skeyDesc = "VRF Signing Key"
    vkeyDesc = "VRF Verification Key"

runGenesisKeyGenVested :: VerificationKeyFile -> SigningKeyFile
                        -> ExceptT SophieGenesisCmdError IO ()
runGenesisKeyGenVested (VerificationKeyFile vkeyPath)
                        (SigningKeyFile skeyPath) = do
    skey <- liftIO $ generateSigningKey AsGenesisVestedKey
    let vkey = getVerificationKey skey
    firstExceptT SophieGenesisCmdGenesisFileError
      . newExceptT
      $ writeFileTextEnvelope skeyPath (Just skeyDesc) skey
    firstExceptT SophieGenesisCmdGenesisFileError
      . newExceptT
      $ writeFileTextEnvelope vkeyPath (Just vkeyDesc) vkey
  where
    skeyDesc, vkeyDesc :: TextEnvelopeDescr
    skeyDesc = "Vested Signing Key"
    vkeyDesc = "Vested Verification Key"


runGenesisKeyGenVestedDelegate :: VerificationKeyFile
                         -> SigningKeyFile
                         -> OpCertCounterFile
                         -> ExceptT SophieGenesisCmdError IO ()
runGenesisKeyGenVestedDelegate (VerificationKeyFile vkeyPath)
                         (SigningKeyFile skeyPath)
                         (OpCertCounterFile ocertCtrPath) = do
    skey <- liftIO $ generateSigningKey AsGenesisVestedDelegateKey
    let vkey = getVerificationKey skey
    firstExceptT SophieGenesisCmdGenesisFileError
      . newExceptT
      $ writeFileTextEnvelope skeyPath (Just skeyDesc) skey
    firstExceptT SophieGenesisCmdGenesisFileError
      . newExceptT
      $ writeFileTextEnvelope vkeyPath (Just vkeyDesc) vkey
    firstExceptT SophieGenesisCmdGenesisFileError
      . newExceptT
      $ writeFileTextEnvelope ocertCtrPath (Just certCtrDesc)
      $ OperationalCertificateIssueCounter
          initialCounter
          (castVerificationKey vkey)  -- Cast to a 'StakePoolKey'
  where
    skeyDesc, vkeyDesc, certCtrDesc :: TextEnvelopeDescr
    skeyDesc = "Vested delegate operator key"
    vkeyDesc = "Vested delegate operator key"
    certCtrDesc = "Next certificate issue number: "
               <> fromString (show initialCounter)

    initialCounter :: Word64
    initialCounter = 0


runGenesisKeyGenVestedDelegateVRF :: VerificationKeyFile -> SigningKeyFile
                            -> ExceptT SophieGenesisCmdError IO ()
runGenesisKeyGenVestedDelegateVRF (VerificationKeyFile vkeyPath)
                            (SigningKeyFile skeyPath) = do
    skey <- liftIO $ generateSigningKey AsVrfKey
    let vkey = getVerificationKey skey
    firstExceptT SophieGenesisCmdGenesisFileError
      . newExceptT
      $ writeFileTextEnvelope skeyPath (Just skeyDesc) skey
    firstExceptT SophieGenesisCmdGenesisFileError
      . newExceptT
      $ writeFileTextEnvelope vkeyPath (Just vkeyDesc) vkey
  where
    skeyDesc, vkeyDesc :: TextEnvelopeDescr
    skeyDesc = "VRF Signing Key"
    vkeyDesc = "VRF Verification Key"

runGenesisKeyGenUTxO :: VerificationKeyFile -> SigningKeyFile
                     -> ExceptT SophieGenesisCmdError IO ()
runGenesisKeyGenUTxO (VerificationKeyFile vkeyPath)
                     (SigningKeyFile skeyPath) = do
    skey <- liftIO $ generateSigningKey AsGenesisUTxOKey
    let vkey = getVerificationKey skey
    firstExceptT SophieGenesisCmdGenesisFileError
      . newExceptT
      $ writeFileTextEnvelope skeyPath (Just skeyDesc) skey
    firstExceptT SophieGenesisCmdGenesisFileError
      . newExceptT
      $ writeFileTextEnvelope vkeyPath (Just vkeyDesc) vkey
  where
    skeyDesc, vkeyDesc :: TextEnvelopeDescr
    skeyDesc = "Genesis Initial UTxO Signing Key"
    vkeyDesc = "Genesis Initial UTxO Verification Key"


runGenesisKeyHash :: VerificationKeyFile -> ExceptT SophieGenesisCmdError IO ()
runGenesisKeyHash (VerificationKeyFile vkeyPath) = do
    vkey <- firstExceptT SophieGenesisCmdTextEnvReadFileError . newExceptT $
            readFileTextEnvelopeAnyOf
              [ FromSomeType (AsVerificationKey AsGenesisKey)
                             AGenesisKey
              , FromSomeType (AsVerificationKey AsGenesisDelegateKey)
                             AGenesisDelegateKey
              , FromSomeType (AsVerificationKey AsGenesisVestedKey)
                             AGenesisVestedKey
              , FromSomeType (AsVerificationKey AsGenesisVestedDelegateKey)
                             AGenesisVestedDelegateKey
              , FromSomeType (AsVerificationKey AsGenesisUTxOKey)
                             AGenesisUTxOKey
              ]
              vkeyPath
    liftIO $ BS.putStrLn (renderKeyHash vkey)
  where
    renderKeyHash :: SomeGenesisKey VerificationKey -> ByteString
    renderKeyHash (AGenesisKey         vk) = renderVerificationKeyHash vk
    renderKeyHash (AGenesisDelegateKey vk) = renderVerificationKeyHash vk
    renderKeyHash (AGenesisVestedKey         vk) = renderVerificationKeyHash vk
    renderKeyHash (AGenesisVestedDelegateKey vk) = renderVerificationKeyHash vk
    renderKeyHash (AGenesisUTxOKey     vk) = renderVerificationKeyHash vk

    renderVerificationKeyHash :: Key keyrole => VerificationKey keyrole -> ByteString
    renderVerificationKeyHash = serialiseToRawBytesHex
                              . verificationKeyHash


runGenesisVerKey :: VerificationKeyFile -> SigningKeyFile
                 -> ExceptT SophieGenesisCmdError IO ()
runGenesisVerKey (VerificationKeyFile vkeyPath) (SigningKeyFile skeyPath) = do
    skey <- firstExceptT SophieGenesisCmdTextEnvReadFileError . newExceptT $
            readFileTextEnvelopeAnyOf
              [ FromSomeType (AsSigningKey AsGenesisKey)
                             AGenesisKey
              , FromSomeType (AsSigningKey AsGenesisDelegateKey)
                             AGenesisDelegateKey
              , FromSomeType (AsSigningKey AsGenesisVestedKey)
                             AGenesisVestedKey
              , FromSomeType (AsSigningKey AsGenesisVestedDelegateKey)
                             AGenesisVestedDelegateKey 
              , FromSomeType (AsSigningKey AsGenesisUTxOKey)
                             AGenesisUTxOKey
              ]
              skeyPath

    let vkey :: SomeGenesisKey VerificationKey
        vkey = case skey of
          AGenesisKey               sk -> AGenesisKey               (getVerificationKey sk)
          AGenesisDelegateKey       sk -> AGenesisDelegateKey       (getVerificationKey sk)
          AGenesisVestedKey         sk -> AGenesisVestedKey         (getVerificationKey sk)
          AGenesisVestedDelegateKey sk -> AGenesisVestedDelegateKey (getVerificationKey sk)
          AGenesisUTxOKey           sk -> AGenesisUTxOKey           (getVerificationKey sk)

    firstExceptT SophieGenesisCmdGenesisFileError . newExceptT . liftIO $
      case vkey of
        AGenesisKey               vk -> writeFileTextEnvelope vkeyPath Nothing vk
        AGenesisDelegateKey       vk -> writeFileTextEnvelope vkeyPath Nothing vk
        AGenesisVestedKey         vk -> writeFileTextEnvelope vkeyPath Nothing vk
        AGenesisVestedDelegateKey vk -> writeFileTextEnvelope vkeyPath Nothing vk
        AGenesisUTxOKey           vk -> writeFileTextEnvelope vkeyPath Nothing vk

data SomeGenesisKey f
     = AGenesisKey               (f GenesisKey)
     | AGenesisDelegateKey       (f GenesisDelegateKey)
     | AGenesisVestedKey         (f GenesisVestedKey)
     | AGenesisVestedDelegateKey (f GenesisVestedDelegateKey)
     | AGenesisUTxOKey           (f GenesisUTxOKey)


runGenesisTxIn :: VerificationKeyFile -> NetworkId -> Maybe OutputFile
               -> ExceptT SophieGenesisCmdError IO ()
runGenesisTxIn (VerificationKeyFile vkeyPath) network mOutFile = do
    vkey <- firstExceptT SophieGenesisCmdTextEnvReadFileError . newExceptT $
            readFileTextEnvelope (AsVerificationKey AsGenesisUTxOKey) vkeyPath
    let txin = genesisUTxOPseudoTxIn network (verificationKeyHash vkey)
    liftIO $ writeOutput mOutFile (renderTxIn txin)


runGenesisAddr :: VerificationKeyFile -> NetworkId -> Maybe OutputFile
               -> ExceptT SophieGenesisCmdError IO ()
runGenesisAddr (VerificationKeyFile vkeyPath) network mOutFile = do
    vkey <- firstExceptT SophieGenesisCmdTextEnvReadFileError . newExceptT $
            readFileTextEnvelope (AsVerificationKey AsGenesisUTxOKey) vkeyPath
    let vkh  = verificationKeyHash (castVerificationKey vkey)
        addr = makeSophieAddress network (PaymentCredentialByKey vkh)
                                  NoStakeAddress
    liftIO $ writeOutput mOutFile (serialiseAddress addr)

writeOutput :: Maybe OutputFile -> Text -> IO ()
writeOutput (Just (OutputFile fpath)) = Text.writeFile fpath
writeOutput Nothing                   = Text.putStrLn


--
-- Create Genesis command implementation
--

runGenesisCreate :: GenesisDir
                 -> Word  -- ^ num genesis & delegate keys to make
                 -> Word  -- ^ num vested & vested delegate keys to make
                 -> Word  -- ^ num utxo keys to make
                 -> Maybe SystemStart
                 -> Maybe Entropic
                 -> NetworkId
                 -> ExceptT SophieGenesisCmdError IO ()
runGenesisCreate (GenesisDir rootdir)
                 genNumGenesisKeys genNumVestedKeys genNumUTxOKeys
                 mStart mAmount network = do
  liftIO $ do
    createDirectoryIfMissing False rootdir
    createDirectoryIfMissing False gendir
    createDirectoryIfMissing False deldir
    createDirectoryIfMissing False vesteddir
    createDirectoryIfMissing False vesteddeldir
    createDirectoryIfMissing False utxodir

  template <- readSophieGenesis (rootdir </> "genesis.spec.json") adjustTemplate

  forM_ [ 1 .. genNumGenesisKeys ] $ \index -> do
    createGenesisKeys  gendir  index
    createDelegateKeys deldir index
  
  forM_ [ 1 .. genNumVestedKeys ] $ \index -> do
    createVestedKeys  vesteddir  index
    createVestedDelegateKeys vesteddeldir index

  forM_ [ 1 .. genNumUTxOKeys ] $ \index ->
    createUtxoKeys utxodir index

  genDlgs <- readGenDelegsMap gendir deldir
  vestedDlgs <- readVestedDelegsMap vesteddir vesteddeldir
  utxoAddrs <- readInitialFundAddresses utxodir network
  start <- maybe (SystemStart <$> getCurrentTimePlus30) pure mStart

  let (sophieGenesis, aurumGenesis) =
        updateTemplate
          -- Sophie genesis parameters
          start genDlgs vestedDlgs mAmount utxoAddrs mempty (Entropic 0) [] [] template
          -- Aurum genesis parameters
          -- TODO aurum: parameterize these, don't just use defaults
          aurumGenesisDefaultEntropicPerUtxoWord
          aurumGenesisDefaultExecutionPrices
          aurumGenesisDefaultMaxTxExecutionUnits
          aurumGenesisDefaultMaxBlockExecutionUnits
          aurumGenesisDefaultMaxValueSize
          aurumGenesisDefaultCollateralPercent
          aurumGenesisDefaultMaxCollateralInputs

  writeFileGenesis (rootdir </> "genesis.json")        sophieGenesis
  writeFileGenesis (rootdir </> "genesis.aurum.json") aurumGenesis
  --TODO: rationalise the naming convention on these genesis json files.
  where
    adjustTemplate t = t { sgNetworkMagic = unNetworkMagic (toNetworkMagic network) }
    gendir  = rootdir </> "genesis-keys"
    deldir  = rootdir </> "delegate-keys"
    vesteddir  = rootdir </> "vested-keys"
    vesteddeldir  = rootdir </> "vesteddelegate-keys"
    utxodir = rootdir </> "utxo-keys"

runGenesisCreateStaked
  :: GenesisDir
  -> Word           -- ^ num genesis & delegate keys to make
  -> Word           -- ^ num vested & vesteddelegate keys to make
  -> Word           -- ^ num utxo keys to make
  -> Word           -- ^ num pools to make
  -> Word           -- ^ num delegators to make
  -> Maybe SystemStart
  -> Maybe Entropic -- ^ supply going to non-delegators
  -> Entropic       -- ^ supply going to delegators
  -> NetworkId
  -> Word           -- ^ bulk credential files to write
  -> Word           -- ^ pool credentials per bulk file
  -> Word           -- ^ num stuffed UTxO entries
  -> ExceptT SophieGenesisCmdError IO ()
runGenesisCreateStaked (GenesisDir rootdir)
                 genNumGenesisKeys genNumVestedKeys genNumUTxOKeys genNumPools genNumStDelegs
                 mStart mNonDlgAmount stDlgAmount network
                 bulkPoolCredFiles bulkPoolsPerFile numStuffedUtxo = do
  liftIO $ do
    createDirectoryIfMissing False rootdir
    createDirectoryIfMissing False gendir
    createDirectoryIfMissing False deldir
    createDirectoryIfMissing False vesteddir
    createDirectoryIfMissing False vesteddeldir
    createDirectoryIfMissing False pooldir
    createDirectoryIfMissing False stdeldir
    createDirectoryIfMissing False utxodir

  template <- readSophieGenesis (rootdir </> "genesis.spec.json") adjustTemplate

  forM_ [ 1 .. genNumGenesisKeys ] $ \index -> do
    createGenesisKeys  gendir  index
    createDelegateKeys deldir index
  
  forM_ [ 1 .. genNumVestedKeys ] $ \index -> do
    createVestedKeys  vesteddir  index
    createDelegateKeys vesteddeldir index

  forM_ [ 1 .. genNumUTxOKeys ] $ \index ->
    createUtxoKeys utxodir index

  pools <- forM [ 1 .. genNumPools ] $ \index -> do
    createPoolCredentials pooldir index
    buildPool network pooldir index

  when (bulkPoolCredFiles * bulkPoolsPerFile > genNumPools) $
    left $ SophieGenesisCmdTooFewPoolsForBulkCreds  genNumPools bulkPoolCredFiles bulkPoolsPerFile
  -- We generate the bulk files for the last pool indices,
  -- so that all the non-bulk pools have stable indices at beginning:
  let bulkOffset  = fromIntegral $ genNumPools - bulkPoolCredFiles * bulkPoolsPerFile
      bulkIndices :: [Word]   = [ 1 + bulkOffset .. genNumPools ]
      bulkSlices  :: [[Word]] = List.chunksOf (fromIntegral bulkPoolsPerFile) bulkIndices
  forM_ (zip [ 1 .. bulkPoolCredFiles ] bulkSlices) $
    uncurry (writeBulkPoolCredentials pooldir)

  forM_ [ 1 .. genNumStDelegs ] $ \index ->
    createDelegatorCredentials stdeldir index

  delegations :: [Delegation] <-
    -- Distribute M delegates across N pools:
    forM [ (pool, delegIx)
         | (pool, poolIx) <- zip pools [1 ..]
         , delegIxLocal <- [ 1 .. delegsPerPool ] ++
                           -- Add all remaining delegates to the last pool:
                           if delegsRemaining /= 0 && poolIx == genNumPools
                           then [ delegsPerPool + 1 .. delegsPerPool + delegsRemaining ]
                           else []
         , let delegIx = delegIxLocal + delegsPerPool * (poolIx - 1)] $
      uncurry (computeDelegation network stdeldir)

  genDlgs <- readGenDelegsMap gendir deldir
  vestedDlgs <- readVestedDelegsMap vesteddir vesteddeldir
  nonDelegAddrs <- readInitialFundAddresses utxodir network
  start <- maybe (SystemStart <$> getCurrentTimePlus30) pure mStart

  stuffedUtxoAddrs <- liftIO $ replicateM (fromIntegral numStuffedUtxo)
                      genStuffedAddress

  let poolMap :: Map (Ledger.KeyHash Ledger.Staking StandardCrypto) (Ledger.PoolParams StandardCrypto)
      poolMap = Map.fromList $ mkDelegationMapEntry <$> delegations
      delegAddrs = dInitialUtxoAddr <$> delegations
      (sophieGenesis, aurumGenesis) =
        updateTemplate
          -- Sophie genesis parameters
          start genDlgs vestedDlgs mNonDlgAmount nonDelegAddrs poolMap
          stDlgAmount delegAddrs stuffedUtxoAddrs template
          -- Aurum genesis parameters
          -- TODO aurum: parameterize these, don't just use defaults
          aurumGenesisDefaultEntropicPerUtxoWord
          aurumGenesisDefaultExecutionPrices
          aurumGenesisDefaultMaxTxExecutionUnits
          aurumGenesisDefaultMaxBlockExecutionUnits
          aurumGenesisDefaultMaxValueSize
          aurumGenesisDefaultCollateralPercent
          aurumGenesisDefaultMaxCollateralInputs

  writeFileGenesis (rootdir </> "genesis.json")        sophieGenesis
  writeFileGenesis (rootdir </> "genesis.aurum.json") aurumGenesis
  --TODO: rationalise the naming convention on these genesis json files.

  liftIO $ Text.putStrLn $ mconcat $
    [ "generated genesis with: "
    , textShow genNumGenesisKeys, " genesis keys, "
    , textShow genNumVestedKeys, " vested keys, "
    , textShow genNumUTxOKeys, " non-delegating UTxO keys, "
    , textShow genNumPools, " stake pools, "
    , textShow genNumStDelegs, " delegating UTxO keys, "
    , textShow (length delegations), " delegation relationships, "
    , textShow (Map.size poolMap), " delegation map entries, "
    , textShow (length delegAddrs), " delegating addresses"
    ] ++
    [ mconcat
      [ ", "
      , textShow bulkPoolCredFiles, " bulk pool credential files, "
      , textShow bulkPoolsPerFile, " pools per bulk credential file, indices starting from "
      , textShow bulkOffset, ", "
      , textShow $ length bulkIndices, " total pools in bulk nodes, each bulk node having this many entries: "
      , textShow $ length <$> bulkSlices
      ]
    | bulkPoolCredFiles * bulkPoolsPerFile > 0 ]

  where
    (,) delegsPerPool delegsRemaining = divMod genNumStDelegs genNumPools
    adjustTemplate t = t { sgNetworkMagic = unNetworkMagic (toNetworkMagic network) }
    mkDelegationMapEntry :: Delegation -> (Ledger.KeyHash Ledger.Staking StandardCrypto, Ledger.PoolParams StandardCrypto)
    mkDelegationMapEntry d = (dDelegStaking d, dPoolParams d)

    gendir   = rootdir </> "genesis-keys"
    deldir   = rootdir </> "delegate-keys"
    vesteddir = rootdir </> "vested-keys"
    vesteddeldir   = rootdir </> "vesteddelegate-keys"
    pooldir  = rootdir </> "pools"
    stdeldir = rootdir </> "stake-delegator-keys"
    utxodir  = rootdir </> "utxo-keys"

    genStuffedAddress :: IO (AddressInEra SophieEra)
    genStuffedAddress =
      sophieAddressInEra <$>
      (SophieAddress
       <$> pure Ledger.Testnet
       <*> (Ledger.KeyHashObj . mkKeyHash . read64BitInt
             <$> Crypto.runSecureRandom (getRandomBytes 8))
       <*> pure Ledger.StakeRefNull)

    read64BitInt :: ByteString -> Int
    read64BitInt = (fromIntegral :: Word64 -> Int)
      . Bin.runGet Bin.getWord64le . LBS.fromStrict

    mkDummyHash :: forall h a. HashAlgorithm h => Proxy h -> Int -> Hash.Hash h a
    mkDummyHash _ = coerce . Ledger.hashWithSerialiser @h toCBOR

    mkKeyHash :: forall c discriminator. Crypto c => Int -> Ledger.KeyHash discriminator c
    mkKeyHash = Ledger.KeyHash . mkDummyHash (Proxy @(ADDRHASH c))

-- -------------------------------------------------------------------------------------------------

createDelegateKeys :: FilePath -> Word -> ExceptT SophieGenesisCmdError IO ()
createDelegateKeys dir index = do
  liftIO $ createDirectoryIfMissing False dir
  runGenesisKeyGenDelegate
        (VerificationKeyFile $ dir </> "delegate" ++ strIndex ++ ".vkey")
        coldSK
        opCertCtr
  runGenesisKeyGenDelegateVRF
        (VerificationKeyFile $ dir </> "delegate" ++ strIndex ++ ".vrf.vkey")
        (SigningKeyFile $ dir </> "delegate" ++ strIndex ++ ".vrf.skey")
  firstExceptT SophieGenesisCmdNodeCmdError $ do
    runNodeKeyGenKES
        kesVK
        (SigningKeyFile $ dir </> "delegate" ++ strIndex ++ ".kes.skey")
    runNodeIssueOpCert
        (VerificationKeyFilePath kesVK)
        coldSK
        opCertCtr
        (KESPeriod 0)
        (OutputFile $ dir </> "opcert" ++ strIndex ++ ".cert")
 where
   strIndex = show index
   kesVK = VerificationKeyFile $ dir </> "delegate" ++ strIndex ++ ".kes.vkey"
   coldSK = SigningKeyFile $ dir </> "delegate" ++ strIndex ++ ".skey"
   opCertCtr = OpCertCounterFile $ dir </> "delegate" ++ strIndex ++ ".counter"

createVestedDelegateKeys :: FilePath -> Word -> ExceptT SophieGenesisCmdError IO ()
createVestedDelegateKeys dir index = do
  liftIO $ createDirectoryIfMissing False dir
  runGenesisKeyGenVestedDelegate
        (VerificationKeyFile $ dir </> "vested delegate" ++ strIndex ++ ".vkey")
        coldSK
        opCertCtr
  runGenesisKeyGenVestedDelegateVRF
        (VerificationKeyFile $ dir </> "vested delegate" ++ strIndex ++ ".vrf.vkey")
        (SigningKeyFile $ dir </> "vested delegate" ++ strIndex ++ ".vrf.skey")
  firstExceptT SophieGenesisCmdNodeCmdError $ do
    runNodeKeyGenKES
        kesVK
        (SigningKeyFile $ dir </> "vested delegate" ++ strIndex ++ ".kes.skey")
    runNodeIssueOpCert
        (VerificationKeyFilePath kesVK)
        coldSK
        opCertCtr
        (KESPeriod 0)
        (OutputFile $ dir </> "opcert" ++ strIndex ++ ".cert")
 where
   strIndex = show index
   kesVK = VerificationKeyFile $ dir </> "vested delegate" ++ strIndex ++ ".kes.vkey"
   coldSK = SigningKeyFile $ dir </> "vested delegate" ++ strIndex ++ ".skey"
   opCertCtr = OpCertCounterFile $ dir </> "vested delegate" ++ strIndex ++ ".counter"

createGenesisKeys :: FilePath -> Word -> ExceptT SophieGenesisCmdError IO ()
createGenesisKeys dir index = do
  liftIO $ createDirectoryIfMissing False dir
  let strIndex = show index
  runGenesisKeyGenGenesis
        (VerificationKeyFile $ dir </> "genesis" ++ strIndex ++ ".vkey")
        (SigningKeyFile $ dir </> "genesis" ++ strIndex ++ ".skey")

createVestedKeys :: FilePath -> Word -> ExceptT SophieGenesisCmdError IO ()
createVestedKeys dir index = do
  liftIO $ createDirectoryIfMissing False dir
  let strIndex = show index
  runGenesisKeyGenVested
        (VerificationKeyFile $ dir </> "vested" ++ strIndex ++ ".vkey")
        (SigningKeyFile $ dir </> "vested" ++ strIndex ++ ".skey")

createUtxoKeys :: FilePath -> Word -> ExceptT SophieGenesisCmdError IO ()
createUtxoKeys dir index = do
  liftIO $ createDirectoryIfMissing False dir
  let strIndex = show index
  runGenesisKeyGenUTxO
        (VerificationKeyFile $ dir </> "utxo" ++ strIndex ++ ".vkey")
        (SigningKeyFile $ dir </> "utxo" ++ strIndex ++ ".skey")

createPoolCredentials :: FilePath -> Word -> ExceptT SophieGenesisCmdError IO ()
createPoolCredentials dir index = do
  liftIO $ createDirectoryIfMissing False dir
  firstExceptT SophieGenesisCmdNodeCmdError $ do
    runNodeKeyGenKES
        kesVK
        (SigningKeyFile $ dir </> "kes" ++ strIndex ++ ".skey")
    runNodeKeyGenVRF
        (VerificationKeyFile $ dir </> "vrf" ++ strIndex ++ ".vkey")
        (SigningKeyFile $ dir </> "vrf" ++ strIndex ++ ".skey")
    runNodeKeyGenCold
        (VerificationKeyFile $ dir </> "cold" ++ strIndex ++ ".vkey")
        coldSK
        opCertCtr
    runNodeIssueOpCert
        (VerificationKeyFilePath kesVK)
        coldSK
        opCertCtr
        (KESPeriod 0)
        (OutputFile $ dir </> "opcert" ++ strIndex ++ ".cert")
  firstExceptT SophieGenesisCmdStakeAddressCmdError $
    runStakeAddressKeyGen
        (VerificationKeyFile $ dir </> "staking-reward" ++ strIndex ++ ".vkey")
        (SigningKeyFile $ dir </> "staking-reward" ++ strIndex ++ ".skey")
 where
   strIndex = show index
   kesVK = VerificationKeyFile $ dir </> "kes" ++ strIndex ++ ".vkey"
   coldSK = SigningKeyFile $ dir </> "cold" ++ strIndex ++ ".skey"
   opCertCtr = OpCertCounterFile $ dir </> "opcert" ++ strIndex ++ ".counter"

createDelegatorCredentials :: FilePath -> Word -> ExceptT SophieGenesisCmdError IO ()
createDelegatorCredentials dir index = do
  liftIO $ createDirectoryIfMissing False dir
  firstExceptT SophieGenesisCmdAddressCmdError $ do
    runAddressKeyGen
        AddressKeySophie
        addrVK
        (SigningKeyFile $ dir </> "payment" ++ strIndex ++ ".skey")
  firstExceptT SophieGenesisCmdStakeAddressCmdError $
    runStakeAddressKeyGen
        (VerificationKeyFile $ dir </> "staking" ++ strIndex ++ ".vkey")
        (SigningKeyFile $ dir </> "staking" ++ strIndex ++ ".skey")
 where
   strIndex = show index
   addrVK = VerificationKeyFile $ dir </> "payment" ++ strIndex ++ ".vkey"

data Delegation
  = Delegation
    { dInitialUtxoAddr  :: AddressInEra SophieEra
    , dDelegStaking     :: Ledger.KeyHash Ledger.Staking StandardCrypto
    , dPoolParams       :: Ledger.PoolParams StandardCrypto
    }

buildPool :: NetworkId -> FilePath -> Word -> ExceptT SophieGenesisCmdError IO (Ledger.PoolParams StandardCrypto)
buildPool nw dir index = do
    StakePoolVerificationKey poolColdVK <- firstExceptT (SophieGenesisCmdPoolCmdError
                                                         . SophiePoolCmdReadFileError)
      . newExceptT
      $ readFileTextEnvelope (AsVerificationKey AsStakePoolKey) poolColdVKF
    VrfVerificationKey poolVrfVK <- firstExceptT (SophieGenesisCmdNodeCmdError
                                                  . SophieNodeCmdReadFileError)
      . newExceptT
      $ readFileTextEnvelope (AsVerificationKey AsVrfKey) poolVrfVKF
    rewardsSVK <- firstExceptT SophieGenesisCmdTextEnvReadFileError
      . newExceptT
      $ readFileTextEnvelope (AsVerificationKey AsStakeKey) poolRewardVKF
    pure Ledger.PoolParams
      { Ledger._poolId     = Ledger.hashKey poolColdVK
      , Ledger._poolVrf    = Ledger.hashVerKeyVRF poolVrfVK
      , Ledger._poolPledge = Ledger.Coin 0
      , Ledger._poolCost   = Ledger.Coin 0
      , Ledger._poolMargin = minBound
      , Ledger._poolRAcnt  =
          toSophieStakeAddr $ makeStakeAddress nw $ StakeCredentialByKey (verificationKeyHash rewardsSVK)
      , Ledger._poolOwners = mempty
      , Ledger._poolRelays = Seq.empty
      , Ledger._poolMD     = Ledger.SNothing
      }
 where
   strIndex = show index
   poolColdVKF = dir </> "cold" ++ strIndex ++ ".vkey"
   poolVrfVKF = dir </> "vrf" ++ strIndex ++ ".vkey"
   poolRewardVKF = dir </> "staking-reward" ++ strIndex ++ ".vkey"

writeBulkPoolCredentials :: FilePath -> Word -> [Word] -> ExceptT SophieGenesisCmdError IO ()
writeBulkPoolCredentials dir bulkIx poolIxs = do
  creds <- mapM readPoolCreds poolIxs
  handleIOExceptT (SophieGenesisCmdFileError . FileIOError bulkFile) $
    LBS.writeFile bulkFile $ Aeson.encode creds
 where
   bulkFile = dir </> "bulk" ++ show bulkIx ++ ".creds"

   readPoolCreds :: Word -> ExceptT SophieGenesisCmdError IO
                                   (TextEnvelope, TextEnvelope, TextEnvelope)
   readPoolCreds ix = do
     (,,) <$> readEnvelope poolCert
          <*> readEnvelope poolVrfSKF
          <*> readEnvelope poolKesSKF
    where
      strIndex = show ix
      poolCert = dir </> "opcert" ++ strIndex ++ ".cert"
      poolVrfSKF = dir </> "vrf" ++ strIndex ++ ".skey"
      poolKesSKF = dir </> "kes" ++ strIndex ++ ".skey"
   readEnvelope :: FilePath -> ExceptT SophieGenesisCmdError IO TextEnvelope
   readEnvelope fp = do
     content <- handleIOExceptT (SophieGenesisCmdFileError . FileIOError fp) $
                  BS.readFile fp
     firstExceptT (SophieGenesisCmdAesonDecodeError fp . Text.pack) . hoistEither $
       Aeson.eitherDecodeStrict' content

computeDelegation :: NetworkId -> FilePath -> Ledger.PoolParams StandardCrypto -> Word -> ExceptT SophieGenesisCmdError IO Delegation
computeDelegation nw delegDir pool delegIx = do
    paySVK <- firstExceptT (SophieGenesisCmdAddressCmdError
                           . SophieAddressCmdVerificationKeyTextOrFileError) $
                 readAddressVerificationKeyTextOrFile
                   (VktofVerificationKeyFile payVKF)
    initialUtxoAddr <- case paySVK of
      APaymentVerificationKey payVK ->
        firstExceptT SophieGenesisCmdAddressCmdError
        $ buildSophieAddress payVK (Just . StakeVerifierKey . VerificationKeyFilePath . VerificationKeyFile $ stakeVKF) nw
      _ -> left $ SophieGenesisCmdUnexpectedAddressVerificationKey payVKF "APaymentVerificationKey" paySVK

    StakeVerificationKey stakeVK <- firstExceptT SophieGenesisCmdTextEnvReadFileError
      . newExceptT
      $ readFileTextEnvelope (AsVerificationKey AsStakeKey) stakeVKF

    pure Delegation
      { dInitialUtxoAddr = sophieAddressInEra initialUtxoAddr
      , dDelegStaking = Ledger.hashKey stakeVK
      , dPoolParams = pool
      }
 where
   strIndexDeleg = show delegIx
   payVKF = VerificationKeyFile $ delegDir </> "payment" ++ strIndexDeleg ++ ".vkey"
   stakeVKF = delegDir </> "staking" ++ strIndexDeleg ++ ".vkey"

-- | Current UTCTime plus 30 seconds
getCurrentTimePlus30 :: ExceptT SophieGenesisCmdError IO UTCTime
getCurrentTimePlus30 =
    plus30sec <$> liftIO getCurrentTime
  where
    plus30sec :: UTCTime -> UTCTime
    plus30sec = addUTCTime (30 :: NominalDiffTime)


readSophieGenesis
  :: FilePath
  -> (SophieGenesis StandardSophie -> SophieGenesis StandardSophie)
  -> ExceptT SophieGenesisCmdError IO (SophieGenesis StandardSophie)
readSophieGenesis fpath adjustDefaults = do
    readAndDecode
      `catchError` \err ->
        case err of
          SophieGenesisCmdGenesisFileError (FileIOError _ ioe)
            | isDoesNotExistError ioe -> writeDefault
          _                           -> left err
  where
    readAndDecode = do
      lbs <- handleIOExceptT (SophieGenesisCmdGenesisFileError . FileIOError fpath) $ LBS.readFile fpath
      firstExceptT (SophieGenesisCmdAesonDecodeError fpath . Text.pack)
        . hoistEither $ Aeson.eitherDecode' lbs

    defaults :: SophieGenesis StandardSophie
    defaults = adjustDefaults sophieGenesisDefaults

    writeDefault = do
      handleIOExceptT (SophieGenesisCmdGenesisFileError . FileIOError fpath) $
        LBS.writeFile fpath (encodePretty defaults)
      return defaults


updateTemplate
    :: SystemStart
    -- Genesis delegation (not stake-based):
    -> Map (Hash GenesisKey) (Hash GenesisDelegateKey, Hash VrfKey)
    -- Vested delegation (not stake-based):
    -> Map (Hash VestedKey)   (Hash VestedDelegateKey, Hash VrfKey)
    -- Non-delegated initial UTxO spec:
    -> Maybe Entropic
    -> [AddressInEra SophieEra]
    -- Genesis staking: pools/delegation map & delegated initial UTxO spec:
    -> Map (Ledger.KeyHash 'Ledger.Staking StandardCrypto) (Ledger.PoolParams StandardCrypto)
    -> Entropic
    -> [AddressInEra SophieEra]
    -> [AddressInEra SophieEra]
    -> SophieGenesis StandardSophie
    -- Aurum genesis parameters
    -> Entropic            -- ^ Bcc per UTxO word
    -> ExecutionUnitPrices -- ^ Execution prices (memory, steps)
    -> ExecutionUnits      -- ^ Max Tx execution units
    -> ExecutionUnits      -- ^ Max block execution units
    -> Natural             -- ^ Max value size
    -> Natural             -- ^ Collateral percentage
    -> Natural             -- ^ Max collateral inputs
    -> (SophieGenesis StandardSophie, Aurum.AurumGenesis)
updateTemplate (SystemStart start)
               genDelegMap vestedDelegMap mAmountNonDeleg utxoAddrsNonDeleg
               poolSpecs (Entropic amountDeleg) utxoAddrsDeleg stuffedUtxoAddrs
               template coinsPerUTxOWord prices maxTxExUnits maxBlockExUnits
               maxValueSize collateralPercentage maxCollateralInputs = do

    let sophieGenesis = template
          { sgSystemStart = start
          , sgMaxEntropicSupply = fromIntegral $ nonDelegCoin + delegCoin
          , sgGenDelegs = sophieDelKeys
          , sgVestedDelegs = sophieVestedDelKeys
          , sgInitialFunds = Map.fromList
                              [ (toSophieAddr addr, toSophieEntropic v)
                              | (addr, v) <-
                                distribute nonDelegCoin utxoAddrsNonDeleg ++
                                distribute delegCoin    utxoAddrsDeleg ++
                                mkStuffedUtxo stuffedUtxoAddrs ]
          , sgStaking =
            SophieGenesisStaking
              { sgsPools = Map.fromList
                            [ (Ledger._poolId poolParams, poolParams)
                            | poolParams <- Map.elems poolSpecs ]
              , sgsStake = Ledger._poolId <$> poolSpecs
              }
          }
        cModel = case Aurum.CostModel <$> defaultCostModelParams of
                   Just (Aurum.CostModel m) ->
                     if Aurum.validateCostModelParams m
                     then Map.singleton Aurum.ZerepochV1 (Aurum.CostModel m)
                     else panic "updateTemplate: defaultCostModel is invalid"

                   Nothing -> panic "updateTemplate: Could not extract cost model params from defaultCostModel"
        --TODO: we need a better validation story. We also ought to wrap the
        -- genesis type in the API properly.
        prices' = case toAurumPrices prices of
                    Nothing -> panic "updateTemplate: invalid prices"
                    Just p  -> p
        aurumGenesis = Aurum.AurumGenesis
          { Aurum.coinsPerUTxOWord     = toSophieEntropic coinsPerUTxOWord
          , Aurum.costmdls             = cModel
          , Aurum.prices               = prices'
          , Aurum.maxTxExUnits         = toAurumExUnits maxTxExUnits
          , Aurum.maxBlockExUnits      = toAurumExUnits maxBlockExUnits
          , Aurum.maxValSize           = maxValueSize
          , Aurum.collateralPercentage = collateralPercentage
          , Aurum.maxCollateralInputs  = maxCollateralInputs
          }
    (sophieGenesis, aurumGenesis)
  where
    nonDelegCoin, delegCoin :: Integer
    nonDelegCoin = fromIntegral $ fromMaybe (sgMaxEntropicSupply template) (unEntropic <$> mAmountNonDeleg)
    delegCoin = fromIntegral amountDeleg

    distribute :: Integer -> [AddressInEra SophieEra] -> [(AddressInEra SophieEra, Entropic)]
    distribute funds addrs =
      fst $ List.foldl' folder ([], fromIntegral funds) addrs
     where
       nAddrs, coinPerAddr, splitThreshold :: Integer
       nAddrs = fromIntegral $ length addrs
       coinPerAddr = funds `div` nAddrs
       splitThreshold = coinPerAddr + nAddrs

       folder :: ([(AddressInEra SophieEra, Entropic)], Integer)
              -> AddressInEra SophieEra
              -> ([(AddressInEra SophieEra, Entropic)], Integer)
       folder (acc, rest) addr
         | rest > splitThreshold =
             ((addr, Entropic coinPerAddr) : acc, rest - coinPerAddr)
         | otherwise = ((addr, Entropic rest) : acc, 0)

    mkStuffedUtxo :: [AddressInEra SophieEra] -> [(AddressInEra SophieEra, Entropic)]
    mkStuffedUtxo xs = (, Entropic minUtxoVal) <$> xs
      where (Coin minUtxoVal) = Sophie._minUTxOValue $ sgProtocolParams template

    sophieDelKeys =
      Map.fromList
        [ (gh, Ledger.GenDelegPair gdh h)
        | (GenesisKeyHash gh,
           (GenesisDelegateKeyHash gdh, VrfKeyHash h)) <- Map.toList genDelegMap
        ]
    sophieVestedDelKeys =
      Map.fromList
        [ (ah, Ledger.VestedDelegPair adh h)
        | (VestedKeyHash ah,
           (VestedDelegateKeyHash adh, VrfKeyHash h)) <- Map.toList vestedDelegMap
        ]

    unEntropic :: Integral a => Entropic -> a
    unEntropic (Entropic coin) = fromIntegral coin

writeFileGenesis
  :: ToJSON genesis
  => FilePath
  -> genesis
  -> ExceptT SophieGenesisCmdError IO ()
writeFileGenesis fpath genesis =
  handleIOExceptT (SophieGenesisCmdGenesisFileError . FileIOError fpath) $
    LBS.writeFile fpath (encodePretty genesis)

-- ----------------------------------------------------------------------------

readGenDelegsMap :: FilePath -> FilePath
                 -> ExceptT SophieGenesisCmdError IO
                            (Map (Hash GenesisKey)
                                 (Hash GenesisDelegateKey, Hash VrfKey))
readGenDelegsMap gendir deldir = do
    gkm <- readGenesisKeys gendir
    dkm <- readDelegateKeys deldir
    vkm <- readDelegateVrfKeys deldir

    let combinedMap :: Map Int (VerificationKey GenesisKey,
                                (VerificationKey GenesisDelegateKey,
                                 VerificationKey VrfKey))
        combinedMap =
          Map.intersectionWith (,)
            gkm
            (Map.intersectionWith (,)
               dkm vkm)

    -- All the maps should have an identical set of keys. Complain if not.
    let gkmExtra = gkm Map.\\ combinedMap
        dkmExtra = dkm Map.\\ combinedMap
        vkmExtra = vkm Map.\\ combinedMap
    unless (Map.null gkmExtra && Map.null dkmExtra && Map.null vkmExtra) $
      throwError $ SophieGenesisCmdMismatchedGenesisKeyFiles
                     (Map.keys gkm) (Map.keys dkm) (Map.keys vkm)

    let delegsMap :: Map (Hash GenesisKey)
                         (Hash GenesisDelegateKey, Hash VrfKey)
        delegsMap =
          Map.fromList [ (gh, (dh, vh))
                       | (g,(d,v)) <- Map.elems combinedMap
                       , let gh = verificationKeyHash g
                             dh = verificationKeyHash d
                             vh = verificationKeyHash v
                       ]

    pure delegsMap


readGenesisKeys :: FilePath -> ExceptT SophieGenesisCmdError IO
                                       (Map Int (VerificationKey GenesisKey))
readGenesisKeys gendir = do
  files <- liftIO (listDirectory gendir)
  fileIxs <- extractFileNameIndexes [ gendir </> file
                                    | file <- files
                                    , takeExtension file == ".vkey" ]
  firstExceptT SophieGenesisCmdTextEnvReadFileError $
    Map.fromList <$>
      sequence
        [ (,) ix <$> readKey file
        | (file, ix) <- fileIxs ]
  where
    readKey = newExceptT
              . readFileTextEnvelope (AsVerificationKey AsGenesisKey)

readDelegateKeys :: FilePath
                 -> ExceptT SophieGenesisCmdError IO
                            (Map Int (VerificationKey GenesisDelegateKey))
readDelegateKeys deldir = do
  files <- liftIO (listDirectory deldir)
  fileIxs <- extractFileNameIndexes [ deldir </> file
                                    | file <- files
                                    , takeExtensions file == ".vkey" ]
  firstExceptT SophieGenesisCmdTextEnvReadFileError $
    Map.fromList <$>
      sequence
        [ (,) ix <$> readKey file
        | (file, ix) <- fileIxs ]
  where
    readKey = newExceptT
            . readFileTextEnvelope (AsVerificationKey AsGenesisDelegateKey)

readDelegateVrfKeys :: FilePath -> ExceptT SophieGenesisCmdError IO
                                           (Map Int (VerificationKey VrfKey))
readDelegateVrfKeys deldir = do
  files <- liftIO (listDirectory deldir)
  fileIxs <- extractFileNameIndexes [ deldir </> file
                                    | file <- files
                                    , takeExtensions file == ".vrf.vkey" ]
  firstExceptT SophieGenesisCmdTextEnvReadFileError $
    Map.fromList <$>
      sequence
        [ (,) ix <$> readKey file
        | (file, ix) <- fileIxs ]
  where
    readKey = newExceptT
            . readFileTextEnvelope (AsVerificationKey AsVrfKey)

-- ----------------------------------------------------------------------------

readVestedDelegsMap :: FilePath -> FilePath
                 -> ExceptT SophieGenesisCmdError IO
                            (Map (Hash VestedKey)
                                 (Hash VestedDelegateKey, Hash VrfKey))
readVestedDelegsMap vesteddir vesteddeldir = do
    akm <- readVestedKeys vesteddir
    dkm <- readVestedDelegateKeys vesteddeldir
    vkm <- readVestedDelegateVrfKeys vesteddeldir

    let combinedMap :: Map Int (VerificationKey VestedKey,
                                (VerificationKey VestedDelegateKey,
                                 VerificationKey VrfKey))
        combinedMap =
          Map.intersectionWith (,)
            akm
            (Map.intersectionWith (,)
               dkm vkm)

    -- All the maps should have an identical set of keys. Complain if not.
    let akmExtra = akm Map.\\ combinedMap
        dkmExtra = dkm Map.\\ combinedMap
        vkmExtra = vkm Map.\\ combinedMap
    unless (Map.null akmExtra && Map.null dkmExtra && Map.null vkmExtra) $
      throwError $ SophieGenesisCmdMismatchedVestedKeyFiles
                     (Map.keys akm) (Map.keys dkm) (Map.keys vkm)

    let vestedDelegsMap :: Map (Hash VestedKey)
                         (Hash VestedDelegateKey, Hash VrfKey)
        vestedDelegsMap =
          Map.fromList [ (ah, (dh, vh))
                       | (a,(d,v)) <- Map.elems combinedMap
                       , let ah = verificationKeyHash a
                             dh = verificationKeyHash d
                             vh = verificationKeyHash v
                       ]

    pure vestedDelegsMap


readVestedKeys :: FilePath -> ExceptT SophieGenesisCmdError IO
                                       (Map Int (VerificationKey VestedKey))
readVestedKeys vesteddir = do
  files <- liftIO (listDirectory vesteddir)
  fileIxs <- extractFileNameIndexes [ vesteddir </> file
                                    | file <- files
                                    , takeExtension file == ".vkey" ]
  firstExceptT SophieGenesisCmdTextEnvReadFileError $
    Map.fromList <$>
      sequence
        [ (,) ix <$> readKey file
        | (file, ix) <- fileIxs ]
  where
    readKey = newExceptT
              . readFileTextEnvelope (AsVerificationKey AsVestedKey)

readVestedDelegateKeys :: FilePath
                 -> ExceptT SophieGenesisCmdError IO
                            (Map Int (VerificationKey VestedDelegateKey))
readVestedDelegateKeys vesteddeldir = do
  files <- liftIO (listDirectory vesteddeldir)
  fileIxs <- extractFileNameIndexes [ vesteddeldir </> file
                                    | file <- files
                                    , takeExtensions file == ".vkey" ]
  firstExceptT SophieGenesisCmdTextEnvReadFileError $
    Map.fromList <$>
      sequence
        [ (,) ix <$> readKey file
        | (file, ix) <- fileIxs ]
  where
    readKey = newExceptT
            . readFileTextEnvelope (AsVerificationKey AsVestedDelegateKey)

readVestedDelegateVrfKeys :: FilePath -> ExceptT SophieGenesisCmdError IO
                                           (Map Int (VerificationKey VrfKey))
readVestedDelegateVrfKeys vesteddeldir = do
  files <- liftIO (listDirectory vesteddeldir)
  fileIxs <- extractFileNameIndexes [ vesteddeldir </> file
                                    | file <- files
                                    , takeExtensions file == ".vrf.vkey" ]
  firstExceptT SophieGenesisCmdTextEnvReadFileError $
    Map.fromList <$>
      sequence
        [ (,) ix <$> readKey file
        | (file, ix) <- fileIxs ]
  where
    readKey = newExceptT
            . readFileTextEnvelope (AsVerificationKey AsVrfKey)


-- | The file path is of the form @"delegate-keys/delegate3.vkey"@.
-- This function reads the file and extracts the index (in this case 3).
--
extractFileNameIndex :: FilePath -> Maybe Int
extractFileNameIndex fp =
  case filter isDigit fp of
    [] -> Nothing
    xs -> readMaybe xs

extractFileNameIndexes :: [FilePath]
                       -> ExceptT SophieGenesisCmdError IO [(FilePath, Int)]
extractFileNameIndexes files = do
    case [ file | (file, Nothing) <- filesIxs ] of
      []     -> return ()
      files' -> throwError (SophieGenesisCmdFilesNoIndex files')
    case filter (\g -> length g > 1)
       . groupBy ((==) `on` snd)
       . sortBy (compare `on` snd)
       $ [ (file, ix) | (file, Just ix) <- filesIxs ] of
      [] -> return ()
      (g:_) -> throwError (SophieGenesisCmdFilesDupIndex (map fst g))

    return [ (file, ix) | (file, Just ix) <- filesIxs ]
  where
    filesIxs = [ (file, extractFileNameIndex file) | file <- files ]

readInitialFundAddresses :: FilePath -> NetworkId
                         -> ExceptT SophieGenesisCmdError IO [AddressInEra SophieEra]
readInitialFundAddresses utxodir nw = do
    files <- liftIO (listDirectory utxodir)
    vkeys <- firstExceptT SophieGenesisCmdTextEnvReadFileError $
               sequence
                 [ newExceptT $
                     readFileTextEnvelope (AsVerificationKey AsGenesisUTxOKey)
                                          (utxodir </> file)
                 | file <- files
                 , takeExtension file == ".vkey" ]
    return [ addr | vkey <- vkeys
           , let vkh  = verificationKeyHash (castVerificationKey vkey)
                 addr = makeSophieAddressInEra nw (PaymentCredentialByKey vkh)
                                                NoStakeAddress
           ]


-- | Hash a genesis file
runGenesisHashFile :: GenesisFile -> ExceptT SophieGenesisCmdError IO ()
runGenesisHashFile (GenesisFile fpath) = do
   content <- handleIOExceptT (SophieGenesisCmdGenesisFileError . FileIOError fpath) $
              BS.readFile fpath
   let gh :: Crypto.Hash Crypto.Blake2b_256 ByteString
       gh = Crypto.hashWith id content
   liftIO $ Text.putStrLn (Crypto.hashToTextAsHex gh)

--
-- Aurum genesis
--


aurumGenesisDefaultEntropicPerUtxoWord :: Entropic
aurumGenesisDefaultEntropicPerUtxoWord = Entropic 1

aurumGenesisDefaultExecutionPrices :: ExecutionUnitPrices
aurumGenesisDefaultExecutionPrices =
    ExecutionUnitPrices {
       priceExecutionSteps  = 1 % 10,
       priceExecutionMemory = 1 % 10
    }

aurumGenesisDefaultMaxTxExecutionUnits :: ExecutionUnits
aurumGenesisDefaultMaxTxExecutionUnits =
    ExecutionUnits {
      executionSteps  = 500_000_000_000,
      executionMemory = 500_000_000_000
    }

aurumGenesisDefaultMaxBlockExecutionUnits :: ExecutionUnits
aurumGenesisDefaultMaxBlockExecutionUnits =
    ExecutionUnits {
      executionSteps  = 500_000_000_000,
      executionMemory = 500_000_000_000
    }

aurumGenesisDefaultMaxValueSize :: Natural
aurumGenesisDefaultMaxValueSize = 4000

aurumGenesisDefaultCollateralPercent :: Natural
aurumGenesisDefaultCollateralPercent = 1 --TODO change to 100%

aurumGenesisDefaultMaxCollateralInputs :: Natural
aurumGenesisDefaultMaxCollateralInputs = 5


readAurumGenesis
  :: FilePath
  -> ExceptT SophieGenesisCmdError IO Aurum.AurumGenesis
readAurumGenesis fpath = do
  readAndDecode
    `catchError` \err ->
      case err of
        SophieGenesisCmdGenesisFileError (FileIOError _ ioe)
          | isDoesNotExistError ioe -> panic "Sophie genesis file not found."
        _                           -> left err

 where
  readAndDecode :: ExceptT SophieGenesisCmdError IO Aurum.AurumGenesis
  readAndDecode = do
      lbs <- handleIOExceptT (SophieGenesisCmdGenesisFileError . FileIOError fpath) $ LBS.readFile fpath
      firstExceptT (SophieGenesisCmdAesonDecodeError fpath . Text.pack)
        . hoistEither $ Aeson.eitherDecode' lbs
