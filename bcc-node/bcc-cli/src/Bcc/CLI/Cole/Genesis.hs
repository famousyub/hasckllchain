{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

module Bcc.CLI.Cole.Genesis
  ( ColeGenesisError(..)
  , GenesisParameters(..)
  , NewDirectory(..)
  , dumpGenesis
  , mkGenesis
  , readGenesis
  , renderColeGenesisError
  )
where

import           Bcc.Prelude hiding (option, show, trace)
import           Prelude (String)

import           Control.Monad.Trans.Except.Extra (firstExceptT, left, right)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map.Strict as Map
import           Data.Text.Lazy.Builder (toLazyText)
import           Data.Time (UTCTime)
import           Formatting.Buildable
import           Text.Printf (printf)

import           System.Directory (createDirectory, doesPathExist)
import           System.FilePath ((</>))
#ifdef UNIX
import           System.Posix.Files (ownerReadMode, setFileMode)
#else
import           System.Directory (emptyPermissions, readable, setPermissions)
#endif
import           Bcc.Api (Key (..), NetworkId)
import           Bcc.Api.Cole (ColeKey, SerialiseAsRawBytes (..), SigningKey (..),
                     toColeRequiresNetworkMagic)

import qualified Bcc.Chain.Common as Common
import           Bcc.Chain.Delegation hiding (Map, epoch)
import           Bcc.Chain.Genesis (GeneratedSecrets (..))
import qualified Bcc.Chain.Genesis as Genesis
import qualified Bcc.Chain.UTxO as UTxO

import qualified Bcc.Crypto as Crypto

import           Bcc.CLI.Cole.Delegation
import           Bcc.CLI.Cole.Key
import           Bcc.CLI.Helpers (textShow)
import           Bcc.CLI.Types (GenesisFile (..))

data ColeGenesisError
  = ColeDelegationCertSerializationError !ColeDelegationError
  | ColeDelegationKeySerializationError ColeDelegationError
  | GenesisGenerationError !Genesis.GenesisDataGenerationError
  | GenesisOutputDirAlreadyExists FilePath
  | GenesisReadError !FilePath !Genesis.GenesisDataError
  | GenesisSpecError !Text
  | MakeGenesisDelegationError !Genesis.GenesisDelegationError
  | NoGenesisDelegationForKey !Text
  | ProtocolParametersParseFailed !FilePath !Text
  | PoorKeyFailure !ColeKeyFailure

  deriving Show

renderColeGenesisError :: ColeGenesisError -> Text
renderColeGenesisError err =
  case err of
    ProtocolParametersParseFailed pParamFp parseError ->
      "Protocol parameters parse failed at: " <> textShow pParamFp <> " Error: " <> parseError
    ColeDelegationCertSerializationError bDelegSerErr ->
      "Error while serializing the delegation certificate: " <> textShow bDelegSerErr
    ColeDelegationKeySerializationError bKeySerErr ->
      "Error while serializing the delegation key: " <> textShow bKeySerErr
    PoorKeyFailure bKeyFailure ->
      "Error creating poor keys: " <> textShow bKeyFailure
    MakeGenesisDelegationError genDelegError ->
      "Error creating genesis delegation: " <> textShow genDelegError
    GenesisGenerationError genDataGenError ->
      "Error generating genesis: " <> textShow genDataGenError
    GenesisOutputDirAlreadyExists genOutDir ->
      "Genesis output directory already exists: " <> textShow genOutDir
    GenesisReadError genFp genDataError ->
      "Error while reading genesis file at: " <> textShow genFp <> " Error: " <> textShow genDataError
    GenesisSpecError genSpecError ->
      "Error while creating genesis spec" <> textShow genSpecError
    NoGenesisDelegationForKey verKey ->
      "Error while creating genesis, no delegation certificate for this verification key:" <> textShow verKey

newtype NewDirectory =
  NewDirectory FilePath
  deriving (Eq, Ord, Show, IsString)

-- | Parameters required for generation of new genesis.
data GenesisParameters = GenesisParameters
  { gpStartTime :: !UTCTime
  , gpProtocolParamsFile :: !FilePath
  , gpK :: !Common.BlockCount
  , gpProtocolMagic :: !Crypto.ProtocolMagic
  , gpTestnetBalance :: !Genesis.TestnetBalanceOptions
  , gpFakeAvvmOptions :: !Genesis.FakeAvvmOptions
  , gpAvvmBalanceFactor :: !Common.EntropicPortion
  , gpSeed :: !(Maybe Integer)
  } deriving Show


mkGenesisSpec :: GenesisParameters -> ExceptT ColeGenesisError IO Genesis.GenesisSpec
mkGenesisSpec gp = do
  protoParamsRaw <- lift . LB.readFile $ gpProtocolParamsFile gp

  protocolParameters <- withExceptT
    (ProtocolParametersParseFailed (gpProtocolParamsFile gp)) $
    ExceptT . pure $ canonicalDecodePretty protoParamsRaw

  -- We're relying on the generator to fake AVVM and delegation.
  genesisDelegation <- withExceptT MakeGenesisDelegationError $
    Genesis.mkGenesisDelegation []

  withExceptT GenesisSpecError $
    ExceptT . pure $ Genesis.mkGenesisSpec
      (Genesis.GenesisAvvmBalances mempty)
      genesisDelegation
      protocolParameters
      (gpK gp)
      (gpProtocolMagic gp)
      (mkGenesisInitialiser True)

  where
    mkGenesisInitialiser :: Bool -> Genesis.GenesisInitializer
    mkGenesisInitialiser useHeavyDlg =
      Genesis.GenesisInitializer
      (gpTestnetBalance gp)
      (gpFakeAvvmOptions gp)
      (Common.entropicPortionToRational (gpAvvmBalanceFactor gp))
      useHeavyDlg

-- | Generate a genesis, for given blockchain start time, protocol parameters,
-- security parameter, protocol magic, testnet balance options, fake AVVM options,
-- AVVM balance factor and seed.  Throw an error in the following cases: if the
-- protocol parameters file can't be read or fails parse, if genesis delegation
-- couldn't be generated, if the parameter-derived genesis specification is wrong,
-- or if the genesis fails generation.
mkGenesis
  :: GenesisParameters
  -> ExceptT ColeGenesisError IO (Genesis.GenesisData, Genesis.GeneratedSecrets)
mkGenesis gp = do
  genesisSpec <- mkGenesisSpec gp

  withExceptT GenesisGenerationError $
    Genesis.generateGenesisData (gpStartTime gp) genesisSpec

-- | Read genesis from a file.
readGenesis :: GenesisFile
            -> NetworkId
            -> ExceptT ColeGenesisError IO Genesis.Config
readGenesis (GenesisFile file) nw =
  firstExceptT (GenesisReadError file) $ do
    (genesisData, genesisHash) <- Genesis.readGenesisData file
    return Genesis.Config {
      Genesis.configGenesisData       = genesisData,
      Genesis.configGenesisHash       = genesisHash,
      Genesis.configReqNetMagic       = toColeRequiresNetworkMagic nw,
      Genesis.configUTxOConfiguration = UTxO.defaultUTxOConfiguration
    }

-- | Write out genesis into a directory that must not yet exist.  An error is
-- thrown if the directory already exists, or the genesis has delegate keys that
-- are not delegated to.
dumpGenesis
  :: NewDirectory
  -> Genesis.GenesisData
  -> Genesis.GeneratedSecrets
  -> ExceptT ColeGenesisError IO ()
dumpGenesis (NewDirectory outDir) genesisData gs = do
  exists <- liftIO $ doesPathExist outDir
  if exists
  then left $ GenesisOutputDirAlreadyExists outDir
  else liftIO $ createDirectory outDir
  liftIO $ LB.writeFile genesisJSONFile (canonicalEncodePretty genesisData)

  dlgCerts <- mapM findDelegateCert . map ColeSigningKey $ gsRichSecrets gs

  liftIO $ wOut "genesis-keys" "key"
                serialiseToRawBytes
                (map ColeSigningKey $ gsDlgIssuersSecrets gs)
  liftIO $ wOut "delegate-keys" "key"
                serialiseToRawBytes
                (map ColeSigningKey $ gsRichSecrets gs)
  liftIO $ wOut "poor-keys" "key"
                serialiseToRawBytes
                (map (ColeSigningKey . Genesis.poorSecretToKey) $ gsPoorSecrets gs)
  liftIO $ wOut "delegation-cert" "json" serialiseDelegationCert dlgCerts
  liftIO $ wOut "avvm-secrets" "secret" printFakeAvvmSecrets $ gsFakeAvvmSecrets gs
 where
  dlgCertMap :: Map Common.KeyHash Certificate
  dlgCertMap = Genesis.unGenesisDelegation $ Genesis.gdHeavyDelegation genesisData

  findDelegateCert :: SigningKey ColeKey -> ExceptT ColeGenesisError IO Certificate
  findDelegateCert bSkey@(ColeSigningKey sk) =
    case find (isCertForSK sk) (Map.elems dlgCertMap) of
      Nothing -> left . NoGenesisDelegationForKey
                 . prettyPublicKey $ getVerificationKey bSkey
      Just x  -> right x

  genesisJSONFile :: FilePath
  genesisJSONFile = outDir <> "/genesis.json"

  printFakeAvvmSecrets :: Crypto.RedeemSigningKey -> ByteString
  printFakeAvvmSecrets rskey = encodeUtf8 . toStrict . toLazyText $ build rskey

  -- Compare a given 'SigningKey' with a 'Certificate' 'VerificationKey'
  isCertForSK :: Crypto.SigningKey -> Certificate -> Bool
  isCertForSK sk cert = delegateVK cert == Crypto.toVerification sk

  wOut :: String -> String -> (a -> ByteString) -> [a] -> IO ()
  wOut = writeSecrets outDir

writeSecrets :: FilePath -> String -> String -> (a -> ByteString) -> [a] -> IO ()
writeSecrets outDir prefix suffix secretOp xs =
  forM_ (zip xs [0::Int ..]) $
  \(secret, nr)-> do
    let filename = outDir </> prefix <> "." <> printf "%03d" nr <> "." <> suffix
    BS.writeFile filename $ secretOp secret
#ifdef UNIX
    setFileMode    filename ownerReadMode
#else
    setPermissions filename (emptyPermissions {readable = True})
#endif
