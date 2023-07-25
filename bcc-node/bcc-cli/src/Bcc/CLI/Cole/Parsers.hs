module Bcc.CLI.Cole.Parsers
  ( ColeCommand(..)
  , NodeCmd(..)
  , backwardsCompatibilityCommands
  , parseColeCommands
  , parseHeavyDelThd
  , parseInstallerHash
  , parseMaxBlockSize
  , parseMaxHeaderSize
  , parseMaxTxSize
  , parseMaxProposalSize
  , parseMpcThd
  , parseScriptVersion
  , parseSlotDuration
  , parseSoftforkRule
  , parseSystemTag
  , parseTxFeePolicy
  , parseUpdateProposalThd
  , parseUpdateProposalTTL
  , parseUnlockStakeEpoch
  , parseUpdateVoteThd
  ) where

import           Bcc.Prelude hiding (option)
import           Prelude (String)

import           Control.Monad (fail)
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import           Data.Attoparsec.Combinator ((<?>))
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Char as Char
import qualified Data.Text as Text
import           Data.Time (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Formatting (build, sformat)

import           Options.Applicative
import qualified Options.Applicative as Opt

import           Bcc.Binary (Annotated (..))

import           Bcc.Crypto (RequiresNetworkMagic (..))
import           Bcc.Crypto.Hashing (hashRaw)
import           Bcc.Crypto.ProtocolMagic (AProtocolMagic (..), ProtocolMagic,
                   ProtocolMagicId (..))

import           Bcc.Chain.Common (BlockCount (..), TxFeePolicy (..), TxSizeLinear (..),
                   decodeAddressBase58, rationalToEntropicPortion)
import qualified Bcc.Chain.Common as Cole
import           Bcc.Chain.Genesis (FakeAvvmOptions (..), TestnetBalanceOptions (..))
import           Bcc.Chain.Slotting (EpochNumber (..), SlotNumber (..))
import           Bcc.Chain.Update (ApplicationName (..), InstallerHash (..), NumSoftwareVersion,
                   ProtocolVersion (..), SoftforkRule (..), SoftwareVersion (..), SystemTag (..),
                   checkApplicationName, checkSystemTag)

import           Bcc.Api hiding (UpdateProposal, GenesisParameters)
import           Bcc.Api.Cole (Address (..), ColeProtocolParametersUpdate (..), Entropic (..),
                   toColeEntropic)

import           Bcc.CLI.Cole.Commands
import           Bcc.CLI.Cole.Genesis
import           Bcc.CLI.Cole.Key
import           Bcc.CLI.Cole.Tx
import           Bcc.CLI.Run (ClientCommand (ColeCommand))
import           Bcc.CLI.Sophie.Commands (ColeKeyFormat (..))
import           Bcc.CLI.Types

command' :: String -> String -> Parser a -> Mod CommandFields a
command' c descr p =
    command c $ info (p <**> helper)
              $ mconcat [ progDesc descr ]

backwardsCompatibilityCommands :: Parser ClientCommand
backwardsCompatibilityCommands =
  asum hiddenCmds
 where
  convertToColeCommand :: Mod CommandFields ColeCommand -> Parser ClientCommand
  convertToColeCommand p = ColeCommand <$> Opt.subparser (p <> Opt.internal)

  hiddenCmds :: [Parser ClientCommand]
  hiddenCmds = map convertToColeCommand [ parseGenesisRelatedValues
                                         , parseKeyRelatedValues
                                         , parseTxRelatedValues
                                         , parseLocalNodeQueryValues
                                         , parseMiscellaneous
                                         ]

-- Implemented with asum so all commands don't get hidden when trying to hide
-- the 'pNodeCmdBackwardCompatible' parser.
parseColeCommands :: Parser ColeCommand
parseColeCommands = asum
  [ subParser "key" (Opt.info (Opt.subparser parseKeyRelatedValues)
      $ Opt.progDesc "Cole key utility commands")
  , subParser "transaction" (Opt.info (Opt.subparser parseTxRelatedValues)
      $ Opt.progDesc "Cole transaction commands")
  , subParser "query" (Opt.info (Opt.subparser parseLocalNodeQueryValues)
      $ Opt.progDesc "Cole node query commands.")
  , subParser "genesis" (Opt.info (Opt.subparser parseGenesisRelatedValues)
      $ Opt.progDesc "Cole genesis block commands")
  , subParser "governance" (Opt.info (NodeCmd <$> Opt.subparser pNodeCmd)
      $ Opt.progDesc "Cole governance commands")
  , subParser "miscellaneous" (Opt.info (Opt.subparser parseMiscellaneous)
      $ Opt.progDesc "Cole miscellaneous commands")
  , NodeCmd <$> pNodeCmdBackwardCompatible
  ]
 where
   subParser :: String -> ParserInfo ColeCommand -> Parser ColeCommand
   subParser name pInfo = Opt.subparser $ Opt.command name pInfo <> Opt.metavar name

pNodeCmdBackwardCompatible :: Parser NodeCmd
pNodeCmdBackwardCompatible = Opt.subparser $ pNodeCmd <> Opt.internal

parseCBORObject :: Parser CBORObject
parseCBORObject = asum
  [ CBORBlockCole <$> option auto
      (  long "cole-block"
      <> help
          (   "The CBOR file is a cole era block."
          <>  " Enter the number of slots in an epoch. The default value is 21600")
      <> metavar "INT"
      <> value (EpochSlots 21600)
      )

  , flag' CBORDelegationCertificateCole $
        long "cole-delegation-certificate"
     <> help "The CBOR file is a cole era delegation certificate"

  , flag' CBORTxCole $
        long "cole-tx"
     <> help "The CBOR file is a cole era tx"

  , flag' CBORUpdateProposalCole $
        long "cole-update-proposal"
     <> help "The CBOR file is a cole era update proposal"
  , flag' CBORVoteCole $
        long "cole-vote"
     <> help "The CBOR file is a cole era vote"
  ]

-- | Values required to create genesis.
parseGenesisParameters :: Parser GenesisParameters
parseGenesisParameters =
  GenesisParameters
    <$> parseUTCTime
          "start-time"
          "Start time of the new cluster to be enshrined in the new genesis."
    <*> parseFilePath
          "protocol-parameters-file"
          "JSON file with protocol parameters."
    <*> parseK
    <*> parseProtocolMagic
    <*> parseTestnetBalanceOptions
    <*> parseFakeAvvmOptions
    <*> (rationalToEntropicPortion <$>
         parseFractionWithDefault
          "avvm-balance-factor"
          "AVVM balances will be multiplied by this factor (defaults to 1)."
          1)
    <*> optional
        ( parseIntegral
            "secret-seed"
            "Optionally specify the seed of generation."
        )

parseGenesisRelatedValues :: Mod CommandFields ColeCommand
parseGenesisRelatedValues =
  mconcat
    [ command' "genesis" "Create genesis."
      $ Genesis
          <$> parseNewDirectory
              "genesis-output-dir"
              "Non-existent directory where genesis JSON file and secrets shall be placed."
          <*> parseGenesisParameters
    , command' "print-genesis-hash" "Compute hash of a genesis file."
        $ PrintGenesisHash
            <$> parseGenesisFile "genesis-json"
    ]

-- | Values required to create keys and perform
-- transformation on keys.
parseKeyRelatedValues :: Mod CommandFields ColeCommand
parseKeyRelatedValues =
    mconcat
        [ command' "keygen" "Generate a signing key."
            $ Keygen
                <$> parseNewSigningKeyFile "secret"
        , command'
            "to-verification"
            "Extract a verification key in its base64 form."
            $ ToVerification
                <$> parseColeKeyFormat
                <*> parseSigningKeyFile
                      "secret"
                      "Signing key file to extract the verification part from."
                <*> parseNewVerificationKeyFile "to"
        , command'
            "signing-key-public"
            "Pretty-print a signing key's verification key (not a secret)."
            $ PrettySigningKeyPublic
                <$> parseColeKeyFormat
                <*> parseSigningKeyFile
                      "secret"
                      "Signing key to pretty-print."
        , command'
            "signing-key-address"
            "Print address of a signing key."
            $ PrintSigningKeyAddress
                <$> parseColeKeyFormat
                <*> pNetworkId
                <*> parseSigningKeyFile
                      "secret"
                      "Signing key, whose address is to be printed."
        , command'
            "migrate-delegate-key-from"
            "Migrate a delegate key from an older version."
            $ MigrateDelegateKeyFrom
                <$> parseSigningKeyFile "from" "Legacy signing key file to migrate."
                <*> parseNewSigningKeyFile "to"
        ]

parseLocalNodeQueryValues :: Mod CommandFields ColeCommand
parseLocalNodeQueryValues =
    mconcat
        [ command' "get-tip" "Get the tip of your local node's blockchain"
            $ GetLocalNodeTip
                <$> pNetworkId
        ]

parseMiscellaneous :: Mod CommandFields ColeCommand
parseMiscellaneous = mconcat
  [ command'
      "validate-cbor"
      "Validate a CBOR blockchain object."
      $ ValidateCBOR
          <$> parseCBORObject
          <*> parseFilePath "filepath" "Filepath of CBOR file."
  , command'
      "pretty-print-cbor"
      "Pretty print a CBOR file."
      $ PrettyPrintCBOR
          <$> parseFilePath "filepath" "Filepath of CBOR file."
  ]



parseTestnetBalanceOptions :: Parser TestnetBalanceOptions
parseTestnetBalanceOptions =
  TestnetBalanceOptions
    <$> parseIntegral
          "n-poor-addresses"
          "Number of poor nodes (with small balance)."
    <*> parseIntegral
          "n-delegate-addresses"
          "Number of delegate nodes (with huge balance)."
    <*> parseEntropic
          "total-balance"
          "Total balance owned by these nodes."
    <*> parseFraction
          "delegate-share"
          "Portion of stake owned by all delegates together."

parseTxIn :: Parser TxIn
parseTxIn =
  option
  (readerFromAttoParser parseTxInAtto)
  $ long "txin"
    <> metavar "(TXID,INDEX)"
    <> help "Transaction input is a pair of an UTxO TxId and a zero-based output index."

parseTxInAtto :: Atto.Parser TxIn
parseTxInAtto =
  TxIn <$> (Atto.char '(' *> parseTxIdAtto <* Atto.char ',')
       <*> (parseTxIxAtto <* Atto.char ')')


parseTxIdAtto :: Atto.Parser TxId
parseTxIdAtto = (<?> "Transaction ID (hexadecimal)") $ do
  bstr <- Atto.takeWhile1 Char.isHexDigit
  case deserialiseFromRawBytesHex AsTxId bstr of
    Just addr -> return addr
    Nothing -> fail $ "Incorrect transaction id format:: " ++ show bstr

parseTxIxAtto :: Atto.Parser TxIx
parseTxIxAtto = toEnum <$> Atto.decimal

parseTxOut :: Parser (TxOut ColeEra)
parseTxOut =
  option
    ( (\(addr, entropic) -> TxOut (pAddressInEra addr)
                                  (pEntropicTxOut entropic)
                                  TxOutDatumHashNone)
      <$> auto
    )
    $ long "txout"
      <> metavar "'(\"ADDR\", ENTROPIC)'"
      <> help "Specify a transaction output, as a pair of an address and entropic."
 where
  pAddressInEra :: Text -> AddressInEra ColeEra
  pAddressInEra t =
    case decodeAddressBase58 t of
      Left err -> panic $ "Bad Base58 address: " <> Text.pack (show err)
      Right coleAddress -> AddressInEra ColeAddressInAnyEra $ ColeAddress coleAddress

  pEntropicTxOut :: Word64 -> TxOutValue ColeEra
  pEntropicTxOut l =
    if l > (maxBound :: Word64)
    then panic $ show l <> " entropic exceeds the Word64 upper bound"
    else TxOutBccOnly BccOnlyInColeEra . Entropic $ toInteger l

readerFromAttoParser :: Atto.Parser a -> Opt.ReadM a
readerFromAttoParser p =
  Opt.eitherReader (Atto.parseOnly (p <* Atto.endOfInput) . BSC.pack)

parseTxRelatedValues :: Mod CommandFields ColeCommand
parseTxRelatedValues =
  mconcat
    [ command'
        "submit-tx"
        "Submit a raw, signed transaction, in its on-wire representation."
        $ SubmitTx
            <$> pNetworkId
            <*> parseTxFile "tx"
    , command'
        "issue-genesis-utxo-expenditure"
        "Write a file with a signed transaction, spending genesis UTxO."
        $ SpendGenesisUTxO
            <$> parseGenesisFile "genesis-json"
            <*> pNetworkId
            <*> parseColeKeyFormat
            <*> parseNewTxFile "tx"
            <*> parseSigningKeyFile
                  "wallet-key"
                  "Key that has access to all mentioned genesis UTxO inputs."
            <*> parseAddress
                  "rich-addr-from"
                  "Tx source: genesis UTxO richman address (non-HD)."
            <*> some parseTxOut

    , command'
        "issue-utxo-expenditure"
        "Write a file with a signed transaction, spending normal UTxO."
        $ SpendUTxO
            <$> pNetworkId
            <*> parseColeKeyFormat
            <*> parseNewTxFile "tx"
            <*> parseSigningKeyFile
                  "wallet-key"
                  "Key that has access to all mentioned genesis UTxO inputs."
            <*> some parseTxIn
            <*> some parseTxOut

    , command'
        "txid"
        "Print the txid of a raw, signed transaction."
        $ GetTxId
            <$> parseTxFile "tx"
    ]

pNodeCmd :: Mod CommandFields NodeCmd
pNodeCmd =
    mconcat
      [ Opt.command "create-update-proposal"
          (Opt.info parseColeUpdateProposal $ Opt.progDesc  "Create an update proposal.")

      , Opt.command "create-proposal-vote"
          (Opt.info parseColeVote $ Opt.progDesc "Create an update proposal vote.")

      , Opt.command "submit-update-proposal"
          (Opt.info parseColeUpdateProposalSubmission $ Opt.progDesc "Submit an update proposal.")

      , Opt.command "submit-proposal-vote"
          (Opt.info parseColeVoteSubmission $ Opt.progDesc "Submit a proposal vote.")
      ]

parseColeUpdateProposal :: Parser NodeCmd
parseColeUpdateProposal = do
  UpdateProposal
    <$> pNetworkId
    <*> parseSigningKeyFile "signing-key" "Path to signing key."
    <*> parseProtocolVersion
    <*> parseSoftwareVersion
    <*> parseSystemTag
    <*> parseInstallerHash
    <*> parseFilePath "filepath" "Cole proposal output filepath."
    <*> pColeProtocolParametersUpdate

parseColeVoteSubmission :: Parser NodeCmd
parseColeVoteSubmission = do
  SubmitVote
    <$> pNetworkId
    <*> parseFilePath "filepath" "Filepath of Cole update proposal vote."


pColeProtocolParametersUpdate :: Parser ColeProtocolParametersUpdate
pColeProtocolParametersUpdate =
  ColeProtocolParametersUpdate
    <$> optional parseScriptVersion
    <*> optional parseSlotDuration
    <*> optional parseMaxBlockSize
    <*> optional parseMaxHeaderSize
    <*> optional parseMaxTxSize
    <*> optional parseMaxProposalSize
    <*> optional parseMpcThd
    <*> optional parseHeavyDelThd
    <*> optional parseUpdateVoteThd
    <*> optional parseUpdateProposalThd
    <*> optional parseUpdateProposalTTL
    <*> optional parseSoftforkRule
    <*> optional parseTxFeePolicy
    <*> optional parseUnlockStakeEpoch

parseColeUpdateProposalSubmission :: Parser NodeCmd
parseColeUpdateProposalSubmission =
  SubmitUpdateProposal
    <$> pNetworkId
    <*> parseFilePath "filepath" "Filepath of Cole update proposal."


parseColeVote :: Parser NodeCmd
parseColeVote =
  CreateVote
    <$> pNetworkId
    <*> (SigningKeyFile <$> parseFilePath "signing-key" "Filepath of signing key.")
    <*> parseFilePath "proposal-filepath" "Filepath of Cole update proposal."
    <*> parseVoteBool
    <*> parseFilePath "output-filepath" "Cole vote output filepath."

--------------------------------------------------------------------------------
-- CLI Parsers
--------------------------------------------------------------------------------

parseScriptVersion :: Parser Word16
parseScriptVersion =
  option auto
    ( long "script-version"
    <> metavar "WORD16"
    <> help "Proposed script version."
    )

parseSlotDuration :: Parser Natural
parseSlotDuration =
  option auto
    ( long "slot-duration"
    <> metavar "NATURAL"
    <> help "Proposed slot duration."
    )

parseSystemTag :: Parser SystemTag
parseSystemTag = option (eitherReader checkSysTag)
                   ( long "system-tag"
                   <> metavar "STRING"
                   <> help "Identify which system (linux, win64, etc) the update proposal is for."
                   )
 where
  checkSysTag :: String -> Either String SystemTag
  checkSysTag name =
    let tag = SystemTag $ toS name
    in case checkSystemTag tag of
         Left err -> Left . toS $ sformat build err
         Right () -> Right tag

parseInstallerHash :: Parser InstallerHash
parseInstallerHash =
  InstallerHash .  hashRaw . C8.pack
    <$> strOption ( long "installer-hash"
                  <> metavar "HASH"
                  <> help "Software hash."
                  )

parseMaxBlockSize :: Parser Natural
parseMaxBlockSize =
  option auto
    ( long "max-block-size"
    <> metavar "NATURAL"
    <> help "Proposed max block size."
    )

parseMaxHeaderSize :: Parser Natural
parseMaxHeaderSize =
  option auto
    ( long "max-header-size"
    <> metavar "NATURAL"
    <> help "Proposed max block header size."
    )

parseMaxTxSize :: Parser Natural
parseMaxTxSize =
  option auto
    ( long "max-tx-size"
    <> metavar "NATURAL"
    <> help "Proposed max transaction size."
    )

parseMaxProposalSize :: Parser  Natural
parseMaxProposalSize =
  option auto
    ( long "max-proposal-size"
    <> metavar "NATURAL"
    <> help "Proposed max update proposal size."
    )

parseMpcThd :: Parser Cole.EntropicPortion
parseMpcThd =
  rationalToEntropicPortion
    <$> parseFraction "max-mpc-thd" "Proposed max mpc threshold."

parseProtocolVersion :: Parser ProtocolVersion
parseProtocolVersion =
  ProtocolVersion <$> (parseWord "protocol-version-major" "Protocol verson major." "WORD16" :: Parser Word16)
                  <*> (parseWord "protocol-version-sentry" "Protocol verson sentry." "WORD16" :: Parser Word16)

parseHeavyDelThd :: Parser Cole.EntropicPortion
parseHeavyDelThd =
  rationalToEntropicPortion
    <$> parseFraction "heavy-del-thd" "Proposed heavy delegation threshold."

parseUpdateVoteThd :: Parser Cole.EntropicPortion
parseUpdateVoteThd =
  rationalToEntropicPortion
    <$> parseFraction "update-vote-thd" "Propose update vote threshold."

parseUpdateProposalThd :: Parser Cole.EntropicPortion
parseUpdateProposalThd =
  rationalToEntropicPortion
    <$> parseFraction "update-proposal-thd" "Propose update proposal threshold."

parseUpdateProposalTTL :: Parser SlotNumber
parseUpdateProposalTTL =
  SlotNumber
    <$> option auto
          ( long "time-to-live"
          <> metavar "WORD64"
          <> help "Proposed time for an update proposal to live."
          )

parseSoftforkRule :: Parser SoftforkRule
parseSoftforkRule =
  SoftforkRule
    <$> (rationalToEntropicPortion <$> parseFraction "softfork-init-thd" "Propose initial threshold (right after proposal is confirmed).")
    <*> (rationalToEntropicPortion <$> parseFraction "softfork-min-thd" "Propose minimum threshold (threshold can't be less than this).")
    <*> (rationalToEntropicPortion <$> parseFraction "softfork-thd-dec" "Propose threshold decrement (threshold will decrease by this amount after each epoch).")


parseSoftwareVersion :: Parser SoftwareVersion
parseSoftwareVersion =
  SoftwareVersion <$> parseApplicationName <*> parseNumSoftwareVersion

parseApplicationName :: Parser ApplicationName
parseApplicationName = option (eitherReader checkAppNameLength)
       (  long "application-name"
       <> metavar "STRING"
       <> help "The name of the application."
       )
 where
  checkAppNameLength :: String -> Either String ApplicationName
  checkAppNameLength name =
    let appName = ApplicationName $ toS name
    in case checkApplicationName appName of
         Left err -> Left . toS $ sformat build err
         Right () -> Right appName

parseNumSoftwareVersion :: Parser NumSoftwareVersion
parseNumSoftwareVersion =
  parseWord
    "software-version-num"
    "Numeric software version associated with application name."
    "WORD32"

parseTxFeePolicy :: Parser TxFeePolicy
parseTxFeePolicy =
  TxFeePolicyTxSizeLinear
    <$> ( TxSizeLinear <$> parseEntropic "tx-fee-a-constant" "Propose the constant a for txfee = a + b*s where s is the size."
                       <*> parseFraction "tx-fee-b-constant" "Propose the constant b for txfee = a + b*s where s is the size."
        )

parseVoteBool :: Parser Bool
parseVoteBool = flag' True (long "vote-yes" <> help "Vote yes with respect to an update proposal.")
            <|> flag' False (long "vote-no" <> help "Vote no with respect to an update proposal.")

parseUnlockStakeEpoch :: Parser EpochNumber
parseUnlockStakeEpoch =
  EpochNumber
    <$> option auto
      ( long "unlock-stake-epoch"
      <> metavar "WORD64"
      <> help "Proposed epoch to unlock all stake."
      )


parseWord :: Integral a => String -> String -> String -> Parser a
parseWord optname desc metvar = option (fromInteger <$> auto)
  $ long optname <> metavar metvar <> help desc



parseAddress :: String -> String -> Parser (Address ColeAddr)
parseAddress opt desc =
  option (cliParseBase58Address <$> str)
    $ long opt <> metavar "ADDR" <> help desc

parseColeKeyFormat :: Parser ColeKeyFormat
parseColeKeyFormat = asum
  [ flag' LegacyColeKeyFormat $
        long "cole-legacy-formats"
     <> help "Cole/bcc-sl formats and compatibility"

  , flag' NonLegacyColeKeyFormat $
        long "cole-formats"
     <> help "Cole era formats and compatibility"

    -- And hidden compatibility flag aliases that should be deprecated:
  , flag' LegacyColeKeyFormat $ hidden <> long "cole-legacy"
  , flag' NonLegacyColeKeyFormat $ hidden <> long "real-pbft"

  -- Default Cole key format
  , pure NonLegacyColeKeyFormat
  ]


parseFakeAvvmOptions :: Parser FakeAvvmOptions
parseFakeAvvmOptions =
  FakeAvvmOptions
    <$> parseIntegral "avvm-entry-count" "Number of AVVM addresses."
    <*> parseEntropic "avvm-entry-balance" "AVVM address."

parseK :: Parser BlockCount
parseK =
  BlockCount
    <$> parseIntegral "k" "The security parameter of the Shardagnostic protocol."

parseNewDirectory :: String -> String -> Parser NewDirectory
parseNewDirectory opt desc = NewDirectory <$> parseFilePath opt desc

parseFractionWithDefault
  :: String
  -> String
  -> Double
  -> Parser Rational
parseFractionWithDefault optname desc w =
  toRational <$> option readDouble
    ( long optname
    <> metavar "DOUBLE"
    <> help desc
    <> value w
    )

pNetworkId :: Parser NetworkId
pNetworkId =
  pMainnet' <|> fmap Testnet pTestnetMagic
 where
   pMainnet' :: Parser NetworkId
   pMainnet' =
    Opt.flag' Mainnet
      (  Opt.long "mainnet"
      <> Opt.help "Use the mainnet magic id."
      )

pTestnetMagic :: Parser NetworkMagic
pTestnetMagic =
  NetworkMagic <$>
    Opt.option Opt.auto
      (  Opt.long "testnet-magic"
      <> Opt.metavar "NATURAL"
      <> Opt.help "Specify a testnet magic id."
      )

parseNewSigningKeyFile :: String -> Parser NewSigningKeyFile
parseNewSigningKeyFile opt =
  NewSigningKeyFile
    <$> parseFilePath opt "Non-existent file to write the signing key to."

parseNewTxFile :: String -> Parser NewTxFile
parseNewTxFile opt =
  NewTxFile
    <$> parseFilePath opt "Non-existent file to write the signed transaction to."

parseNewVerificationKeyFile :: String -> Parser NewVerificationKeyFile
parseNewVerificationKeyFile opt =
  NewVerificationKeyFile
    <$> parseFilePath opt "Non-existent file to write the verification key to."

parseProtocolMagicId :: String -> Parser ProtocolMagicId
parseProtocolMagicId arg =
  ProtocolMagicId
    <$> parseIntegral arg "The magic number unique to any instance of Bcc."

parseProtocolMagic :: Parser ProtocolMagic
parseProtocolMagic =
  flip AProtocolMagic RequiresMagic . flip Annotated ()
    <$> parseProtocolMagicId "protocol-magic"

parseTxFile :: String -> Parser TxFile
parseTxFile opt =
  TxFile
    <$> parseFilePath opt "File containing the signed transaction."

parseUTCTime :: String -> String -> Parser UTCTime
parseUTCTime optname desc =
  option (posixSecondsToUTCTime . fromInteger <$> auto)
    $ long optname <> metavar "POSIXSECONDS" <> help desc

cliParseBase58Address :: Text -> Address ColeAddr
cliParseBase58Address t =
  case decodeAddressBase58 t of
    Left err -> panic $ "Bad Base58 address: " <> Text.pack (show err)
    Right coleAddress -> ColeAddress coleAddress

parseFraction :: String -> String -> Parser Rational
parseFraction optname desc =
  option (toRational <$> readDouble) $
      long optname
   <> metavar "DOUBLE"
   <> help desc

parseIntegral :: Integral a => String -> String -> Parser a
parseIntegral optname desc = option (fromInteger <$> auto)
  $ long optname <> metavar "INT" <> help desc

parseEntropic :: String -> String -> Parser Cole.Entropic
parseEntropic optname desc =
  Opt.option (readerFromAttoParser parseEntropicAtto)
    (  long optname
    <> metavar "INT"
    <> help desc
    )
 where
  parseEntropicAtto :: Atto.Parser Cole.Entropic
  parseEntropicAtto = do
    i <- Atto.decimal
    if i > toInteger (maxBound :: Word64)
    then fail $ show i <> " entropic exceeds the Word64 upper bound"
    else case toColeEntropic (Entropic i) of
           Just coleEntropic -> return coleEntropic
           Nothing -> panic $ "Error converting entropic: " <> Text.pack (show i)

readDouble :: ReadM Double
readDouble = do
  f <- auto
  when (f < 0) $ readerError "fraction must be >= 0"
  when (f > 1) $ readerError "fraction must be <= 1"
  return f

parseFilePath :: String -> String -> Parser FilePath
parseFilePath optname desc =
  strOption
    ( long optname
    <> metavar "FILEPATH"
    <> help desc
    <> completer (bashCompleter "file")
    )

parseSigningKeyFile :: String -> String -> Parser SigningKeyFile
parseSigningKeyFile opt desc = SigningKeyFile <$> parseFilePath opt desc


parseGenesisFile :: String -> Parser GenesisFile
parseGenesisFile opt =
  GenesisFile <$> parseFilePath opt "Genesis JSON file."

