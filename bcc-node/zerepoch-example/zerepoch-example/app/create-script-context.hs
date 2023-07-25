import           Prelude

import           Bcc.Api

import           Control.Monad.Trans.Except (runExceptT)
import qualified Data.ByteString.Lazy as LB
import           Data.Foldable
import           Data.Word
import           Options.Applicative
import qualified Options.Applicative as Opt

import           Bcc.ZerepochExample.ScriptContextChecker

main :: IO ()
main = runScriptContextCmd =<< customExecParser pref opts

pref :: ParserPrefs
pref = Opt.prefs showHelpOnEmpty

opts :: ParserInfo ScriptContextCmd
opts = Opt.info (parseScriptContextCmd <**> Opt.helper) Opt.fullDesc

parseScriptContextCmd :: Parser ScriptContextCmd
parseScriptContextCmd = parseGenerateDummy <|> parseGenerateTxBody
 where
  parseGenerateDummy :: Parser ScriptContextCmd
  parseGenerateDummy =
    flag' GenerateDummyScriptContextRedeemer
      (  long "generate"
      <> help "Create a dummy script context redeemer"
      )


  parseGenerateTxBody :: Parser ScriptContextCmd
  parseGenerateTxBody =
    GenerateScriptContextRedeemerTxBody
      <$> strOption ( long "generate-tx"
                    <> metavar "FILE"
                    <> help "Create a script context from a tx body."
                    <> Opt.completer (Opt.bashCompleter "file")
                    )
      <*> pConsensusModeParams
      <*> pNetworkId

data ScriptContextCmd
  = GenerateDummyScriptContextRedeemer
  | GenerateScriptContextRedeemerTxBody
      FilePath
      AnyConsensusModeParams
      NetworkId

runScriptContextCmd :: ScriptContextCmd -> IO ()
runScriptContextCmd GenerateDummyScriptContextRedeemer =
  LB.writeFile "example/work/script-context.redeemer" sampleTestScriptContextDataJSON
runScriptContextCmd (GenerateScriptContextRedeemerTxBody txbodyfile cModeParams nid) = do
      eTxBodyRedeemer <- runExceptT $ txToRedeemer txbodyfile cModeParams nid
      case eTxBodyRedeemer of
        Left err -> print err
        Right () -> return ()

pConsensusModeParams :: Parser AnyConsensusModeParams
pConsensusModeParams = asum
  [ Opt.flag' (AnyConsensusModeParams SophieModeParams)
      (  Opt.long "sophie-mode"
      <> Opt.help "For talking to a node running in Sophie-only mode."
      )
  , Opt.flag' ()
      (  Opt.long "cole-mode"
      <> Opt.help "For talking to a node running in Cole-only mode."
      )
       *> pColeConsensusMode
  , Opt.flag' ()
      (  Opt.long "bcc-mode"
      <> Opt.help "For talking to a node running in full Bcc mode (default)."
      )
       *> pBccConsensusMode
  , -- Default to the Bcc consensus mode.
    pure . AnyConsensusModeParams . BccModeParams $ EpochSlots defaultColeEpochSlots
  ]
 where
   pBccConsensusMode :: Parser AnyConsensusModeParams
   pBccConsensusMode = AnyConsensusModeParams . BccModeParams <$> pEpochSlots
   pColeConsensusMode :: Parser AnyConsensusModeParams
   pColeConsensusMode = AnyConsensusModeParams . ColeModeParams <$> pEpochSlots

defaultColeEpochSlots :: Word64
defaultColeEpochSlots = 21600

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

pEpochSlots :: Parser EpochSlots
pEpochSlots =
  EpochSlots <$>
    Opt.option Opt.auto
      (  Opt.long "epoch-slots"
      <> Opt.metavar "NATURAL"
      <> Opt.help "The number of slots per epoch for the Cole era."
      <> Opt.value defaultColeEpochSlots -- Default to the mainnet value.
      <> Opt.showDefault
      )
