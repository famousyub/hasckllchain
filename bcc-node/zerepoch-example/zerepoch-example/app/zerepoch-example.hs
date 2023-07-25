import           Prelude

import           Bcc.Api

import           System.Directory
import           System.FilePath.Posix ((</>))

import           Bcc.ZerepochExample.AlwaysFails (alwaysFailsScript)
import           Bcc.ZerepochExample.AlwaysSucceeds (alwaysSucceedsScript)
import           Bcc.ZerepochExample.CustomDatumRedeemerGuess
import           Bcc.ZerepochExample.DatumRedeemerGuess (guessScript)
import           Bcc.ZerepochExample.MintingScript (apiExampleZerepochMintingScript)
import           Bcc.ZerepochExample.ScriptContextChecker
import           Bcc.ZerepochExample.Sum (sumScript)

main :: IO ()
main = do
  let dir = "generated-zerepoch-scripts"
  createDirectory dir

  _ <- writeFileTextEnvelope (dir </> "always-fails.zerepoch") Nothing alwaysFailsScript
  _ <- writeFileTextEnvelope (dir </> "always-succeeds-spending.zerepoch") Nothing alwaysSucceedsScript
  _ <- writeFileTextEnvelope (dir </> "guess-42-datum-42-txin.zerepoch") Nothing guessScript
  _ <- writeFileTextEnvelope (dir </> "custom-guess-42-datum-42.zerepoch") Nothing customGuessScript
  _ <- writeFileTextEnvelope (dir </> "anyone-can-mint.zerepoch") Nothing apiExampleZerepochMintingScript
  _ <- writeFileTextEnvelope (dir </> "sum.zerepoch") Nothing sumScript
  _ <- writeFileTextEnvelope (dir </> "context-equivalance-test.zerepoch") Nothing scriptContextCheckScript
  _ <- writeFileTextEnvelope (dir </> "minting-context-equivalance-test.zerepoch") Nothing customApiExampleZerepochMintingScript
  return ()
