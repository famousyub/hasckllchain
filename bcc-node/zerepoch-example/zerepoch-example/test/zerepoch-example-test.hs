import           Bcc.Prelude

import           Hedgehog.Main (defaultMain)
import           Test.ZerepochExample.Zerepoch
import           Test.ZerepochExample.ScriptData

main :: IO ()
main =
  defaultMain
    [ Test.ZerepochExample.Zerepoch.tests
    , Test.ZerepochExample.ScriptData.tests
    ]
