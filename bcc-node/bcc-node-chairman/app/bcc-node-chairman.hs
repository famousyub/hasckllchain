module Main where

import           Bcc.Chairman.Commands
import           Control.Monad
import           Data.Function
import           Data.Semigroup
import           Options.Applicative
import           System.IO (IO)

main :: IO ()
main = join
  . customExecParser
    ( prefs (showHelpOnEmpty <> showHelpOnError)
    )
  $ info (commands <**> helper)
    (  fullDesc
    <> progDesc "Chairman checks Bcc clusters for progress and consensus."
    <> header "Chairman sits in a room full of Sophie nodes, and checks \
              \if they are all behaving ..."
    )
