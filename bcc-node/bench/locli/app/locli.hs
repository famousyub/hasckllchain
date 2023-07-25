{-# LANGUAGE OverloadedStrings #-}

import           Bcc.Prelude hiding (option)

import           Control.Monad.Trans.Except.Exit (orDie)
import qualified Options.Applicative as Opt

import           Bcc.Analysis.TopHandler
import           Bcc.Unlog.Parsers (opts, pref)
import           Bcc.Unlog.Run (renderCommandError, runCommand)


main :: IO ()
main = toplevelExceptionHandler $ do

  co <- Opt.customExecParser pref opts

  orDie renderCommandError $ runCommand co
