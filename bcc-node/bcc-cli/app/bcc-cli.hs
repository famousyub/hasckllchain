{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

import           Bcc.Prelude hiding (option)

import           Control.Monad.Trans.Except.Exit (orDie)
import qualified Options.Applicative as Opt

import           Bcc.CLI.Parsers (opts, pref)
import           Bcc.CLI.Run (renderClientCommandError, runClientCommand)
import           Bcc.CLI.TopHandler
import qualified Bcc.Crypto.Libsodium as Crypto
#ifdef UNIX
import           System.Posix.Files
#endif

main :: IO ()
main = toplevelExceptionHandler $ do
  -- TODO: Remove sodiumInit: https://github.com/The-Blockchain-Company/bcc-base/issues/175
  Crypto.sodiumInit
#ifdef UNIX
  _ <- setFileCreationMask (otherModes `unionFileModes` groupModes)
#endif
  co <- Opt.customExecParser pref opts

  orDie renderClientCommandError $ runClientCommand co
