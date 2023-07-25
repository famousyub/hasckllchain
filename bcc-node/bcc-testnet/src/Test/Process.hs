module Test.Process
  ( bashPath
  , execCli
  , execCli'
  , procCli
  , procNode
  , procSubmitApi
  , procChairman
  ) where

import           Control.Monad (return)
import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Function
import           Data.Maybe
import           Data.String
import           GHC.Stack (HasCallStack)
import           Hedgehog (MonadTest)
import           Hedgehog.Extras.Test.Process (ExecConfig)
import           System.IO (FilePath)
import           System.Process (CreateProcess)

import qualified GHC.Stack as GHC
import qualified Hedgehog.Extras.Test.Process as H
import qualified System.Environment as IO
import qualified System.IO.Unsafe as IO

-- | Path to the bash executable.  This is used on Windows so that the caller can supply a Windows
-- path to the bash executable because there is no reliable way to invoke bash without the full
-- Windows path from Haskell.
bashPath :: FilePath
bashPath = IO.unsafePerformIO $ do
  mValue <- IO.lookupEnv "BASH_PATH"
  case mValue of
    Just "" -> return "bash"
    Just value -> return value
    Nothing -> return "bash"
  
{-# NOINLINE bashPath #-}

-- | Run bcc-cli, returning the stdout
execCli
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => [String]
  -> m String
execCli = GHC.withFrozenCallStack $ H.execFlex "bcc-cli" "BCC_CLI"

-- | Run bcc-cli, returning the stdout
execCli'
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => ExecConfig
  -> [String]
  -> m String
execCli' execConfig = GHC.withFrozenCallStack $ H.execFlex' execConfig "bcc-cli" "BCC_CLI"

-- | Create a 'CreateProcess' describing how to start the bcc-cli process
-- and an argument list.
procCli
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => [String]
  -- ^ Arguments to the CLI command
  -> m CreateProcess
  -- ^ Captured stdout
procCli = GHC.withFrozenCallStack $ H.procFlex "bcc-cli" "BCC_CLI"

-- | Create a 'CreateProcess' describing how to start the bcc-node process
-- and an argument list.
procNode
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => [String]
  -- ^ Arguments to the CLI command
  -> m CreateProcess
  -- ^ Captured stdout
procNode = GHC.withFrozenCallStack $ H.procFlex "bcc-node" "BCC_NODE"

-- | Create a 'CreateProcess' describing how to start the bcc-submit-api process
-- and an argument list.
procSubmitApi
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => [String]
  -- ^ Arguments to the CLI command
  -> m CreateProcess
  -- ^ Captured stdout
procSubmitApi = GHC.withFrozenCallStack $ H.procFlex "bcc-submit-api" "BCC_SUBMIT_API"

-- | Create a 'CreateProcess' describing how to start the bcc-node-chairman process
-- and an argument list.
procChairman
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => [String]
  -- ^ Arguments to the CLI command
  -> m CreateProcess
  -- ^ Captured stdout
procChairman = GHC.withFrozenCallStack $ H.procFlex "bcc-node-chairman" "BCC_NODE_CHAIRMAN" . ("run":)
