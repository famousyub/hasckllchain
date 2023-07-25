{-# LANGUAGE GADTs #-}

-- | Dispatch for running all the CLI commands
module Bcc.CLI.Run
  ( ClientCommand(..)
  , ClientCommandErrors
  , renderClientCommandError
  , runClientCommand
  ) where

import           Bcc.Prelude

import           Control.Monad.Trans.Except.Extra (firstExceptT)
import           Data.String
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import           Bcc.CLI.Cole.Commands (ColeCommand)
import           Bcc.CLI.Cole.Run (ColeClientCmdError, renderColeClientCmdError,
                   runColeClientCommand)
import           Bcc.CLI.Sophie.Commands (SophieCommand)
import           Bcc.CLI.Sophie.Run (SophieClientCmdError, renderSophieClientCmdError,
                   runSophieClientCommand)

import           Bcc.CLI.Render (customRenderHelp)

import           Bcc.Config.Git.Rev (gitRev)
import           Data.Version (showVersion)
import           Paths_bcc_cli (version)
import           System.Info (arch, compilerName, compilerVersion, os)
import           Options.Applicative.Types (Option (..), OptReader (..), Parser (..), ParserInfo (..), ParserPrefs (..))
import           Options.Applicative.Help.Core

import qualified Data.List as L
import qualified System.IO as IO

-- | Sub-commands of 'bcc-cli'.
data ClientCommand =

    -- | Cole Related Commands
    ColeCommand ColeCommand

    -- | Sophie Related Commands
  | SophieCommand SophieCommand

    -- | Sophie-related commands that have been parsed under the
    -- now-deprecated \"sophie\" subcommand.
  | DeprecatedSophieSubcommand SophieCommand

  | forall a. Help ParserPrefs (ParserInfo a)
  | DisplayVersion

data ClientCommandErrors
  = ColeClientError ColeClientCmdError
  | SophieClientError SophieCommand SophieClientCmdError
  deriving Show

runClientCommand :: ClientCommand -> ExceptT ClientCommandErrors IO ()
runClientCommand (ColeCommand c) = firstExceptT ColeClientError $ runColeClientCommand c
runClientCommand (SophieCommand c) = firstExceptT (SophieClientError c) $ runSophieClientCommand c
runClientCommand (DeprecatedSophieSubcommand c) =
  firstExceptT (SophieClientError c)
    $ runSophieClientCommandWithDeprecationWarning
    $ runSophieClientCommand c
runClientCommand (Help pprefs allParserInfo) = runHelp pprefs allParserInfo
runClientCommand DisplayVersion = runDisplayVersion

renderClientCommandError :: ClientCommandErrors -> Text
renderClientCommandError (ColeClientError err) =
  renderColeClientCmdError err
renderClientCommandError (SophieClientError cmd err) =
  renderSophieClientCmdError cmd err

-- | Combine an 'ExceptT' that will write a warning message to @stderr@ with
-- the provided 'ExceptT'.
ioExceptTWithWarning :: MonadIO m => Text -> ExceptT e m () -> ExceptT e m ()
ioExceptTWithWarning warningMsg e =
  liftIO (Text.hPutStrLn stderr warningMsg) >> e

-- | Used in the event that Sophie-related commands are run using the
-- now-deprecated \"sophie\" subcommand.
runSophieClientCommandWithDeprecationWarning
  :: MonadIO m
  => ExceptT e m ()
  -> ExceptT e m ()
runSophieClientCommandWithDeprecationWarning =
    ioExceptTWithWarning warningMsg
  where
    warningMsg :: Text
    warningMsg =
      "WARNING: The \"sophie\" subcommand is now deprecated and will be "
        <> "removed in the future. Please use the top-level commands instead."

runDisplayVersion :: ExceptT ClientCommandErrors IO ()
runDisplayVersion = do
    liftIO . putTextLn $ mconcat
                [ "bcc-cli ", renderVersion version
                , " - ", Text.pack os, "-", Text.pack arch
                , " - ", Text.pack compilerName, "-", renderVersion compilerVersion
                , "\ngit rev ", gitRev
                ]
  where
    renderVersion = Text.pack . showVersion


helpAll :: ParserPrefs -> String -> [String] -> ParserInfo a -> IO ()
helpAll pprefs progn rnames parserInfo = do
  IO.putStrLn $ customRenderHelp 80 (usage_help parserInfo)
  IO.putStrLn ""
  go (infoParser parserInfo)
  where go :: Parser a -> IO ()
        go p = case p of
          NilP _ -> return ()
          OptP optP -> case optMain optP of
            CmdReader _ cs f -> do
              forM_ cs $ \c ->
                forM_ (f c) $ \subParserInfo -> 
                  helpAll pprefs progn (c:rnames) subParserInfo
            _ -> return ()
          AltP pa pb -> go pa >> go pb
          MultP pf px -> go pf >> go px
          BindP pa _ -> go pa
        usage_help i =
              mconcat
              [ usageHelp (pure . parserUsage pprefs (infoParser i) . L.unwords $ progn : reverse rnames)
              , descriptionHelp (infoProgDesc i)
              ]

runHelp :: ParserPrefs -> ParserInfo a -> ExceptT ClientCommandErrors IO ()
runHelp pprefs allParserInfo = liftIO $ helpAll pprefs "bcc-cli" [] allParserInfo
