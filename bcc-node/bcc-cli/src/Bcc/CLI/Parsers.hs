{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Bcc.CLI.Parsers
  ( opts
  , pref
  ) where

import           Bcc.Prelude
import           Bcc.CLI.Cole.Parsers (backwardsCompatibilityCommands, parseColeCommands)
import           Bcc.CLI.Render (customRenderHelp)
import           Bcc.CLI.Run (ClientCommand (..))
import           Bcc.CLI.Sophie.Parsers (parseSophieCommands)
import           Options.Applicative
import           Prelude (String)

import qualified Options.Applicative as Opt

command' :: String -> String -> Parser a -> Mod CommandFields a
command' c descr p =
    command c $ info (p <**> helper)
              $ mconcat [ progDesc descr ]

opts :: ParserInfo ClientCommand
opts =
  Opt.info (parseClientCommand <**> Opt.helper)
    ( Opt.fullDesc
      <> Opt.header
      "bcc-cli - utility to support a variety of key\
      \ operations (genesis generation, migration,\
      \ pretty-printing..) for different system generations."
    )

pref :: ParserPrefs
pref = Opt.prefs $ mempty
  <> showHelpOnEmpty
  <> helpHangUsageOverflow 10
  <> helpRenderHelp customRenderHelp

parseClientCommand :: Parser ClientCommand
parseClientCommand =
  asum
    -- There are name clashes between Sophie commands and the Cole backwards
    -- compat commands (e.g. "genesis"), and we need to prefer the Sophie ones
    -- so we list it first.
    [ parseSophie
    , parseCole
    , parseDeprecatedSophieSubcommand
    , backwardsCompatibilityCommands
    , parseDisplayVersion opts
    ]

parseCole :: Parser ClientCommand
parseCole =
  fmap ColeCommand $
  subparser $ mconcat
    [ commandGroup "Cole specific commands"
    , metavar "Cole specific commands"
    , command'
        "cole"
        "Cole specific commands"
         parseColeCommands
    ]

-- | Parse Sophie-related commands at the top level of the CLI.
parseSophie :: Parser ClientCommand
parseSophie = SophieCommand <$> parseSophieCommands

-- | Parse Sophie-related commands under the now-deprecated \"sophie\"
-- subcommand.
--
-- Note that this subcommand is 'internal' and is therefore hidden from the
-- help text.
parseDeprecatedSophieSubcommand :: Parser ClientCommand
parseDeprecatedSophieSubcommand =
  subparser $ mconcat
    [ commandGroup "Sophie specific commands (deprecated)"
    , metavar "Sophie specific commands"
    , command'
        "sophie"
        "Sophie specific commands (deprecated)"
        (DeprecatedSophieSubcommand <$> parseSophieCommands)
    , internal
    ]

-- Yes! A --version flag or version command. Either guess is right!
parseDisplayVersion :: ParserInfo a -> Parser ClientCommand
parseDisplayVersion allParserInfo =
      subparser
        (mconcat
         [ commandGroup "Miscellaneous commands"
         , metavar "Miscellaneous commands"
         , command'
           "help"
           "Show all help"
           (pure (Help pref allParserInfo))
         , command'
           "version"
           "Show the bcc-cli version"
           (pure DisplayVersion)
         ]
        )
  <|> flag' DisplayVersion
        (  long "version"
        <> help "Show the bcc-cli version"
        <> hidden
        )
