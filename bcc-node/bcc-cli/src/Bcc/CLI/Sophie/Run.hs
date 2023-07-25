module Bcc.CLI.Sophie.Run
  ( SophieClientCmdError
  , renderSophieClientCmdError
  , runSophieClientCommand
  ) where

import           Bcc.Prelude

import           Control.Monad.Trans.Except.Extra (firstExceptT)

import           Bcc.CLI.Sophie.Parsers

import           Bcc.CLI.Sophie.Run.Address
import           Bcc.CLI.Sophie.Run.Governance
import           Bcc.CLI.Sophie.Run.Key
import           Bcc.CLI.Sophie.Run.Node
import           Bcc.CLI.Sophie.Run.Pool
import           Bcc.CLI.Sophie.Run.Query
import           Bcc.CLI.Sophie.Run.StakeAddress
import           Bcc.CLI.Sophie.Run.Transaction
                                         -- Block, System, DevOps
import           Bcc.CLI.Sophie.Run.Genesis
import           Bcc.CLI.Sophie.Run.TextView

data SophieClientCmdError
  = SophieCmdAddressError !SophieAddressCmdError
  | SophieCmdGenesisError !SophieGenesisCmdError
  | SophieCmdGovernanceError !SophieGovernanceCmdError
  | SophieCmdNodeError !SophieNodeCmdError
  | SophieCmdPoolError !SophiePoolCmdError
  | SophieCmdStakeAddressError !SophieStakeAddressCmdError
  | SophieCmdTextViewError !SophieTextViewFileError
  | SophieCmdTransactionError !SophieTxCmdError
  | SophieCmdQueryError !SophieQueryCmdError
  | SophieCmdKeyError !SophieKeyCmdError
  deriving Show

renderSophieClientCmdError :: SophieCommand -> SophieClientCmdError -> Text
renderSophieClientCmdError cmd err =
  case err of
    SophieCmdAddressError addrCmdErr ->
       renderError cmd renderSophieAddressCmdError addrCmdErr
    SophieCmdGenesisError genesisCmdErr ->
       renderError cmd renderSophieGenesisCmdError genesisCmdErr
    SophieCmdGovernanceError govCmdErr ->
       renderError cmd renderSophieGovernanceError govCmdErr
    SophieCmdNodeError nodeCmdErr ->
       renderError cmd renderSophieNodeCmdError nodeCmdErr
    SophieCmdPoolError poolCmdErr ->
       renderError cmd renderSophiePoolCmdError poolCmdErr
    SophieCmdStakeAddressError stakeAddrCmdErr ->
       renderError cmd renderSophieStakeAddressCmdError stakeAddrCmdErr
    SophieCmdTextViewError txtViewErr ->
       renderError cmd renderSophieTextViewFileError txtViewErr
    SophieCmdTransactionError txErr ->
       renderError cmd renderSophieTxCmdError txErr
    SophieCmdQueryError queryErr ->
       renderError cmd renderSophieQueryCmdError queryErr
    SophieCmdKeyError keyErr ->
       renderError cmd renderSophieKeyCmdError keyErr
 where
   renderError :: SophieCommand -> (a -> Text) -> a -> Text
   renderError sophieCmd renderer shelCliCmdErr =
      mconcat [ "Command failed: "
              , renderSophieCommand sophieCmd
              , "  Error: "
              , renderer shelCliCmdErr
              ]


--
-- CLI sophie command dispatch
--

runSophieClientCommand :: SophieCommand -> ExceptT SophieClientCmdError IO ()
runSophieClientCommand (AddressCmd      cmd) = firstExceptT SophieCmdAddressError $ runAddressCmd cmd
runSophieClientCommand (StakeAddressCmd cmd) = firstExceptT SophieCmdStakeAddressError $ runStakeAddressCmd cmd
runSophieClientCommand (KeyCmd          cmd) = firstExceptT SophieCmdKeyError $ runKeyCmd cmd
runSophieClientCommand (TransactionCmd  cmd) = firstExceptT SophieCmdTransactionError $ runTransactionCmd  cmd
runSophieClientCommand (NodeCmd         cmd) = firstExceptT SophieCmdNodeError $ runNodeCmd cmd
runSophieClientCommand (PoolCmd         cmd) = firstExceptT SophieCmdPoolError $ runPoolCmd cmd
runSophieClientCommand (QueryCmd        cmd) = firstExceptT SophieCmdQueryError $ runQueryCmd cmd
runSophieClientCommand (GovernanceCmd   cmd) = firstExceptT SophieCmdGovernanceError $ runGovernanceCmd cmd
runSophieClientCommand (GenesisCmd      cmd) = firstExceptT SophieCmdGenesisError $ runGenesisCmd cmd
runSophieClientCommand (TextViewCmd     cmd) = firstExceptT SophieCmdTextViewError $ runTextViewCmd cmd
