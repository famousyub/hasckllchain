module Bcc.CLI.Sophie.Run.Governance
  ( SophieGovernanceCmdError
  , renderSophieGovernanceError
  , runGovernanceCmd
  ) where

import           Bcc.Prelude

import qualified Data.Text as Text

import           Control.Monad.Trans.Except.Extra (firstExceptT, left, newExceptT)

import           Bcc.Api
import           Bcc.Api.Sophie

import           Bcc.CLI.Sophie.Key (InputDecodeError, VerificationKeyOrHashOrFile,
                   readVerificationKeyOrHashOrFile, readVerificationKeyOrHashOrTextEnvFile)
import           Bcc.CLI.Sophie.Parsers
import           Bcc.CLI.Types

import qualified Sophie.Spec.Ledger.TxBody as Sophie


data SophieGovernanceCmdError
  = SophieGovernanceCmdTextEnvReadError !(FileError TextEnvelopeError)
  | SophieGovernanceCmdKeyReadError !(FileError InputDecodeError)
  | SophieGovernanceCmdTextEnvWriteError !(FileError ())
  | SophieGovernanceCmdEmptyUpdateProposalError
  | SophieGovernanceCmdMIRCertificateKeyRewardMistmach
      !FilePath
      !Int
      -- ^ Number of stake verification keys
      !Int
      -- ^ Number of reward amounts
  deriving Show

renderSophieGovernanceError :: SophieGovernanceCmdError -> Text
renderSophieGovernanceError err =
  case err of
    SophieGovernanceCmdTextEnvReadError fileErr -> Text.pack (displayError fileErr)
    SophieGovernanceCmdKeyReadError fileErr -> Text.pack (displayError fileErr)
    SophieGovernanceCmdTextEnvWriteError fileErr -> Text.pack (displayError fileErr)
    -- TODO: The equality check is still not working for empty update proposals.
    SophieGovernanceCmdEmptyUpdateProposalError ->
      "Empty update proposals are not allowed"
    SophieGovernanceCmdMIRCertificateKeyRewardMistmach fp numVKeys numRwdAmts ->
       "Error creating the MIR certificate at: " <> textShow fp
       <> " The number of staking keys: " <> textShow numVKeys
       <> " and the number of reward amounts: " <> textShow numRwdAmts
       <> " are not equivalent."
  where
    textShow x = Text.pack (show x)


runGovernanceCmd :: GovernanceCmd -> ExceptT SophieGovernanceCmdError IO ()
runGovernanceCmd (GovernanceMIRPayStakeAddressesCertificate mirpot vKeys rewards out) =
  runGovernanceMIRCertificatePayStakeAddrs mirpot vKeys rewards out
runGovernanceCmd (GovernanceMIRTransfer amt out direction) =
  runGovernanceMIRCertificateTransfer amt out direction
runGovernanceCmd (GovernanceGenesisKeyDelegationCertificate genVk genDelegVk vrfVk out) =
  runGovernanceGenesisKeyDelegationCertificate genVk genDelegVk vrfVk out
runGovernanceCmd (GovernanceVestedKeyDelegationCertificate genVk genDelegVk vrfVk out) =
  runGovernanceVestedKeyDelegationCertificate genVk genDelegVk vrfVk out
runGovernanceCmd (GovernanceUpdateProposal out eNo genVKeys ppUp) =
  runGovernanceUpdateProposal out eNo genVKeys ppUp

runGovernanceMIRCertificatePayStakeAddrs
  :: Sophie.MIRPot
  -> [StakeAddress] -- ^ Stake addresses
  -> [Entropic]     -- ^ Corresponding reward amounts (same length)
  -> OutputFile
  -> ExceptT SophieGovernanceCmdError IO ()
runGovernanceMIRCertificatePayStakeAddrs mirPot sAddrs rwdAmts (OutputFile oFp) = do

    unless (length sAddrs == length rwdAmts) $
      left $ SophieGovernanceCmdMIRCertificateKeyRewardMistmach
               oFp (length sAddrs) (length rwdAmts)

    let sCreds  = map stakeAddrToStakeCredential sAddrs
        mirCert = makeMIRCertificate mirPot (StakeAddressesMIR $ zip sCreds rwdAmts)

    firstExceptT SophieGovernanceCmdTextEnvWriteError
      . newExceptT
      $ writeFileTextEnvelope oFp (Just mirCertDesc) mirCert
  where
    mirCertDesc :: TextEnvelopeDescr
    mirCertDesc = "Move Instantaneous Rewards Certificate"

    --TODO: expose a pattern for StakeAddress that give us the StakeCredential
    stakeAddrToStakeCredential :: StakeAddress -> StakeCredential
    stakeAddrToStakeCredential (StakeAddress _ scred) =
      fromSophieStakeCredential scred

runGovernanceMIRCertificateTransfer
  :: Entropic
  -> OutputFile
  -> TransferDirection
  -> ExceptT SophieGovernanceCmdError IO ()
runGovernanceMIRCertificateTransfer ll (OutputFile oFp) direction = do
  mirCert <- case direction of
                 TransferToReserves ->
                   return . makeMIRCertificate Sophie.TreasuryMIR $ SendToReservesMIR ll
                 TransferToTreasury ->
                   return . makeMIRCertificate Sophie.ReservesMIR $ SendToTreasuryMIR ll

  firstExceptT SophieGovernanceCmdTextEnvWriteError
    . newExceptT
    $ writeFileTextEnvelope oFp (Just $ mirCertDesc direction) mirCert
 where
  mirCertDesc :: TransferDirection -> TextEnvelopeDescr
  mirCertDesc TransferToTreasury = "MIR Certificate Send To Treasury"
  mirCertDesc TransferToReserves = "MIR Certificate Send To Reserves"


runGovernanceGenesisKeyDelegationCertificate
  :: VerificationKeyOrHashOrFile GenesisKey
  -> VerificationKeyOrHashOrFile GenesisDelegateKey
  -> VerificationKeyOrHashOrFile VrfKey
  -> OutputFile
  -> ExceptT SophieGovernanceCmdError IO ()
runGovernanceGenesisKeyDelegationCertificate genVkOrHashOrFp
                                             genDelVkOrHashOrFp
                                             vrfVkOrHashOrFp
                                             (OutputFile oFp) = do
    genesisVkHash <- firstExceptT SophieGovernanceCmdKeyReadError
      . newExceptT
      $ readVerificationKeyOrHashOrTextEnvFile AsGenesisKey genVkOrHashOrFp
    genesisDelVkHash <-firstExceptT SophieGovernanceCmdKeyReadError
      . newExceptT
      $ readVerificationKeyOrHashOrTextEnvFile AsGenesisDelegateKey genDelVkOrHashOrFp
    vrfVkHash <- firstExceptT SophieGovernanceCmdKeyReadError
      . newExceptT
      $ readVerificationKeyOrHashOrFile AsVrfKey vrfVkOrHashOrFp
    firstExceptT SophieGovernanceCmdTextEnvWriteError
      . newExceptT
      $ writeFileTextEnvelope oFp (Just genKeyDelegCertDesc)
      $ makeGenesisKeyDelegationCertificate genesisVkHash genesisDelVkHash vrfVkHash
  where
    genKeyDelegCertDesc :: TextEnvelopeDescr
    genKeyDelegCertDesc = "Genesis Key Delegation Certificate"

runGovernanceVestedKeyDelegationCertificate
  :: VerificationKeyOrHashOrFile VestedKey
  -> VerificationKeyOrHashOrFile VestedDelegateKey
  -> VerificationKeyOrHashOrFile VrfKey
  -> OutputFile
  -> ExceptT SophieGovernanceCmdError IO ()
runGovernanceVestedKeyDelegationCertificate genVkOrHashOrFp
                                             genDelVkOrHashOrFp
                                             vrfVkOrHashOrFp
                                             (OutputFile oFp) = do
    vestedVkHash <- firstExceptT SophieGovernanceCmdKeyReadError
      . newExceptT
      $ readVerificationKeyOrHashOrTextEnvFile AsVestedKey genVkOrHashOrFp
    vestedDelVkHash <-firstExceptT SophieGovernanceCmdKeyReadError
      . newExceptT
      $ readVerificationKeyOrHashOrTextEnvFile AsVestedDelegateKey genDelVkOrHashOrFp
    vrfVkHash <- firstExceptT SophieGovernanceCmdKeyReadError
      . newExceptT
      $ readVerificationKeyOrHashOrFile AsVrfKey vrfVkOrHashOrFp
    firstExceptT SophieGovernanceCmdTextEnvWriteError
      . newExceptT
      $ writeFileTextEnvelope oFp (Just genKeyDelegCertDesc)
      $ makeVestedKeyDelegationCertificate vestedVkHash vestedDelVkHash vrfVkHash
  where
    genKeyDelegCertDesc :: TextEnvelopeDescr
    genKeyDelegCertDesc = "Vested Key Delegation Certificate"

runGovernanceUpdateProposal
  :: OutputFile
  -> EpochNo
  -> [VerificationKeyFile]
  -- ^ Genesis verification keys
  -> ProtocolParametersUpdate
  -> ExceptT SophieGovernanceCmdError IO ()
runGovernanceUpdateProposal (OutputFile upFile) eNo genVerKeyFiles upPprams = do
    when (upPprams == mempty) $ left SophieGovernanceCmdEmptyUpdateProposalError
    genVKeys <- sequence
                  [ firstExceptT SophieGovernanceCmdTextEnvReadError . newExceptT $
                      readFileTextEnvelope
                        (AsVerificationKey AsGenesisKey)
                        vkeyFile
                  | VerificationKeyFile vkeyFile <- genVerKeyFiles ]
    let genKeyHashes = map verificationKeyHash genVKeys
        upProp = makeSophieUpdateProposal upPprams genKeyHashes eNo
    firstExceptT SophieGovernanceCmdTextEnvWriteError . newExceptT $
      writeFileTextEnvelope upFile Nothing upProp
