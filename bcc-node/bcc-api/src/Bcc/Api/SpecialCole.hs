{-# LANGUAGE TypeFamilies #-}

-- | Special Cole values that we can submit to a node to propose an update proposal
-- or to vote on an update proposal. These are not transactions.
--
module Bcc.Api.SpecialCole
  ( ColeUpdateProposal(..),
    ColeProtocolParametersUpdate(..),
    AsType(AsColeUpdateProposal, AsColeVote),
    makeProtocolParametersUpdate,
    toColeLedgerUpdateProposal,
    ColeVote(..),
    makeColeUpdateProposal,
    makeColeVote,
    toColeLedgertoColeVote,
  ) where

import           Prelude

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map.Strict as M
import           Data.Word
import           Numeric.Natural

import           Bcc.Api.HasTypeProxy
import           Bcc.Api.KeysCole
import           Bcc.Api.NetworkId (NetworkId, toColeProtocolMagicId)
import           Bcc.Api.SerialiseRaw

import qualified Bcc.Binary as Binary
import           Bcc.Chain.Common (EntropicPortion, TxFeePolicy)
import           Bcc.Chain.Slotting
import           Bcc.Chain.Update (AProposal (aBody, annotation), InstallerHash,
                     ProposalBody (ProposalBody), ProtocolParametersUpdate (..), ProtocolVersion,
                     SoftforkRule, SoftwareVersion, SystemTag, UpId, mkVote, recoverUpId,
                     recoverVoteId, signProposal)
import qualified Bcc.Chain.Update.Vote as ColeVote
import           Bcc.Crypto (SafeSigner, noPassSafeSigner)

import           Shardagnostic.Consensus.Cole.Ledger.Block (ColeBlock)
import qualified Shardagnostic.Consensus.Cole.Ledger.Mempool as Mempool

{- HLINT ignore "Use void" -}

-- | Cole era update proposal

newtype ColeUpdateProposal =
    ColeUpdateProposal { unColeUpdateProposal :: AProposal ByteString}
  deriving (Eq, Show)

instance HasTypeProxy ColeUpdateProposal where
  data AsType ColeUpdateProposal = AsColeUpdateProposal
  proxyToAsType _ = AsColeUpdateProposal

instance SerialiseAsRawBytes ColeUpdateProposal where
  serialiseToRawBytes (ColeUpdateProposal proposal) = annotation proposal
  deserialiseFromRawBytes AsColeUpdateProposal bs =
    let lBs = LB.fromStrict bs
    in case Binary.decodeFull lBs of
        Left _deserFail -> Nothing
        Right proposal -> Just (ColeUpdateProposal proposal')
          where
            proposal' :: AProposal ByteString
            proposal' = Binary.annotationBytes lBs proposal

makeColeUpdateProposal
  :: NetworkId
  -> ProtocolVersion
  -> SoftwareVersion
  -> SystemTag
  -> InstallerHash
  -> SomeColeSigningKey
  -> ColeProtocolParametersUpdate
  -> ColeUpdateProposal
makeColeUpdateProposal nId pVer sVer sysTag insHash
                          bWit paramsToUpdate =
  let nonAnnotatedProposal :: AProposal ()
      nonAnnotatedProposal = signProposal (toColeProtocolMagicId nId) proposalBody noPassSigningKey
      annotatedPropBody :: Binary.Annotated ProposalBody ByteString
      annotatedPropBody = Binary.reAnnotate $ aBody nonAnnotatedProposal
  in ColeUpdateProposal
       $ nonAnnotatedProposal { aBody = annotatedPropBody
                              , annotation = Binary.serialize' nonAnnotatedProposal
                              }
 where
   proposalBody :: ProposalBody
   proposalBody = ProposalBody pVer protocolParamsUpdate sVer metaData

   metaData :: M.Map SystemTag InstallerHash
   metaData = M.singleton sysTag insHash

   noPassSigningKey :: SafeSigner
   noPassSigningKey = noPassSafeSigner $ toColeSigningKey bWit

   protocolParamsUpdate :: ProtocolParametersUpdate
   protocolParamsUpdate = makeProtocolParametersUpdate paramsToUpdate

data ColeProtocolParametersUpdate =
  ColeProtocolParametersUpdate
    { bPpuScriptVersion     :: !(Maybe Word16)
      -- ^ Redundant. This was meant to be the version of the
      -- Zerepoch smart contract language, however, there are no
      -- smart contracts nor scripts in the Cole era.
    , bPpuSlotDuration      :: !(Maybe Natural)
      -- ^ Slot duration in milliseconds.
    , bPpuMaxBlockSize      :: !(Maybe Natural)
      -- ^ Maximum block size in bytes.
    , bPpuMaxHeaderSize     :: !(Maybe Natural)
      -- ^ Maximum block header size in bytes.
    , bPpuMaxTxSize         :: !(Maybe Natural)
      -- ^ Maxiumum transaction size in bytes.
    , bPpuMaxProposalSize   :: !(Maybe Natural)
      -- ^ Maximum update proposal size in bytes.
    , bPpuMpcThd            :: !(Maybe EntropicPortion)
    , bPpuHeavyDelThd       :: !(Maybe EntropicPortion)
      -- ^ Heavyweight delegation threshold. The delegate (i.e stakeholder)
      -- must possess no less than this threshold of stake in order to participate
      -- in heavyweight delegation.
    , bPpuUpdateVoteThd     :: !(Maybe EntropicPortion)
    , bPpuUpdateProposalThd :: !(Maybe EntropicPortion)
    , bPpuUpdateProposalTTL :: !(Maybe SlotNumber)
    , bPpuSoftforkRule      :: !(Maybe SoftforkRule)
      -- ^ Values defining the softfork resolution rule. When the stake belonging
      -- to block issuers, issuing a given block version, is greater than the
      -- current softfork resolution threshold, this block version is adopted.
    , bPpuTxFeePolicy       :: !(Maybe TxFeePolicy)
      -- ^ Transaction fee policy represents a formula to compute the minimal allowed
      -- Fee for a transaction. Transactions with lesser fees won't be accepted.
    , bPpuUnlockStakeEpoch  :: !(Maybe EpochNumber)
      -- ^ This has been re-purposed for unlocking the ShardagnosticBFT logic in the software.
      -- Relevant: [CDEC-610](https://tbco.myjetbrains.com/youtrack/issue/CDEC-610)
    } deriving Show

makeProtocolParametersUpdate
  :: ColeProtocolParametersUpdate
  -> ProtocolParametersUpdate
makeProtocolParametersUpdate apiPpu =
  ProtocolParametersUpdate
    { ppuScriptVersion = bPpuScriptVersion apiPpu
    , ppuSlotDuration = bPpuSlotDuration apiPpu
    , ppuMaxBlockSize = bPpuMaxBlockSize apiPpu
    , ppuMaxHeaderSize = bPpuMaxHeaderSize apiPpu
    , ppuMaxTxSize = bPpuMaxTxSize apiPpu
    , ppuMaxProposalSize = bPpuMaxProposalSize apiPpu
    , ppuMpcThd = bPpuMpcThd apiPpu
    , ppuHeavyDelThd = bPpuHeavyDelThd apiPpu
    , ppuUpdateVoteThd = bPpuUpdateVoteThd apiPpu
    , ppuUpdateProposalThd = bPpuUpdateProposalThd apiPpu
    , ppuUpdateProposalTTL = bPpuUpdateProposalTTL apiPpu
    , ppuSoftforkRule = bPpuSoftforkRule apiPpu
    , ppuTxFeePolicy = bPpuTxFeePolicy apiPpu
    , ppuUnlockStakeEpoch = bPpuUnlockStakeEpoch apiPpu
    }

toColeLedgerUpdateProposal :: ColeUpdateProposal -> Mempool.GenTx ColeBlock
toColeLedgerUpdateProposal (ColeUpdateProposal proposal) =
  Mempool.ColeUpdateProposal (recoverUpId proposal) proposal

-- | Cole era votes

newtype ColeVote = ColeVote { unColeVote :: ColeVote.AVote ByteString }
  deriving (Eq, Show)

instance HasTypeProxy ColeVote where
  data AsType ColeVote = AsColeVote
  proxyToAsType _ = AsColeVote

instance SerialiseAsRawBytes ColeVote where
  serialiseToRawBytes (ColeVote vote) = Binary.serialize' $ fmap (const ()) vote
  deserialiseFromRawBytes AsColeVote bs =
    let lBs = LB.fromStrict bs
    in case Binary.decodeFull lBs of
         Left _deserFail -> Nothing
         Right vote -> Just . ColeVote $ annotateVote vote lBs
   where
    annotateVote :: ColeVote.AVote Binary.ByteSpan -> LB.ByteString -> ColeVote.AVote ByteString
    annotateVote vote bs' = Binary.annotationBytes bs' vote


makeColeVote
  :: NetworkId
  -> SomeColeSigningKey
  -> ColeUpdateProposal
  -> Bool
  -> ColeVote
makeColeVote nId sKey (ColeUpdateProposal proposal) yesOrNo =
  let signingKey = toColeSigningKey sKey
      nonAnnotatedVote :: ColeVote.AVote ()
      nonAnnotatedVote = mkVote (toColeProtocolMagicId nId) signingKey (recoverUpId proposal) yesOrNo
      annotatedProposalId :: Binary.Annotated UpId ByteString
      annotatedProposalId = Binary.reAnnotate $ ColeVote.aProposalId nonAnnotatedVote
  in ColeVote
       $ nonAnnotatedVote { ColeVote.aProposalId = annotatedProposalId
                          , ColeVote.annotation = Binary.annotation annotatedProposalId
                          }

toColeLedgertoColeVote :: ColeVote -> Mempool.GenTx ColeBlock
toColeLedgertoColeVote (ColeVote vote) = Mempool.ColeUpdateVote (recoverVoteId vote) vote
