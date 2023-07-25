{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Bcc.Api.Protocol.Types
  ( BlockType(..)
  , Protocol(..)
  , ProtocolInfoArgs(..)
  , ProtocolClient(..)
  , ProtocolClientInfoArgs(..)
  , SomeNodeClientProtocol(..)
  ) where

import           Bcc.Prelude

import           Bcc.Chain.Slotting (EpochSlots)

import           Shardagnostic.Consensus.Bcc
import           Shardagnostic.Consensus.Bcc.Block
import           Shardagnostic.Consensus.Bcc.ColeHFC (ColeBlockHFC)
import           Shardagnostic.Consensus.Bcc.Node
import           Shardagnostic.Consensus.HardFork.Combinator.Embed.Unary
import           Shardagnostic.Consensus.Node.ProtocolInfo (ProtocolClientInfo (..), ProtocolInfo (..))
import           Shardagnostic.Consensus.Node.Run (RunNode)
import           Shardagnostic.Consensus.Sophie.SophieHFC (SophieBlockHFC)
import           Shardagnostic.Consensus.Util.IOLike (IOLike)

class (RunNode blk, IOLike m) => Protocol m blk where
  data ProtocolInfoArgs m blk
  protocolInfo :: ProtocolInfoArgs m blk -> ProtocolInfo m blk

-- | Node client support for each consensus protocol.
--
-- This is like 'Protocol' but for clients of the node, so with less onerous
-- requirements than to run a node.
--
class (RunNode blk) => ProtocolClient blk where
  data ProtocolClientInfoArgs blk
  protocolClientInfo :: ProtocolClientInfoArgs blk -> ProtocolClientInfo blk


-- | Run PBFT against the Cole ledger
instance IOLike m => Protocol m ColeBlockHFC where
  data ProtocolInfoArgs m ColeBlockHFC = ProtocolInfoArgsCole ProtocolParamsCole
  protocolInfo (ProtocolInfoArgsCole params) = inject $ protocolInfoCole params

instance IOLike m => Protocol m (BccBlock StandardCrypto) where
  data ProtocolInfoArgs m (BccBlock StandardCrypto) =
         ProtocolInfoArgsBcc
           ProtocolParamsCole
          (ProtocolParamsSophieBased StandardSophie)
          (ProtocolParamsSophie StandardCrypto)
          (ProtocolParamsEvie StandardCrypto)
          (ProtocolParamsJen StandardCrypto)
          (ProtocolParamsAurum StandardCrypto)
          (ProtocolTransitionParamsSophieBased StandardSophie)
          (ProtocolTransitionParamsSophieBased StandardEvie)
          (ProtocolTransitionParamsSophieBased StandardJen)
          (ProtocolTransitionParamsSophieBased StandardAurum)

  protocolInfo (ProtocolInfoArgsBcc
               paramsCole
               paramsSophieBased
               paramsSophie
               paramsEvie
               paramsJen
               paramsAurum
               paramsColeSophie
               paramsSophieEvie
               paramsEvieJen
               paramsJenAurum) =
    protocolInfoBcc
      paramsCole
      paramsSophieBased
      paramsSophie
      paramsEvie
      paramsJen
      paramsAurum
      paramsColeSophie
      paramsSophieEvie
      paramsEvieJen
      paramsJenAurum

instance ProtocolClient ColeBlockHFC where
  data ProtocolClientInfoArgs ColeBlockHFC =
    ProtocolClientInfoArgsCole EpochSlots
  protocolClientInfo (ProtocolClientInfoArgsCole epochSlots) =
    inject $ protocolClientInfoCole epochSlots

instance ProtocolClient (BccBlock StandardCrypto) where
  data ProtocolClientInfoArgs (BccBlock StandardCrypto) =
    ProtocolClientInfoArgsBcc EpochSlots
  protocolClientInfo (ProtocolClientInfoArgsBcc epochSlots) =
    protocolClientInfoBcc epochSlots

instance IOLike m => Protocol m (SophieBlockHFC StandardSophie) where
  data ProtocolInfoArgs m (SophieBlockHFC StandardSophie) = ProtocolInfoArgsSophie
    (ProtocolParamsSophieBased StandardSophie)
    (ProtocolParamsSophie StandardCrypto)
  protocolInfo (ProtocolInfoArgsSophie paramsSophieBased paramsSophie) =
    inject $ protocolInfoSophie paramsSophieBased paramsSophie

instance ProtocolClient (SophieBlockHFC StandardSophie) where
  data ProtocolClientInfoArgs (SophieBlockHFC StandardSophie) =
    ProtocolClientInfoArgsSophie
  protocolClientInfo ProtocolClientInfoArgsSophie =
    inject protocolClientInfoSophie

data BlockType blk where
  ColeBlockType :: BlockType ColeBlockHFC
  SophieBlockType :: BlockType (SophieBlockHFC StandardSophie)
  BccBlockType :: BlockType (BccBlock StandardCrypto)

deriving instance Eq (BlockType blk)
deriving instance Show (BlockType blk)

data SomeNodeClientProtocol where

     SomeNodeClientProtocol
       :: (RunNode blk, ProtocolClient blk)
       => ProtocolClientInfoArgs blk
       -> SomeNodeClientProtocol
