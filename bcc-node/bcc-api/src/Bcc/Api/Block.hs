{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | Blocks in the blockchain
--
module Bcc.Api.Block (

    -- * Blocks in the context of an era
    Block(.., Block),
    BlockHeader(..),

    -- ** Blocks in the context of a consensus mode
    BlockInMode(..),
    fromConsensusBlock,

    -- * Points on the chain
    ChainPoint(..),
    SlotNo(..),
    EpochNo(..),
    toConsensusPoint,
    fromConsensusPoint,
    toConsensusPointInMode,
    fromConsensusPointInMode,

    -- * Tip of the chain
    ChainTip(..),
    BlockNo(..),
    chainTipToChainPoint,
    fromConsensusTip,

    -- * Data family instances
    Hash(..),
  ) where

import           Prelude

import           Data.Aeson (ToJSON (..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import           Data.Foldable (Foldable (toList))

import           Bcc.Slotting.Block (BlockNo)
import           Bcc.Slotting.Slot (EpochNo, SlotNo)

import qualified Bcc.Crypto.Hash.Class
import qualified Bcc.Crypto.Hashing
import qualified Shardagnostic.Consensus.Block as Consensus
import qualified Shardagnostic.Consensus.Cole.Ledger as Consensus
import qualified Shardagnostic.Consensus.Bcc.Block as Consensus
import qualified Shardagnostic.Consensus.Bcc.ColeHFC as Consensus
import qualified Shardagnostic.Consensus.HardFork.Combinator as Consensus
import qualified Shardagnostic.Consensus.HardFork.Combinator.Degenerate as Consensus
import qualified Shardagnostic.Consensus.Sophie.Ledger as Consensus
import qualified Shardagnostic.Consensus.Sophie.SophieHFC as Consensus
import qualified Shardagnostic.Network.Block as Consensus

import qualified Bcc.Chain.Block as Cole
import qualified Bcc.Chain.UTxO as Cole
import qualified Bcc.Ledger.Era as Ledger
import qualified Bcc.Protocol.TOptimum.BHeader as Optimum
import qualified Sophie.Spec.Ledger.BlockChain as Ledger

import           Bcc.Api.Eras
import           Bcc.Api.HasTypeProxy
import           Bcc.Api.Hash
import           Bcc.Api.Modes
import           Bcc.Api.SerialiseRaw
import           Bcc.Api.Tx

{- HLINT ignore "Use lambda" -}
{- HLINT ignore "Use lambda-case" -}

-- ----------------------------------------------------------------------------
-- Blocks in an era
--

-- | A blockchain block in a particular Bcc era.
--
data Block era where

     ColeBlock :: Consensus.ColeBlock
                -> Block ColeEra

     SophieBlock :: SophieBasedEra era
                  -> Consensus.SophieBlock (SophieLedgerEra era)
                  -> Block era

-- | A block consists of a header and a body containing transactions.
--
pattern Block :: BlockHeader -> [Tx era] -> Block era
pattern Block header txs <- (getBlockHeaderAndTxs -> (header, txs))

{-# COMPLETE Block #-}

getBlockHeaderAndTxs :: Block era -> (BlockHeader, [Tx era])
getBlockHeaderAndTxs block = (getBlockHeader block, getBlockTxs block)

-- The GADT in the SophieBlock case requires a custom instance
instance Show (Block era) where
    showsPrec p (ColeBlock block) =
      showParen (p >= 11)
        ( showString "ColeBlock "
        . showsPrec 11 block
        )

    showsPrec p (SophieBlock SophieBasedEraSophie block) =
      showParen (p >= 11)
        ( showString "SophieBlock SophieBasedEraSophie "
        . showsPrec 11 block
        )

    showsPrec p (SophieBlock SophieBasedEraEvie block) =
      showParen (p >= 11)
        ( showString "SophieBlock SophieBasedEraEvie "
        . showsPrec 11 block
        )

    showsPrec p (SophieBlock SophieBasedEraJen block) =
      showParen (p >= 11)
        ( showString "SophieBlock SophieBasedEraJen "
        . showsPrec 11 block
        )

    showsPrec p (SophieBlock SophieBasedEraAurum block) =
      showParen (p >= 11)
        ( showString "SophieBlock SophieBasedEraAurum "
        . showsPrec 11 block
        )

getBlockTxs :: forall era . Block era -> [Tx era]
getBlockTxs (ColeBlock Consensus.ColeBlock { Consensus.coleBlockRaw }) =
    case coleBlockRaw of
      Cole.ABOBBoundary{} -> [] -- no txs in EBBs
      Cole.ABOBBlock Cole.ABlock {
          Cole.blockBody =
            Cole.ABody {
              Cole.bodyTxPayload = Cole.ATxPayload txs
            }
        } -> map ColeTx txs
getBlockTxs (SophieBlock era Consensus.SophieBlock{Consensus.sophieBlockRaw}) =
    obtainConsensusSophieBasedEra era $
      getSophieBlockTxs era sophieBlockRaw

getSophieBlockTxs :: forall era ledgerera.
                      ledgerera ~ SophieLedgerEra era
                   => Consensus.SophieBasedEra ledgerera
                   => SophieBasedEra era
                   -> Ledger.Block ledgerera
                   -> [Tx era]
getSophieBlockTxs era (Ledger.Block _header txs) =
  [ SophieTx era txinblock
  | txinblock <- toList (Ledger.fromTxSeq txs) ]

obtainConsensusSophieBasedEra
  :: forall era ledgerera a.
     ledgerera ~ SophieLedgerEra era
  => SophieBasedEra era
  -> (Consensus.SophieBasedEra ledgerera => a) -> a
obtainConsensusSophieBasedEra SophieBasedEraSophie f = f
obtainConsensusSophieBasedEra SophieBasedEraEvie f = f
obtainConsensusSophieBasedEra SophieBasedEraJen    f = f
obtainConsensusSophieBasedEra SophieBasedEraAurum  f = f


-- ----------------------------------------------------------------------------
-- Block in a consensus mode
--

-- | A 'Block' in one of the eras supported by a given protocol mode.
--
-- For multi-era modes such as the 'BccMode' this type is a sum of the
-- different block types for all the eras. It is used in the ChainSync protocol.
--
data BlockInMode mode where
     BlockInMode :: Block era -> EraInMode era mode -> BlockInMode mode

deriving instance Show (BlockInMode mode)


fromConsensusBlock :: ConsensusBlockForMode mode ~ block
                   => ConsensusMode mode -> block -> BlockInMode mode
fromConsensusBlock ColeMode =
    \b -> case b of
      Consensus.DegenBlock b' ->
        BlockInMode (ColeBlock b') ColeEraInColeMode

fromConsensusBlock SophieMode =
    \b -> case b of
      Consensus.DegenBlock b' ->
        BlockInMode (SophieBlock SophieBasedEraSophie b')
                     SophieEraInSophieMode

fromConsensusBlock BccMode =
    \b -> case b of
      Consensus.BlockCole b' ->
        BlockInMode (ColeBlock b') ColeEraInBccMode

      Consensus.BlockSophie b' ->
        BlockInMode (SophieBlock SophieBasedEraSophie b')
                     SophieEraInBccMode

      Consensus.BlockEvie b' ->
        BlockInMode (SophieBlock SophieBasedEraEvie b')
                     EvieEraInBccMode

      Consensus.BlockJen b' ->
        BlockInMode (SophieBlock SophieBasedEraJen b')
                     JenEraInBccMode

      Consensus.BlockAurum b' ->
        BlockInMode (SophieBlock SophieBasedEraAurum b')
                     AurumEraInBccMode

-- ----------------------------------------------------------------------------
-- Block headers
--

data BlockHeader = BlockHeader !SlotNo
                               !(Hash BlockHeader)
                               !BlockNo

-- | For now at least we use a fixed concrete hash type for all modes and era.
-- The different eras do use different types, but it's all the same underlying
-- representation.
newtype instance Hash BlockHeader = HeaderHash SBS.ShortByteString
  deriving (Eq, Ord, Show)

instance SerialiseAsRawBytes (Hash BlockHeader) where
    serialiseToRawBytes (HeaderHash bs) = SBS.fromShort bs

    deserialiseFromRawBytes (AsHash AsBlockHeader) bs
      | BS.length bs == 32 = Just $! HeaderHash (SBS.toShort bs)
      | otherwise          = Nothing

instance HasTypeProxy BlockHeader where
    data AsType BlockHeader = AsBlockHeader
    proxyToAsType _ = AsBlockHeader

getBlockHeader :: forall era . Block era -> BlockHeader
getBlockHeader (SophieBlock sophieEra block) = case sophieEra of
  SophieBasedEraSophie -> go
  SophieBasedEraEvie -> go
  SophieBasedEraJen -> go
  SophieBasedEraAurum -> go
  where
    go :: Consensus.SophieBasedEra (SophieLedgerEra era) => BlockHeader
    go = BlockHeader headerFieldSlot (HeaderHash hashSBS) headerFieldBlockNo
      where
        Consensus.HeaderFields {
            Consensus.headerFieldHash
              = Consensus.SophieHash (Optimum.HashHeader (Bcc.Crypto.Hash.Class.UnsafeHash hashSBS)),
            Consensus.headerFieldSlot,
            Consensus.headerFieldBlockNo
          } = Consensus.getHeaderFields block
getBlockHeader (ColeBlock block)
  = BlockHeader
      headerFieldSlot
      (HeaderHash $ Bcc.Crypto.Hashing.abstractHashToShort coleHeaderHash)
      headerFieldBlockNo
  where
    Consensus.HeaderFields {
      Consensus.headerFieldHash = Consensus.ColeHash coleHeaderHash,
      Consensus.headerFieldSlot,
      Consensus.headerFieldBlockNo
    } = Consensus.getHeaderFields block


-- ----------------------------------------------------------------------------
-- Chain points
--

data ChainPoint = ChainPointAtGenesis
                | ChainPoint !SlotNo !(Hash BlockHeader)
  deriving (Eq, Show)


toConsensusPointInMode :: ConsensusMode mode
                       -> ChainPoint
                       -> Consensus.Point (ConsensusBlockForMode mode)
-- It's the same concrete impl in all cases, but we have to show
-- individually for each case that we satisfy the type equality constraint
-- HeaderHash block ~ OneEraHash xs
toConsensusPointInMode ColeMode   = toConsensusPointHF
toConsensusPointInMode SophieMode = toConsensusPointHF
toConsensusPointInMode BccMode = toConsensusPointHF

fromConsensusPointInMode :: ConsensusMode mode
                         -> Consensus.Point (ConsensusBlockForMode mode)
                         -> ChainPoint
fromConsensusPointInMode ColeMode   = fromConsensusPointHF
fromConsensusPointInMode SophieMode = fromConsensusPointHF
fromConsensusPointInMode BccMode = fromConsensusPointHF


-- | Convert a 'Consensus.Point' for multi-era block type
--
toConsensusPointHF :: Consensus.HeaderHash block ~ Consensus.OneEraHash xs
                   => ChainPoint -> Consensus.Point block
toConsensusPointHF  ChainPointAtGenesis = Consensus.GenesisPoint
toConsensusPointHF (ChainPoint slot (HeaderHash h)) =
    Consensus.BlockPoint slot (Consensus.OneEraHash h)

-- | Convert a 'Consensus.Point' for multi-era block type
--
fromConsensusPointHF :: Consensus.HeaderHash block ~ Consensus.OneEraHash xs
                   => Consensus.Point block -> ChainPoint
fromConsensusPointHF Consensus.GenesisPoint = ChainPointAtGenesis
fromConsensusPointHF (Consensus.BlockPoint slot (Consensus.OneEraHash h)) =
    ChainPoint slot (HeaderHash h)

-- | Convert a 'Consensus.Point' for single Sophie-era block type
--
toConsensusPoint :: forall ledgerera.
                      Consensus.SophieBasedEra ledgerera
                   => ChainPoint
                   -> Consensus.Point (Consensus.SophieBlock ledgerera)
toConsensusPoint ChainPointAtGenesis = Consensus.GenesisPoint
toConsensusPoint (ChainPoint slot (HeaderHash h)) =
    Consensus.BlockPoint slot (Consensus.fromShortRawHash proxy h)
  where
    proxy :: Proxy (Consensus.SophieBlock ledgerera)
    proxy = Proxy

-- | Convert a 'Consensus.Point' for single Sophie-era block type
--
fromConsensusPoint :: forall ledgerera.
                      Consensus.SophieBasedEra ledgerera
                   => Consensus.Point (Consensus.SophieBlock ledgerera)
                   -> ChainPoint
fromConsensusPoint Consensus.GenesisPoint = ChainPointAtGenesis
fromConsensusPoint (Consensus.BlockPoint slot h) =
    ChainPoint slot (HeaderHash (Consensus.toShortRawHash proxy h))
  where
    proxy :: Proxy (Consensus.SophieBlock ledgerera)
    proxy = Proxy


-- ----------------------------------------------------------------------------
-- Chain tips
--

-- | This is like a 'ChainPoint' but is conventionally used for the tip of the
-- chain: that is the most recent block at the end of the chain.
--
-- It also carries the 'BlockNo' of the chain tip.
--
data ChainTip = ChainTipAtGenesis
              | ChainTip !SlotNo !(Hash BlockHeader) !BlockNo
  deriving (Eq, Show)

instance ToJSON ChainTip where
  toJSON ChainTipAtGenesis = Aeson.Null
  toJSON (ChainTip slot headerHash (Consensus.BlockNo bNum)) =
    object [ "slot" .= slot
           , "hash" .= serialiseToRawBytesHexText headerHash
           , "block" .= bNum
           ]

chainTipToChainPoint :: ChainTip -> ChainPoint
chainTipToChainPoint ChainTipAtGenesis = ChainPointAtGenesis
chainTipToChainPoint (ChainTip s h _)  = ChainPoint s h


fromConsensusTip  :: ConsensusBlockForMode mode ~ block
                  => ConsensusMode mode
                  -> Consensus.Tip block
                  -> ChainTip
fromConsensusTip ColeMode = conv
  where
    conv :: Consensus.Tip Consensus.ColeBlockHFC -> ChainTip
    conv Consensus.TipGenesis = ChainTipAtGenesis
    conv (Consensus.Tip slot (Consensus.OneEraHash h) block) =
      ChainTip slot (HeaderHash h) block

fromConsensusTip SophieMode = conv
  where
    conv :: Consensus.Tip (Consensus.SophieBlockHFC Consensus.StandardSophie)
         -> ChainTip
    conv Consensus.TipGenesis = ChainTipAtGenesis
    conv (Consensus.Tip slot (Consensus.OneEraHash h) block) =
      ChainTip slot (HeaderHash h) block

fromConsensusTip BccMode = conv
  where
    conv :: Consensus.Tip (Consensus.BccBlock Consensus.StandardCrypto)
         -> ChainTip
    conv Consensus.TipGenesis = ChainTipAtGenesis
    conv (Consensus.Tip slot (Consensus.OneEraHash h) block) =
      ChainTip slot (HeaderHash h) block

{-
TODO: In principle we should be able to use this common implementation rather
      than repeating it for each mode above. It does actually type-check. The
      problem is that (at least with ghc-8.10.x) ghc's pattern match warning
      mechanism cannot see that the OneEraHash is a complete pattern match.
      I'm guessing that while the type checker can use the type equality to
      see that OneEraHash is a valid pattern, the exhaustiveness checker is for
      some reason not able to use it to see that it is indeed the only pattern.
fromConsensusTip =
    \mode -> case mode of
      ColeMode   -> conv
      SophieMode -> conv
      BccMode -> conv
  where
    conv :: HeaderHash block ~ OneEraHash xs
         => Tip block -> ChainTip
    conv TipGenesis                      = ChainTipAtGenesis
    conv (Tip slot (OneEraHash h) block) = ChainTip slot (HeaderHash h) block
-}
