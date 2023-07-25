{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Consensus modes. The node supports several different modes with different
-- combinations of consensus protocols and ledger eras.
--
module Bcc.Api.Modes (

    -- * Consensus modes
    ColeMode,
    SophieMode,
    BccMode,
    ConsensusMode(..),
    AnyConsensusMode(..),
    renderMode,
    ConsensusModeIsMultiEra(..),

    -- * The eras supported by each mode
    EraInMode(..),
    eraInModeToEra,
    anyEraInModeToAnyEra,
    AnyEraInMode(..),
    toEraInMode,

    -- * Connection paramaters for each mode
    ConsensusModeParams(..),
    AnyConsensusModeParams(..),
    Cole.EpochSlots(..),

    -- * Conversions to and from types in the consensus library
    ConsensusBlockForMode,
    ConsensusBlockForEra,
    toConsensusEraIndex,
    fromConsensusEraIndex,
  ) where

import           Prelude

import           Bcc.Api.Eras
import           Bcc.Ledger.Crypto (StandardCrypto)

import           Data.SOP.Strict (K (K), NS (S, Z))
import           Data.Text (Text)

import qualified Shardagnostic.Consensus.Cole.Ledger as Consensus
import qualified Shardagnostic.Consensus.Bcc.Block as Consensus
import qualified Shardagnostic.Consensus.Bcc.ColeHFC as Consensus (ColeBlockHFC)
import           Shardagnostic.Consensus.HardFork.Combinator as Consensus (EraIndex (..), eraIndexSucc,
                   eraIndexZero)
import           Shardagnostic.Consensus.Sophie.Eras
                   (StandardSophie,
                    StandardEvie,
                    StandardJen,
                    StandardAurum)
import qualified Shardagnostic.Consensus.Sophie.Ledger as Consensus
import qualified Shardagnostic.Consensus.Sophie.SophieHFC as Consensus (SophieBlockHFC)

import qualified Bcc.Chain.Slotting as Cole (EpochSlots (..))

-- ----------------------------------------------------------------------------
-- Consensus modes
--

-- | The Cole-only consensus mode consists of only the Cole era.
--
-- This was used on the mainnet before the deployment of the multi-era
-- 'BccMode'. It is now of little practical use, though it illustrates
-- how a single-era consensus mode works. It may be sensible to remove this
-- at some stage.
--
data ColeMode

-- | The Sophie-only consensus mode consists of only the Sophie era.
--
-- This was used for the early Sophie testnets prior to the use of the
-- multi-era 'BccMode'. It is useful for setting up Sophie test networks
-- (e.g. for benchmarking) without having to go through the complication of the
-- hard fork from Cole to Sophie eras. It also shows how a single-era
-- consensus mode works. It may be replaced by other single-era modes in future.
--
data SophieMode

-- | The Bcc consensus mode consists of all the eras currently in use on
-- the Bcc mainnet. This is currently: the 'ColeEra'; 'SophieEra',
-- 'EvieEra' and 'JenEra', in that order.
--
-- This mode will be extended with new eras as the Bcc mainnet develops.
--
data BccMode

data AnyConsensusModeParams where
  AnyConsensusModeParams :: ConsensusModeParams mode -> AnyConsensusModeParams

deriving instance Show AnyConsensusModeParams

-- | This GADT provides a value-level representation of all the consensus modes.
-- This enables pattern matching on the era to allow them to be treated in a
-- non-uniform way.
--
data ConsensusMode mode where
     ColeMode   :: ConsensusMode ColeMode
     SophieMode :: ConsensusMode SophieMode
     BccMode :: ConsensusMode BccMode


deriving instance Show (ConsensusMode mode)

data AnyConsensusMode where
  AnyConsensusMode :: ConsensusMode mode -> AnyConsensusMode

deriving instance Show AnyConsensusMode

renderMode :: AnyConsensusMode -> Text
renderMode (AnyConsensusMode ColeMode) = "ColeMode"
renderMode (AnyConsensusMode SophieMode) = "SophieMode"
renderMode (AnyConsensusMode BccMode) = "BccMode"

-- | The subset of consensus modes that consist of multiple eras. Some features
-- are not supported in single-era modes (for exact compatibility with not
-- using the hard fork combinatior at all).
--
data ConsensusModeIsMultiEra mode where
     BccModeIsMultiEra :: ConsensusModeIsMultiEra BccMode

deriving instance Show (ConsensusModeIsMultiEra mode)

toEraInMode :: BccEra era -> ConsensusMode mode -> Maybe (EraInMode era mode)
toEraInMode ColeEra   ColeMode   = Just ColeEraInColeMode
toEraInMode SophieEra SophieMode = Just SophieEraInSophieMode
toEraInMode ColeEra   BccMode = Just ColeEraInBccMode
toEraInMode SophieEra BccMode = Just SophieEraInBccMode
toEraInMode EvieEra BccMode = Just EvieEraInBccMode
toEraInMode JenEra    BccMode = Just JenEraInBccMode
toEraInMode AurumEra  BccMode = Just AurumEraInBccMode
toEraInMode _ _                    = Nothing


-- | A representation of which 'BccEra's are included in each
-- 'ConsensusMode'.
--
data EraInMode era mode where
     ColeEraInColeMode     :: EraInMode ColeEra   ColeMode

     SophieEraInSophieMode :: EraInMode SophieEra SophieMode

     ColeEraInBccMode   :: EraInMode ColeEra   BccMode
     SophieEraInBccMode :: EraInMode SophieEra BccMode
     EvieEraInBccMode :: EraInMode EvieEra BccMode
     JenEraInBccMode    :: EraInMode JenEra    BccMode
     AurumEraInBccMode  :: EraInMode AurumEra  BccMode

deriving instance Show (EraInMode era mode)


eraInModeToEra :: EraInMode era mode -> BccEra era
eraInModeToEra ColeEraInColeMode     = ColeEra
eraInModeToEra SophieEraInSophieMode = SophieEra
eraInModeToEra ColeEraInBccMode   = ColeEra
eraInModeToEra SophieEraInBccMode = SophieEra
eraInModeToEra EvieEraInBccMode = EvieEra
eraInModeToEra JenEraInBccMode    = JenEra
eraInModeToEra AurumEraInBccMode  = AurumEra


data AnyEraInMode mode where
     AnyEraInMode :: EraInMode era mode -> AnyEraInMode mode

deriving instance Show (AnyEraInMode mode)


anyEraInModeToAnyEra :: AnyEraInMode mode -> AnyBccEra
anyEraInModeToAnyEra (AnyEraInMode erainmode) =
  case erainmode of
    ColeEraInColeMode     -> AnyBccEra ColeEra
    SophieEraInSophieMode -> AnyBccEra SophieEra
    ColeEraInBccMode   -> AnyBccEra ColeEra
    SophieEraInBccMode -> AnyBccEra SophieEra
    EvieEraInBccMode -> AnyBccEra EvieEra
    JenEraInBccMode    -> AnyBccEra JenEra
    AurumEraInBccMode  -> AnyBccEra AurumEra


-- | The consensus-mode-specific parameters needed to connect to a local node
-- that is using each consensus mode.
--
-- It is in fact only the Cole era that requires extra parameters, but this is
-- of course inherited by the 'BccMode' that uses the Cole era. The reason
-- this parameter is needed stems from unfortunate design decisions from the
-- legacy Cole era. The slots per epoch are needed to be able to /decode/
-- epoch boundary blocks from the Cole era.
--
-- It is possible in future that we may be able to eliminate this parameter by
-- discovering it from the node during the initial handshake.
--
data ConsensusModeParams mode where

     ColeModeParams
       :: Cole.EpochSlots
       -> ConsensusModeParams ColeMode

     SophieModeParams
       :: ConsensusModeParams SophieMode

     BccModeParams
       :: Cole.EpochSlots
       -> ConsensusModeParams BccMode

deriving instance Show (ConsensusModeParams mode)

-- ----------------------------------------------------------------------------
-- Consensus conversion functions
--

-- | A closed type family that maps between the consensus mode (from this API)
-- and the block type used by the consensus libraries.
--
type family ConsensusBlockForMode mode where
  ConsensusBlockForMode ColeMode   = Consensus.ColeBlockHFC
  ConsensusBlockForMode SophieMode = Consensus.SophieBlockHFC StandardSophie
  ConsensusBlockForMode BccMode = Consensus.BccBlock StandardCrypto

type family ConsensusBlockForEra era where
  ConsensusBlockForEra ColeEra   = Consensus.ColeBlock
  ConsensusBlockForEra SophieEra = Consensus.SophieBlock StandardSophie
  ConsensusBlockForEra EvieEra = Consensus.SophieBlock StandardEvie
  ConsensusBlockForEra JenEra    = Consensus.SophieBlock StandardJen
  ConsensusBlockForEra AurumEra  = Consensus.SophieBlock StandardAurum



eraIndex0 :: Consensus.EraIndex (x0 : xs)
eraIndex0 = Consensus.eraIndexZero

eraIndex1 :: Consensus.EraIndex (x1 : x0 : xs)
eraIndex1 = eraIndexSucc eraIndex0

eraIndex2 :: Consensus.EraIndex (x2 : x1 : x0 : xs)
eraIndex2 = eraIndexSucc eraIndex1

eraIndex3 :: Consensus.EraIndex (x3 : x2 : x1 : x0 : xs)
eraIndex3 = eraIndexSucc eraIndex2

eraIndex4 :: Consensus.EraIndex (x4 : x3 : x2 : x1 : x0 : xs)
eraIndex4 = eraIndexSucc eraIndex3

toConsensusEraIndex :: ConsensusBlockForMode mode ~ Consensus.HardForkBlock xs
                    => EraInMode era mode
                    -> Consensus.EraIndex xs
toConsensusEraIndex ColeEraInColeMode     = eraIndex0
toConsensusEraIndex SophieEraInSophieMode = eraIndex0

toConsensusEraIndex ColeEraInBccMode   = eraIndex0
toConsensusEraIndex SophieEraInBccMode = eraIndex1
toConsensusEraIndex EvieEraInBccMode = eraIndex2
toConsensusEraIndex JenEraInBccMode    = eraIndex3
toConsensusEraIndex AurumEraInBccMode  = eraIndex4


fromConsensusEraIndex :: ConsensusBlockForMode mode ~ Consensus.HardForkBlock xs
                      => ConsensusMode mode
                      -> Consensus.EraIndex xs
                      -> AnyEraInMode mode
fromConsensusEraIndex ColeMode = fromColeEraIndex
  where
    fromColeEraIndex :: Consensus.EraIndex
                           '[Consensus.ColeBlock]
                      -> AnyEraInMode ColeMode
    fromColeEraIndex (Consensus.EraIndex (Z (K ()))) =
      AnyEraInMode ColeEraInColeMode

fromConsensusEraIndex SophieMode = fromSophieEraIndex
  where
    fromSophieEraIndex :: Consensus.EraIndex
                             '[Consensus.SophieBlock StandardSophie]
                        -> AnyEraInMode SophieMode
    fromSophieEraIndex (Consensus.EraIndex (Z (K ()))) =
      AnyEraInMode SophieEraInSophieMode


fromConsensusEraIndex BccMode = fromSophieEraIndex
  where
    fromSophieEraIndex :: Consensus.EraIndex
                             (Consensus.BccEras StandardCrypto)
                        -> AnyEraInMode BccMode
    fromSophieEraIndex (Consensus.EraIndex (Z (K ()))) =
      AnyEraInMode ColeEraInBccMode

    fromSophieEraIndex (Consensus.EraIndex (S (Z (K ())))) =
      AnyEraInMode SophieEraInBccMode

    fromSophieEraIndex (Consensus.EraIndex (S (S (Z (K ()))))) =
      AnyEraInMode EvieEraInBccMode

    fromSophieEraIndex (Consensus.EraIndex (S (S (S (Z (K ())))))) =
      AnyEraInMode JenEraInBccMode

    fromSophieEraIndex (Consensus.EraIndex (S (S (S (S (Z (K ()))))))) =
      AnyEraInMode AurumEraInBccMode

