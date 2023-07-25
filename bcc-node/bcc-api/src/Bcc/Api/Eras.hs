{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}


-- | Bcc eras, sometimes we have to distinguish them.
--
module Bcc.Api.Eras
  ( -- * Eras
    ColeEra
  , SophieEra
  , EvieEra
  , JenEra
  , AurumEra
  , BccEra(..)
  , IsBccEra(..)
  , AnyBccEra(..)
  , anyBccEra
  , InAnyBccEra(..)

    -- * Deprecated aliases
  , Cole
  , Sophie
  , Evie
  , Jen

    -- * Sophie-based eras
  , SophieBasedEra(..)
  , IsSophieBasedEra(..)
  , InAnySophieBasedEra(..)
  , sophieBasedToBccEra

    -- ** Mapping to era types from the Sophie ledger library
  , SophieLedgerEra

    -- * Bcc eras, as Cole vs Sophie-based
  , BccEraStyle(..)
  , bccEraStyle

    -- * Data family instances
  , AsType(AsColeEra, AsSophieEra, AsEvieEra, AsJenEra, AsAurumEra,
           AsCole,    AsSophie,    AsEvie,    AsJen)
  ) where

import           Prelude

import           Data.Aeson (ToJSON, toJSON)
import           Data.Type.Equality (TestEquality (..), (:~:) (Refl))

import           Shardagnostic.Consensus.Sophie.Eras as Ledger
                   (StandardSophie,
                    StandardEvie,
                    StandardJen,
                    StandardAurum)

import           Bcc.Api.HasTypeProxy


-- | A type used as a tag to distinguish the Cole era.
data ColeEra

-- | A type used as a tag to distinguish the Sophie era.
data SophieEra

-- | A type used as a tag to distinguish the Evie era.
data EvieEra

-- | A type used as a tag to distinguish the Jen era.
data JenEra

-- | A type used as a tag to distinguish the Aurum era.
data AurumEra

instance HasTypeProxy ColeEra where
    data AsType ColeEra = AsColeEra
    proxyToAsType _ = AsColeEra

instance HasTypeProxy SophieEra where
    data AsType SophieEra = AsSophieEra
    proxyToAsType _ = AsSophieEra

instance HasTypeProxy EvieEra where
    data AsType EvieEra = AsEvieEra
    proxyToAsType _ = AsEvieEra

instance HasTypeProxy JenEra where
    data AsType JenEra = AsJenEra
    proxyToAsType _ = AsJenEra

instance HasTypeProxy AurumEra where
    data AsType AurumEra = AsAurumEra
    proxyToAsType _ = AsAurumEra


-- ----------------------------------------------------------------------------
-- Deprecated aliases
--

type Cole   = ColeEra
type Sophie = SophieEra
type Evie = EvieEra
type Jen    = JenEra

{-# DEPRECATED Cole   "Use 'ColeEra' or 'ColeAddr' as appropriate" #-}
{-# DEPRECATED Sophie "Use 'SophieEra' or 'SophieAddr' as appropriate" #-}
{-# DEPRECATED Evie "Use 'EvieEra' instead" #-}
{-# DEPRECATED Jen    "Use 'JenEra' instead" #-}

pattern AsCole   :: AsType ColeEra
pattern AsCole    = AsColeEra

pattern AsSophie :: AsType SophieEra
pattern AsSophie  = AsSophieEra

pattern AsEvie :: AsType EvieEra
pattern AsEvie  = AsEvieEra

pattern AsJen    :: AsType JenEra
pattern AsJen     = AsJenEra

{-# DEPRECATED AsCole   "Use 'AsColeEra' instead" #-}
{-# DEPRECATED AsSophie "Use 'AsSophieEra' instead" #-}
{-# DEPRECATED AsEvie "Use 'AsEvieEra' instead" #-}
{-# DEPRECATED AsJen    "Use 'AsJenEra' instead" #-}

-- ----------------------------------------------------------------------------
-- Value level representation for Bcc eras
--

-- | This GADT provides a value-level representation of all the Bcc eras.
-- This enables pattern matching on the era to allow them to be treated in a
-- non-uniform way.
--
-- This can be used in combination with the 'IsBccEra' class to get access
-- to this value.
--
-- In combination this can often enable code that handles all eras, and does
-- so uniformly where possible, and non-uniformly where necessary.
--
data BccEra era where
     ColeEra   :: BccEra ColeEra
     SophieEra :: BccEra SophieEra
     EvieEra :: BccEra EvieEra
     JenEra    :: BccEra JenEra
     AurumEra  :: BccEra AurumEra

deriving instance Eq   (BccEra era)
deriving instance Ord  (BccEra era)
deriving instance Show (BccEra era)

instance ToJSON (BccEra era) where
   toJSON ColeEra   = "Cole"
   toJSON SophieEra = "Sophie"
   toJSON EvieEra = "Evie"
   toJSON JenEra    = "Jen"
   toJSON AurumEra  = "Aurum"

instance TestEquality BccEra where
    testEquality ColeEra   ColeEra   = Just Refl
    testEquality SophieEra SophieEra = Just Refl
    testEquality EvieEra EvieEra = Just Refl
    testEquality JenEra    JenEra    = Just Refl
    testEquality AurumEra  AurumEra  = Just Refl
    testEquality _          _          = Nothing


-- | The class of Bcc eras. This allows uniform handling of all Bcc
-- eras, but also non-uniform by making case distinctions on the 'BccEra'
-- constructors, or the 'BccEraStyle' constructors via `bccEraStyle`.
--
class HasTypeProxy era => IsBccEra era where
   bccEra      :: BccEra era

instance IsBccEra ColeEra where
   bccEra      = ColeEra

instance IsBccEra SophieEra where
   bccEra      = SophieEra

instance IsBccEra EvieEra where
   bccEra      = EvieEra

instance IsBccEra JenEra where
   bccEra      = JenEra

instance IsBccEra AurumEra where
   bccEra      = AurumEra

data AnyBccEra where
     AnyBccEra :: IsBccEra era  -- Provide class constraint
                   => BccEra era    -- and explicit value.
                   -> AnyBccEra

deriving instance Show AnyBccEra

instance Eq AnyBccEra where
    AnyBccEra era == AnyBccEra era' =
      case testEquality era era' of
        Nothing   -> False
        Just Refl -> True -- since no constructors share types

instance ToJSON AnyBccEra where
   toJSON (AnyBccEra era) = toJSON era

-- | Like the 'AnyBccEra' constructor but does not demand a 'IsBccEra'
-- class constraint.
--
anyBccEra :: BccEra era -> AnyBccEra
anyBccEra ColeEra   = AnyBccEra ColeEra
anyBccEra SophieEra = AnyBccEra SophieEra
anyBccEra EvieEra = AnyBccEra EvieEra
anyBccEra JenEra    = AnyBccEra JenEra
anyBccEra AurumEra  = AnyBccEra AurumEra

-- | This pairs up some era-dependent type with a 'BccEra' value that tells
-- us what era it is, but hides the era type. This is useful when the era is
-- not statically known, for example when deserialising from a file.
--
data InAnyBccEra thing where
     InAnyBccEra :: IsBccEra era  -- Provide class constraint
                     => BccEra era    -- and explicit value.
                     -> thing era
                     -> InAnyBccEra thing


-- ----------------------------------------------------------------------------
-- Sophie-based eras
--

-- | While the Cole and Sophie eras are quite different, there are several
-- eras that are based on Sophie with only minor differences. It is useful
-- to be able to treat the Sophie-based eras in a mostly-uniform way.
--
-- Values of this type witness the fact that the era is Sophie-based. This
-- can be used to constrain the era to being a Sophie-based on. It allows
-- non-uniform handling making case distinctions on the constructor.
--
data SophieBasedEra era where
     SophieBasedEraSophie :: SophieBasedEra SophieEra
     SophieBasedEraEvie :: SophieBasedEra EvieEra
     SophieBasedEraJen    :: SophieBasedEra JenEra
     SophieBasedEraAurum  :: SophieBasedEra AurumEra

deriving instance Eq   (SophieBasedEra era)
deriving instance Ord  (SophieBasedEra era)
deriving instance Show (SophieBasedEra era)

-- | The class of eras that are based on Sophie. This allows uniform handling
-- of Sophie-based eras, but also non-uniform by making case distinctions on
-- the 'SophieBasedEra' constructors.
--
class IsBccEra era => IsSophieBasedEra era where
   sophieBasedEra :: SophieBasedEra era

instance IsSophieBasedEra SophieEra where
   sophieBasedEra = SophieBasedEraSophie

instance IsSophieBasedEra EvieEra where
   sophieBasedEra = SophieBasedEraEvie

instance IsSophieBasedEra JenEra where
   sophieBasedEra = SophieBasedEraJen

instance IsSophieBasedEra AurumEra where
   sophieBasedEra = SophieBasedEraAurum

-- | This pairs up some era-dependent type with a 'SophieBasedEra' value that
-- tells us what era it is, but hides the era type. This is useful when the era
-- is not statically known, for example when deserialising from a file.
--
data InAnySophieBasedEra thing where
     InAnySophieBasedEra :: IsSophieBasedEra era -- Provide class constraint
                          => SophieBasedEra era   -- and explicit value.
                          -> thing era
                          -> InAnySophieBasedEra thing


-- | Converts a 'SophieBasedEra' to the broader 'BccEra'.
sophieBasedToBccEra :: SophieBasedEra era -> BccEra era
sophieBasedToBccEra SophieBasedEraSophie = SophieEra
sophieBasedToBccEra SophieBasedEraEvie = EvieEra
sophieBasedToBccEra SophieBasedEraJen    = JenEra
sophieBasedToBccEra SophieBasedEraAurum  = AurumEra


-- ----------------------------------------------------------------------------
-- Bcc eras factored as Cole vs Sophie-based
--

-- | This is the same essential information as 'BccEra' but instead of a
-- flat set of alternative eras, it is factored into the legcy Cole era and
-- the current Sophie-based eras.
--
-- This way of factoring the eras is useful because in many cases the
-- major differences are between the Cole and Sophie-based eras, and
-- the Sophie-based eras can often be treated uniformly.
--
data BccEraStyle era where
     LegacyColeEra  :: BccEraStyle ColeEra
     SophieBasedEra :: IsSophieBasedEra era -- Also provide class constraint
                     => SophieBasedEra era
                     -> BccEraStyle era

deriving instance Eq   (BccEraStyle era)
deriving instance Ord  (BccEraStyle era)
deriving instance Show (BccEraStyle era)

-- | The 'BccEraStyle' for a 'BccEra'.
--
bccEraStyle :: BccEra era -> BccEraStyle era
bccEraStyle ColeEra   = LegacyColeEra
bccEraStyle SophieEra = SophieBasedEra SophieBasedEraSophie
bccEraStyle EvieEra = SophieBasedEra SophieBasedEraEvie
bccEraStyle JenEra    = SophieBasedEra SophieBasedEraJen
bccEraStyle AurumEra  = SophieBasedEra SophieBasedEraAurum


-- ----------------------------------------------------------------------------
-- Conversion to Sophie ledger library types
--

-- | A type family that connects our era type tags to equivalent type tags used
-- in the Sophie ledger library.
--
-- This type mapping  connect types from this API with types in the Sophie
-- ledger library which allows writing conversion functions in a more generic
-- way.
--
type family SophieLedgerEra era where

  SophieLedgerEra SophieEra = Ledger.StandardSophie
  SophieLedgerEra EvieEra = Ledger.StandardEvie
  SophieLedgerEra JenEra    = Ledger.StandardJen
  SophieLedgerEra AurumEra  = Ledger.StandardAurum

