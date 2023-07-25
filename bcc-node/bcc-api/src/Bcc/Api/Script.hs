{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Bcc.Api.Script (
    -- * Languages
    SimpleScriptV1,
    SimpleScriptV2,
    ZerepochScriptV1,
    ScriptLanguage(..),
    SimpleScriptVersion(..),
    ZerepochScriptVersion(..),
    AnyScriptLanguage(..),
    AnyZerepochScriptVersion(..),
    IsScriptLanguage(..),
    IsSimpleScriptLanguage(..),

    -- * Scripts in a specific language
    Script(..),

    -- * Scripts in any language
    ScriptInAnyLang(..),
    toScriptInAnyLang,

    -- * Scripts in an era
    ScriptInEra(..),
    toScriptInEra,
    eraOfScriptInEra,

    -- * Use of a script in an era as a witness
    WitCtxTxIn, WitCtxMint, WitCtxStake,
    WitCtx(..),
    ScriptWitness(..),
    Witness(..),
    KeyWitnessInCtx(..),
    ScriptWitnessInCtx(..),
    ScriptDatum(..),
    ScriptRedeemer,
    scriptWitnessScript,

    -- ** Languages supported in each era
    ScriptLanguageInEra(..),
    scriptLanguageSupportedInEra,
    languageOfScriptLanguageInEra,
    eraOfScriptLanguageInEra,

    -- * The simple script language
    SimpleScript(..),
    TimeLocksSupported(..),
    timeLocksSupported,
    adjustSimpleScriptVersion,

    -- * The Zerepoch script language
    ZerepochScript(..),
    exampleZerepochScriptAlwaysSucceeds,
    exampleZerepochScriptAlwaysFails,

    -- * Script data
    ScriptData(..),

    -- * Script execution units
    ExecutionUnits(..),

    -- * Script hashes
    ScriptHash(..),
    hashScript,

    -- * Internal conversion functions
    toSophieScript,
    fromSophieBasedScript,
    toSophieMultiSig,
    fromSophieMultiSig,
    toEvieTimelock,
    fromEvieTimelock,
    toAurumExUnits,
    fromAurumExUnits,
    toSophieScriptHash,
    fromSophieScriptHash,
    toZerepochData,
    fromZerepochData,
    toAurumData,
    fromAurumData,
    toAurumLanguage,
    fromAurumLanguage,

    -- * Data family instances
    AsType(..),
    Hash(..),
  ) where

import           Prelude

import           Data.Word (Word64)
import qualified Data.ByteString.Lazy as LBS
import           Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as SBS
import           Data.Foldable (toList)
import           Data.Scientific (toBoundedInteger)
import           Data.String (IsString)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Type.Equality (TestEquality (..), (:~:) (Refl))
import           Data.Typeable (Typeable)
import           Numeric.Natural (Natural)

import           Data.Aeson (Value (..), object, (.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Aeson.Encoding as Aeson
import qualified Data.Sequence.Strict as Seq
import           Data.Vector (Vector)
import qualified Data.Vector as Vector

import           Control.Applicative
import           Control.Monad

import qualified Bcc.Binary as CBOR

import qualified Bcc.Crypto.Hash.Class as Crypto

import           Bcc.Slotting.Slot (SlotNo)

import qualified Bcc.Ledger.Core as Ledger
import qualified Bcc.Ledger.Era  as Ledger

import qualified Bcc.Ledger.SophieMA.Timelocks as Timelock
import           Shardagnostic.Consensus.Sophie.Eras (StandardCrypto)
import qualified Bcc.Ledger.Keys as Sophie
import qualified Sophie.Spec.Ledger.Scripts as Sophie

import qualified Bcc.Ledger.Aurum.Language as Aurum
import qualified Bcc.Ledger.Aurum.Scripts as Aurum

import qualified Zerepoch.V1.Ledger.Examples as Zerepoch

import           Bcc.Api.Eras
import           Bcc.Api.HasTypeProxy
import           Bcc.Api.Hash
import           Bcc.Api.KeysSophie
import           Bcc.Api.ScriptData
import           Bcc.Api.SerialiseCBOR
import           Bcc.Api.SerialiseJSON
import           Bcc.Api.SerialiseRaw
import           Bcc.Api.SerialiseTextEnvelope
import           Bcc.Api.SerialiseUsing

{- HLINT ignore "Use section" -}


-- ----------------------------------------------------------------------------
-- Types for script language and version
--

-- | The original simple script language which supports
--
-- * require a signature from a given key (by verification key hash)
-- * n-way and combinator
-- * n-way or combinator
-- * m-of-n combinator
--
-- This version of the language was introduced in the 'SophieEra'.
--
data SimpleScriptV1

-- | The second version of the simple script language. It has all the features
-- of 'SimpleScriptV1' plus new atomic predicates:
--
-- * require the time be before a given slot number
-- * require the time be after a given slot number
--
-- This version of the language was introduced in the 'EvieEra'.
--
data SimpleScriptV2

-- | Place holder type to show what the pattern is to extend to multiple
-- languages, not just multiple versions of a single language.
--
data ZerepochScriptV1

instance HasTypeProxy SimpleScriptV1 where
    data AsType SimpleScriptV1 = AsSimpleScriptV1
    proxyToAsType _ = AsSimpleScriptV1

instance HasTypeProxy SimpleScriptV2 where
    data AsType SimpleScriptV2 = AsSimpleScriptV2
    proxyToAsType _ = AsSimpleScriptV2

instance HasTypeProxy ZerepochScriptV1 where
    data AsType ZerepochScriptV1 = AsZerepochScriptV1
    proxyToAsType _ = AsZerepochScriptV1


-- ----------------------------------------------------------------------------
-- Value level representation for script languages
--
data ScriptLanguage lang where

     SimpleScriptLanguage :: SimpleScriptVersion lang -> ScriptLanguage lang

     ZerepochScriptLanguage :: ZerepochScriptVersion lang -> ScriptLanguage lang

deriving instance (Eq   (ScriptLanguage lang))
deriving instance (Show (ScriptLanguage lang))

instance TestEquality ScriptLanguage where
    testEquality (SimpleScriptLanguage lang)
                 (SimpleScriptLanguage lang') = testEquality lang lang'

    testEquality (ZerepochScriptLanguage lang)
                 (ZerepochScriptLanguage lang') = testEquality lang lang'

    testEquality  _ _ = Nothing


data SimpleScriptVersion lang where

     SimpleScriptV1 :: SimpleScriptVersion SimpleScriptV1
     SimpleScriptV2 :: SimpleScriptVersion SimpleScriptV2

deriving instance (Eq   (SimpleScriptVersion lang))
deriving instance (Show (SimpleScriptVersion lang))

instance TestEquality SimpleScriptVersion where
    testEquality SimpleScriptV1 SimpleScriptV1 = Just Refl
    testEquality SimpleScriptV2 SimpleScriptV2 = Just Refl
    testEquality _              _              = Nothing


data ZerepochScriptVersion lang where
    ZerepochScriptV1 :: ZerepochScriptVersion ZerepochScriptV1

deriving instance (Eq   (ZerepochScriptVersion lang))
deriving instance (Show (ZerepochScriptVersion lang))

instance TestEquality ZerepochScriptVersion where
    testEquality ZerepochScriptV1 ZerepochScriptV1 = Just Refl


data AnyScriptLanguage where
     AnyScriptLanguage :: ScriptLanguage lang -> AnyScriptLanguage

deriving instance (Show AnyScriptLanguage)

instance Eq AnyScriptLanguage where
    a == b = fromEnum a == fromEnum b

instance Ord AnyScriptLanguage where
    compare a b = compare (fromEnum a) (fromEnum b)

instance Enum AnyScriptLanguage where
    toEnum 0 = AnyScriptLanguage (SimpleScriptLanguage SimpleScriptV1)
    toEnum 1 = AnyScriptLanguage (SimpleScriptLanguage SimpleScriptV2)
    toEnum 2 = AnyScriptLanguage (ZerepochScriptLanguage ZerepochScriptV1)
    toEnum _ = error "AnyScriptLanguage.toEnum: bad argument"

    fromEnum (AnyScriptLanguage (SimpleScriptLanguage SimpleScriptV1)) = 0
    fromEnum (AnyScriptLanguage (SimpleScriptLanguage SimpleScriptV2)) = 1
    fromEnum (AnyScriptLanguage (ZerepochScriptLanguage ZerepochScriptV1)) = 2

instance Bounded AnyScriptLanguage where
    minBound = AnyScriptLanguage (SimpleScriptLanguage SimpleScriptV1)
    maxBound = AnyScriptLanguage (ZerepochScriptLanguage ZerepochScriptV1)


data AnyZerepochScriptVersion where
     AnyZerepochScriptVersion :: ZerepochScriptVersion lang
                            -> AnyZerepochScriptVersion

deriving instance (Show AnyZerepochScriptVersion)

instance Eq AnyZerepochScriptVersion where
    a == b = fromEnum a == fromEnum b

instance Ord AnyZerepochScriptVersion where
    compare a b = compare (fromEnum a) (fromEnum b)

instance Enum AnyZerepochScriptVersion where
    toEnum 0 = AnyZerepochScriptVersion ZerepochScriptV1
    toEnum _ = error "AnyZerepochScriptVersion.toEnum: bad argument"

    fromEnum (AnyZerepochScriptVersion ZerepochScriptV1) = 0

instance Bounded AnyZerepochScriptVersion where
    minBound = AnyZerepochScriptVersion ZerepochScriptV1
    maxBound = AnyZerepochScriptVersion ZerepochScriptV1

instance ToCBOR AnyZerepochScriptVersion where
    toCBOR = toCBOR . fromEnum

instance FromCBOR AnyZerepochScriptVersion where
    fromCBOR = do
      n <- fromCBOR
      if n >= fromEnum (minBound :: AnyZerepochScriptVersion) &&
         n <= fromEnum (maxBound :: AnyZerepochScriptVersion)
        then return $! toEnum n
        else fail "zerepoch script version out of bounds"

instance ToJSON AnyZerepochScriptVersion where
    toJSON (AnyZerepochScriptVersion ZerepochScriptV1) =
      Aeson.String "ZerepochScriptV1"

parseZerepochScriptVersion :: Text -> Aeson.Parser AnyZerepochScriptVersion
parseZerepochScriptVersion t =
  case t of
    "ZerepochScriptV1" -> return (AnyZerepochScriptVersion ZerepochScriptV1)
    _                -> fail "Expected ZerepochScriptV1"

instance FromJSON AnyZerepochScriptVersion where
    parseJSON = Aeson.withText "ZerepochScriptVersion" parseZerepochScriptVersion

instance Aeson.FromJSONKey AnyZerepochScriptVersion where
    fromJSONKey = Aeson.FromJSONKeyTextParser parseZerepochScriptVersion

instance Aeson.ToJSONKey AnyZerepochScriptVersion where
    toJSONKey = Aeson.ToJSONKeyText toText toAesonEncoding
      where
        toText :: AnyZerepochScriptVersion -> Text
        toText (AnyZerepochScriptVersion ZerepochScriptV1) = "ZerepochScriptV1"
        toAesonEncoding = Aeson.text . toText

toAurumLanguage :: AnyZerepochScriptVersion -> Aurum.Language
toAurumLanguage (AnyZerepochScriptVersion ZerepochScriptV1) = Aurum.ZerepochV1

fromAurumLanguage :: Aurum.Language -> AnyZerepochScriptVersion
fromAurumLanguage Aurum.ZerepochV1 = AnyZerepochScriptVersion ZerepochScriptV1


class HasTypeProxy lang => IsScriptLanguage lang where
    scriptLanguage :: ScriptLanguage lang

instance IsScriptLanguage SimpleScriptV1 where
    scriptLanguage = SimpleScriptLanguage SimpleScriptV1

instance IsScriptLanguage SimpleScriptV2 where
    scriptLanguage = SimpleScriptLanguage SimpleScriptV2

instance IsScriptLanguage ZerepochScriptV1 where
    scriptLanguage = ZerepochScriptLanguage ZerepochScriptV1


class IsScriptLanguage lang => IsSimpleScriptLanguage lang where
    simpleScriptVersion :: SimpleScriptVersion lang

instance IsSimpleScriptLanguage SimpleScriptV1 where
    simpleScriptVersion = SimpleScriptV1

instance IsSimpleScriptLanguage SimpleScriptV2 where
    simpleScriptVersion = SimpleScriptV2


class IsScriptLanguage lang => IsZerepochScriptLanguage lang where
    zerepochScriptVersion :: ZerepochScriptVersion lang

instance IsZerepochScriptLanguage ZerepochScriptV1 where
    zerepochScriptVersion = ZerepochScriptV1


-- ----------------------------------------------------------------------------
-- Script type: covering all script languages
--

-- | A script in a particular language.
--
-- See also 'ScriptInAnyLang' for a script in any of the known languages.
--
-- See also 'ScriptInEra' for a script in a language that is available within
-- a particular era.
--
-- Note that some but not all scripts have an external JSON syntax, hence this
-- type has no JSON serialisation instances. The 'SimpleScript' family of
-- languages do have a JSON syntax and thus have 'ToJSON'\/'FromJSON' instances.
--
data Script lang where

     SimpleScript :: !(SimpleScriptVersion lang)
                  -> !(SimpleScript lang)
                  -> Script lang

     ZerepochScript :: !(ZerepochScriptVersion lang)
                  -> !(ZerepochScript lang)
                  -> Script lang

deriving instance (Eq   (Script lang))
deriving instance (Show (Script lang))

instance HasTypeProxy lang => HasTypeProxy (Script lang) where
    data AsType (Script lang) = AsScript (AsType lang)
    proxyToAsType _ = AsScript (proxyToAsType (Proxy :: Proxy lang))

instance IsScriptLanguage lang => SerialiseAsCBOR (Script lang) where
    serialiseToCBOR (SimpleScript SimpleScriptV1 s) =
      CBOR.serialize' (toSophieMultiSig s)

    serialiseToCBOR (SimpleScript SimpleScriptV2 s) =
      CBOR.serialize' (toEvieTimelock s :: Timelock.Timelock StandardCrypto)

    serialiseToCBOR (ZerepochScript ZerepochScriptV1 s) =
      CBOR.serialize' s

    deserialiseFromCBOR _ bs =
      case scriptLanguage :: ScriptLanguage lang of
        SimpleScriptLanguage SimpleScriptV1 ->
              SimpleScript SimpleScriptV1
            . fromSophieMultiSig
          <$> CBOR.decodeAnnotator "Script" fromCBOR (LBS.fromStrict bs)

        SimpleScriptLanguage SimpleScriptV2 ->
              SimpleScript SimpleScriptV2
            . (fromEvieTimelock TimeLocksInSimpleScriptV2
                                :: Timelock.Timelock StandardCrypto
                                -> SimpleScript SimpleScriptV2)
          <$> CBOR.decodeAnnotator "Script" fromCBOR (LBS.fromStrict bs)

        ZerepochScriptLanguage ZerepochScriptV1 ->
              ZerepochScript ZerepochScriptV1
          <$> CBOR.decodeFull' bs


instance IsScriptLanguage lang => HasTextEnvelope (Script lang) where
    textEnvelopeType _ =
      case scriptLanguage :: ScriptLanguage lang of
        SimpleScriptLanguage SimpleScriptV1 -> "SimpleScriptV1"
        SimpleScriptLanguage SimpleScriptV2 -> "SimpleScriptV2"
        ZerepochScriptLanguage ZerepochScriptV1 -> "ZerepochScriptV1"


-- ----------------------------------------------------------------------------
-- Scripts in any language
--

-- | Sometimes it is necessary to handle all languages without making static
-- type distinctions between languages. For example, when reading external
-- input, or before the era context is known.
--
-- Use 'toScriptInEra' to convert to a script in the context of an era.
--
data ScriptInAnyLang where
     ScriptInAnyLang :: ScriptLanguage lang
                     -> Script lang
                     -> ScriptInAnyLang

deriving instance Show ScriptInAnyLang

-- The GADT in the ScriptInAnyLang constructor requires a custom Eq instance
instance Eq ScriptInAnyLang where
    (==) (ScriptInAnyLang lang  script)
         (ScriptInAnyLang lang' script') =
      case testEquality lang lang' of
        Nothing   -> False
        Just Refl -> script == script'


-- | Convert a script in a specific statically-known language to a
-- 'ScriptInAnyLang'.
--
-- No inverse to this is provided, just do case analysis on the 'ScriptLanguage'
-- field within the 'ScriptInAnyLang' constructor.
--
toScriptInAnyLang :: Script lang -> ScriptInAnyLang
toScriptInAnyLang s@(SimpleScript v _) =
    ScriptInAnyLang (SimpleScriptLanguage v) s
toScriptInAnyLang s@(ZerepochScript v _) =
    ScriptInAnyLang (ZerepochScriptLanguage v) s

instance HasTypeProxy ScriptInAnyLang where
    data AsType ScriptInAnyLang = AsScriptInAnyLang
    proxyToAsType _ = AsScriptInAnyLang


-- ----------------------------------------------------------------------------
-- Scripts in the context of a ledger era
--

data ScriptInEra era where
     ScriptInEra :: ScriptLanguageInEra lang era
                 -> Script lang
                 -> ScriptInEra era

deriving instance Show (ScriptInEra era)

-- The GADT in the ScriptInEra constructor requires a custom instance
instance Eq (ScriptInEra era) where
    (==) (ScriptInEra langInEra  script)
         (ScriptInEra langInEra' script') =
      case testEquality (languageOfScriptLanguageInEra langInEra)
                        (languageOfScriptLanguageInEra langInEra') of
        Nothing   -> False
        Just Refl -> script == script'


data ScriptLanguageInEra lang era where

     SimpleScriptV1InSophie :: ScriptLanguageInEra SimpleScriptV1 SophieEra
     SimpleScriptV1InEvie :: ScriptLanguageInEra SimpleScriptV1 EvieEra
     SimpleScriptV1InJen    :: ScriptLanguageInEra SimpleScriptV1 JenEra
     SimpleScriptV1InAurum  :: ScriptLanguageInEra SimpleScriptV1 AurumEra

     SimpleScriptV2InEvie :: ScriptLanguageInEra SimpleScriptV2 EvieEra
     SimpleScriptV2InJen    :: ScriptLanguageInEra SimpleScriptV2 JenEra
     SimpleScriptV2InAurum  :: ScriptLanguageInEra SimpleScriptV2 AurumEra

     ZerepochScriptV1InAurum  :: ScriptLanguageInEra ZerepochScriptV1 AurumEra

deriving instance Eq   (ScriptLanguageInEra lang era)
deriving instance Show (ScriptLanguageInEra lang era)

instance HasTypeProxy era => HasTypeProxy (ScriptInEra era) where
    data AsType (ScriptInEra era) = AsScriptInEra (AsType era)
    proxyToAsType _ = AsScriptInEra (proxyToAsType (Proxy :: Proxy era))


-- | Check if a given script language is supported in a given era, and if so
-- return the evidence.
--
scriptLanguageSupportedInEra :: BccEra era
                             -> ScriptLanguage lang
                             -> Maybe (ScriptLanguageInEra lang era)
scriptLanguageSupportedInEra era lang =
    case (era, lang) of
      (SophieEra, SimpleScriptLanguage SimpleScriptV1) ->
        Just SimpleScriptV1InSophie

      (EvieEra, SimpleScriptLanguage SimpleScriptV1) ->
        Just SimpleScriptV1InEvie

      (JenEra, SimpleScriptLanguage SimpleScriptV1) ->
        Just SimpleScriptV1InJen

      (EvieEra, SimpleScriptLanguage SimpleScriptV2) ->
        Just SimpleScriptV2InEvie

      (JenEra, SimpleScriptLanguage SimpleScriptV2) ->
        Just SimpleScriptV2InJen

      (AurumEra, SimpleScriptLanguage SimpleScriptV1) ->
        Just SimpleScriptV1InAurum

      (AurumEra, SimpleScriptLanguage SimpleScriptV2) ->
        Just SimpleScriptV2InAurum

      (AurumEra, ZerepochScriptLanguage ZerepochScriptV1) ->
        Just ZerepochScriptV1InAurum

      _ -> Nothing

languageOfScriptLanguageInEra :: ScriptLanguageInEra lang era
                              -> ScriptLanguage lang
languageOfScriptLanguageInEra langInEra =
    case langInEra of
      SimpleScriptV1InSophie -> SimpleScriptLanguage SimpleScriptV1
      SimpleScriptV1InEvie -> SimpleScriptLanguage SimpleScriptV1
      SimpleScriptV1InJen    -> SimpleScriptLanguage SimpleScriptV1
      SimpleScriptV1InAurum  -> SimpleScriptLanguage SimpleScriptV1

      SimpleScriptV2InEvie -> SimpleScriptLanguage SimpleScriptV2
      SimpleScriptV2InJen    -> SimpleScriptLanguage SimpleScriptV2
      SimpleScriptV2InAurum  -> SimpleScriptLanguage SimpleScriptV2

      ZerepochScriptV1InAurum  -> ZerepochScriptLanguage ZerepochScriptV1

eraOfScriptLanguageInEra :: ScriptLanguageInEra lang era
                         -> SophieBasedEra era
eraOfScriptLanguageInEra langInEra =
    case langInEra of
      SimpleScriptV1InSophie -> SophieBasedEraSophie

      SimpleScriptV1InEvie -> SophieBasedEraEvie
      SimpleScriptV2InEvie -> SophieBasedEraEvie

      SimpleScriptV1InJen    -> SophieBasedEraJen
      SimpleScriptV2InJen    -> SophieBasedEraJen

      SimpleScriptV1InAurum  -> SophieBasedEraAurum
      SimpleScriptV2InAurum  -> SophieBasedEraAurum

      ZerepochScriptV1InAurum  -> SophieBasedEraAurum


-- | Given a target era and a script in some language, check if the language is
-- supported in that era, and if so return a 'ScriptInEra'.
--
toScriptInEra :: BccEra era -> ScriptInAnyLang -> Maybe (ScriptInEra era)
toScriptInEra era (ScriptInAnyLang lang s) = do
    lang' <- scriptLanguageSupportedInEra era lang
    return (ScriptInEra lang' s)

eraOfScriptInEra :: ScriptInEra era -> SophieBasedEra era
eraOfScriptInEra (ScriptInEra langInEra _) = eraOfScriptLanguageInEra langInEra


-- ----------------------------------------------------------------------------
-- Scripts used in a transaction (in an era) to witness authorised use
--

-- | A tag type for the context in which a script is used in a transaction.
--
-- This type tags the context as being to witness a transaction input.
--
data WitCtxTxIn

-- | A tag type for the context in which a script is used in a transaction.
--
-- This type tags the context as being to witness minting.
--
data WitCtxMint

-- | A tag type for the context in which a script is used in a transaction.
--
-- This type tags the context as being to witness the use of stake addresses in
-- both certificates and withdrawals.
--
data WitCtxStake


-- | This GADT provides a value-level representation of all the witness
-- contexts. This enables pattern matching on the context to allow them to be
-- treated in a non-uniform way.
--
data WitCtx witctx where
     WitCtxTxIn  :: WitCtx WitCtxTxIn
     WitCtxMint  :: WitCtx WitCtxMint
     WitCtxStake :: WitCtx WitCtxStake


-- | A /use/ of a script within a transaction body to witness that something is
-- being used in an authorised manner. That can be
--
-- * spending a transaction input
-- * minting tokens
-- * using a certificate (stake address certs specifically)
-- * withdrawing from a reward account
--
-- For simple script languages, the use of the script is the same in all
-- contexts. For Zerepoch scripts, using a script involves supplying a redeemer.
-- In addition, Zerepoch scripts used for spending inputs must also supply the
-- datum value used when originally creating the TxOut that is now being spent.
--
data ScriptWitness witctx era where

     SimpleScriptWitness :: ScriptLanguageInEra lang era
                         -> SimpleScriptVersion lang
                         -> SimpleScript        lang
                         -> ScriptWitness witctx era

     ZerepochScriptWitness :: ScriptLanguageInEra  lang era
                         -> ZerepochScriptVersion  lang
                         -> ZerepochScript         lang
                         -> ScriptDatum witctx
                         -> ScriptRedeemer
                         -> ExecutionUnits
                         -> ScriptWitness witctx era

deriving instance Show (ScriptWitness witctx era)

-- The GADT in the SimpleScriptWitness constructor requires a custom instance
instance Eq (ScriptWitness witctx era) where
    (==) (SimpleScriptWitness langInEra  version  script)
         (SimpleScriptWitness langInEra' version' script') =
      case testEquality (languageOfScriptLanguageInEra langInEra)
                        (languageOfScriptLanguageInEra langInEra') of
        Nothing   -> False
        Just Refl -> version == version' && script == script'

    (==) (ZerepochScriptWitness langInEra  version   script
                              datum      redeemer  execUnits)
         (ZerepochScriptWitness langInEra' version'  script'
                              datum'     redeemer' execUnits') =
      case testEquality (languageOfScriptLanguageInEra langInEra)
                        (languageOfScriptLanguageInEra langInEra') of
        Nothing   -> False
        Just Refl ->    version   == version'
                     && script    == script'
                     && datum     == datum'
                     && redeemer  == redeemer'
                     && execUnits == execUnits'

    (==)  _ _ = False

type ScriptRedeemer = ScriptData

data ScriptDatum witctx where
     ScriptDatumForTxIn    :: ScriptData -> ScriptDatum WitCtxTxIn
     NoScriptDatumForMint  ::               ScriptDatum WitCtxMint
     NoScriptDatumForStake ::               ScriptDatum WitCtxStake

deriving instance Eq   (ScriptDatum witctx)
deriving instance Show (ScriptDatum witctx)


scriptWitnessScript :: ScriptWitness witctx era -> ScriptInEra era
scriptWitnessScript (SimpleScriptWitness langInEra version script) =
    ScriptInEra langInEra (SimpleScript version script)

scriptWitnessScript (ZerepochScriptWitness langInEra version script _ _ _) =
    ScriptInEra langInEra (ZerepochScript version script)


-- ----------------------------------------------------------------------------
-- The kind of witness to use, key (signature) or script
--

data Witness witctx era where

     KeyWitness    :: KeyWitnessInCtx witctx
                   -> Witness         witctx era

     ScriptWitness :: ScriptWitnessInCtx witctx
                   -> ScriptWitness      witctx era
                   -> Witness            witctx era

deriving instance Eq   (Witness witctx era)
deriving instance Show (Witness witctx era)

data KeyWitnessInCtx witctx where

     KeyWitnessForSpending  :: KeyWitnessInCtx WitCtxTxIn
     KeyWitnessForStakeAddr :: KeyWitnessInCtx WitCtxStake

data ScriptWitnessInCtx witctx where

     ScriptWitnessForSpending  :: ScriptWitnessInCtx WitCtxTxIn
     ScriptWitnessForMinting   :: ScriptWitnessInCtx WitCtxMint
     ScriptWitnessForStakeAddr :: ScriptWitnessInCtx WitCtxStake

deriving instance Eq   (KeyWitnessInCtx witctx)
deriving instance Show (KeyWitnessInCtx witctx)

deriving instance Eq   (ScriptWitnessInCtx witctx)
deriving instance Show (ScriptWitnessInCtx witctx)


-- ----------------------------------------------------------------------------
-- Script execution units
--

-- | The units for how long a script executes for and how much memory it uses.
-- This is used to declare the resources used by a particular use of a script.
--
-- This type is also used to describe the limits for the maximum overall
-- execution units per transaction or per block.
--
data ExecutionUnits =
     ExecutionUnits {
        -- | This corresponds roughly to the time to execute a script.
        executionSteps  :: Word64,

        -- | This corresponds roughly to the peak memory used during script
        -- execution.
        executionMemory :: Word64
     }
  deriving (Eq, Show)

instance ToCBOR ExecutionUnits where
  toCBOR ExecutionUnits{executionSteps, executionMemory} =
      CBOR.encodeListLen 2
   <> toCBOR executionSteps
   <> toCBOR executionMemory

instance FromCBOR ExecutionUnits where
  fromCBOR = do
    CBOR.enforceSize "ExecutionUnits" 2
    ExecutionUnits
      <$> fromCBOR
      <*> fromCBOR

instance ToJSON ExecutionUnits where
  toJSON ExecutionUnits{executionSteps, executionMemory} =
    object [ "steps"  .= executionSteps
           , "memory" .= executionMemory ]

instance FromJSON ExecutionUnits where
  parseJSON =
    Aeson.withObject "ExecutionUnits" $ \o ->
      ExecutionUnits
        <$> o .: "steps"
        <*> o .: "memory"

toAurumExUnits :: ExecutionUnits -> Aurum.ExUnits
toAurumExUnits ExecutionUnits{executionSteps, executionMemory} =
  Aurum.ExUnits {
    Aurum.exUnitsSteps = executionSteps,
    Aurum.exUnitsMem   = executionMemory
  }

fromAurumExUnits :: Aurum.ExUnits -> ExecutionUnits
fromAurumExUnits Aurum.ExUnits{Aurum.exUnitsSteps, Aurum.exUnitsMem} =
  ExecutionUnits {
    executionSteps  = exUnitsSteps,
    executionMemory = exUnitsMem
  }


-- ----------------------------------------------------------------------------
-- Script Hash
--

-- | We have this type separate from the 'Hash' type to avoid the script
-- hash type being parametrised by the era. The representation is era
-- independent, and there are many places where we want to use a script
-- hash where we don't want things to be era-parametrised.
--
newtype ScriptHash = ScriptHash (Sophie.ScriptHash StandardCrypto)
  deriving stock (Eq, Ord)
  deriving (Show, IsString)   via UsingRawBytesHex ScriptHash
  deriving (ToJSON, FromJSON) via UsingRawBytesHex ScriptHash

instance HasTypeProxy ScriptHash where
    data AsType ScriptHash = AsScriptHash
    proxyToAsType _ = AsScriptHash

instance SerialiseAsRawBytes ScriptHash where
    serialiseToRawBytes (ScriptHash (Sophie.ScriptHash h)) =
      Crypto.hashToBytes h

    deserialiseFromRawBytes AsScriptHash bs =
      ScriptHash . Sophie.ScriptHash <$> Crypto.hashFromBytes bs


hashScript :: Script lang -> ScriptHash
hashScript (SimpleScript SimpleScriptV1 s) =
    -- For V1, we convert to the Sophie-era version specifically and hash that.
    -- Later ledger eras have to be compatible anyway.
    ScriptHash
  . Ledger.hashScript @(SophieLedgerEra SophieEra)
  . toSophieMultiSig
  $ s

hashScript (SimpleScript SimpleScriptV2 s) =
    -- For V2, we convert to the Evie-era version specifically and hash that.
    -- Later ledger eras have to be compatible anyway.
    ScriptHash
  . Ledger.hashScript @(SophieLedgerEra EvieEra)
  . (toEvieTimelock :: SimpleScript SimpleScriptV2
                       -> Timelock.Timelock StandardCrypto)
  $ s

hashScript (ZerepochScript ZerepochScriptV1 (ZerepochScriptSerialised script)) =
    -- For Zerepoch V1, we convert to the Aurum-era version specifically and
    -- hash that. Later ledger eras have to be compatible anyway.
    ScriptHash
  . Ledger.hashScript @(SophieLedgerEra AurumEra)
  $ Aurum.ZerepochScript script

toSophieScriptHash :: ScriptHash -> Sophie.ScriptHash StandardCrypto
toSophieScriptHash (ScriptHash h) =  h

fromSophieScriptHash :: Sophie.ScriptHash StandardCrypto -> ScriptHash
fromSophieScriptHash = ScriptHash


-- ----------------------------------------------------------------------------
-- The simple native script language
--

data SimpleScript lang where

     RequireSignature  :: !(Hash PaymentKey)
                       -> SimpleScript lang

     RequireTimeBefore :: !(TimeLocksSupported lang)
                       -> !SlotNo
                       -> SimpleScript lang

     RequireTimeAfter  :: !(TimeLocksSupported lang)
                       -> !SlotNo
                       -> SimpleScript lang

     RequireAllOf      ::        [SimpleScript lang] -> SimpleScript lang
     RequireAnyOf      ::        [SimpleScript lang] -> SimpleScript lang
     RequireMOf        :: Int -> [SimpleScript lang] -> SimpleScript lang

deriving instance Eq   (SimpleScript lang)
deriving instance Show (SimpleScript lang)

instance HasTypeProxy lang => HasTypeProxy (SimpleScript lang) where
    data AsType (SimpleScript lang) = AsSimpleScript (AsType lang)
    proxyToAsType _ = AsSimpleScript (proxyToAsType (Proxy :: Proxy lang))


-- | Time lock feature in the 'SimpleScript' language.
--
-- The constructors of this type serve as evidence that the timelocks feature
-- is supported in particular versions of the language.
--
data TimeLocksSupported lang where
     TimeLocksInSimpleScriptV2 :: TimeLocksSupported SimpleScriptV2

deriving instance Eq   (TimeLocksSupported lang)
deriving instance Show (TimeLocksSupported lang)

timeLocksSupported :: SimpleScriptVersion lang
                   -> Maybe (TimeLocksSupported lang)
timeLocksSupported SimpleScriptV1 = Nothing
timeLocksSupported SimpleScriptV2 = Just TimeLocksInSimpleScriptV2


-- | Try converting the 'SimpleScript' into a different version of the language.
--
-- This will work when the script only uses the features of the target language
-- version. For example converting from 'SimpleScriptV2' to 'SimpleScriptV1'
-- will work if the script happens not to use time locks feature. On the other
-- hand converting 'SimpleScriptV1' to 'SimpleScriptV2' will always work because
-- it is backwards compatible.
--
adjustSimpleScriptVersion :: SimpleScriptVersion lang'
                          -> SimpleScript lang
                          -> Maybe (SimpleScript lang')
adjustSimpleScriptVersion target = go
  where
    go (RequireSignature sig) = pure (RequireSignature sig)

    go (RequireTimeBefore _ slot) = do
      supported <- timeLocksSupported target
      pure (RequireTimeBefore supported slot)

    go (RequireTimeAfter _ slot) = do
      supported <- timeLocksSupported target
      pure (RequireTimeAfter supported slot)

    go (RequireAllOf ss) = RequireAllOf <$> traverse go ss
    go (RequireAnyOf ss) = RequireAnyOf <$> traverse go ss
    go (RequireMOf m ss) = RequireMOf m <$> traverse go ss


-- ----------------------------------------------------------------------------
-- The Zerepoch script language
--

-- | Zerepoch scripts.
--
-- Note that Zerepoch scripts have a binary serialisation but no JSON
-- serialisation.
--
data ZerepochScript lang where
     ZerepochScriptSerialised :: ShortByteString -> ZerepochScript lang
  deriving stock (Eq, Ord)
  deriving stock (Show) -- TODO: would be nice to use via UsingRawBytesHex
                        -- however that adds an awkward HasTypeProxy lang =>
                        -- constraint to other Show instances elsewhere
  deriving (ToCBOR, FromCBOR) via (UsingRawBytes (ZerepochScript lang))
  deriving anyclass SerialiseAsCBOR

instance HasTypeProxy lang => HasTypeProxy (ZerepochScript lang) where
    data AsType (ZerepochScript lang) = AsZerepochScript (AsType lang)
    proxyToAsType _ = AsZerepochScript (proxyToAsType (Proxy :: Proxy lang))

instance HasTypeProxy lang => SerialiseAsRawBytes (ZerepochScript lang) where
    serialiseToRawBytes (ZerepochScriptSerialised sbs) = SBS.fromShort sbs

    deserialiseFromRawBytes (AsZerepochScript _) bs =
      -- TODO aurum: validate the script syntax and fail decoding if invalid
      Just (ZerepochScriptSerialised (SBS.toShort bs))

instance (IsZerepochScriptLanguage lang, Typeable lang) =>
         HasTextEnvelope (ZerepochScript lang) where
    textEnvelopeType _ =
      case zerepochScriptVersion :: ZerepochScriptVersion lang of
        ZerepochScriptV1 -> "ZerepochScriptV1"


-- | An example Zerepoch script that always succeeds, irrespective of inputs.
--
-- For example, if one were to use this for a payment address then it would
-- allow anyone to spend from it.
--
-- The exact script depends on the context in which it is to be used.
--
exampleZerepochScriptAlwaysSucceeds :: WitCtx witctx
                                  -> ZerepochScript ZerepochScriptV1
exampleZerepochScriptAlwaysSucceeds =
    ZerepochScriptSerialised
  . Zerepoch.alwaysSucceedingNAryFunction
  . scriptArityForWitCtx

-- | An example Zerepoch script that always fails, irrespective of inputs.
--
-- For example, if one were to use this for a payment address then it would
-- be impossible for anyone to ever spend from it.
--
-- The exact script depends on the context in which it is to be used.
--
exampleZerepochScriptAlwaysFails :: WitCtx witctx
                               -> ZerepochScript ZerepochScriptV1
exampleZerepochScriptAlwaysFails =
    ZerepochScriptSerialised
  . Zerepoch.alwaysFailingNAryFunction
  . scriptArityForWitCtx

-- | The expected arity of the Zerepoch function, depending on the context in
-- which it is used.
--
-- The script inputs consist of
--
-- * the optional datum (for txins)
-- * the redeemer
-- * the Zerepoch representation of the tx and environment
--
scriptArityForWitCtx :: WitCtx witctx -> Natural
scriptArityForWitCtx WitCtxTxIn  = 3
scriptArityForWitCtx WitCtxMint  = 2
scriptArityForWitCtx WitCtxStake = 2


-- ----------------------------------------------------------------------------
-- Conversion functions
--

toSophieScript :: ScriptInEra era -> Ledger.Script (SophieLedgerEra era)
toSophieScript (ScriptInEra langInEra (SimpleScript SimpleScriptV1 script)) =
    case langInEra of
      SimpleScriptV1InSophie -> toSophieMultiSig script
      SimpleScriptV1InEvie -> toEvieTimelock script
      SimpleScriptV1InJen    -> toEvieTimelock script
      SimpleScriptV1InAurum  -> Aurum.TimelockScript (toEvieTimelock script)

toSophieScript (ScriptInEra langInEra (SimpleScript SimpleScriptV2 script)) =
    case langInEra of
      SimpleScriptV2InEvie -> toEvieTimelock script
      SimpleScriptV2InJen    -> toEvieTimelock script
      SimpleScriptV2InAurum  -> Aurum.TimelockScript (toEvieTimelock script)

toSophieScript (ScriptInEra langInEra (ZerepochScript ZerepochScriptV1
                                         (ZerepochScriptSerialised script))) =
    case langInEra of
      ZerepochScriptV1InAurum  -> Aurum.ZerepochScript script

fromSophieBasedScript  :: SophieBasedEra era
                        -> Ledger.Script (SophieLedgerEra era)
                        -> ScriptInEra era
fromSophieBasedScript era script =
  case era of
    SophieBasedEraSophie ->
      ScriptInEra SimpleScriptV1InSophie $
      SimpleScript SimpleScriptV1 $
      fromSophieMultiSig script
    SophieBasedEraEvie ->
      ScriptInEra SimpleScriptV2InEvie $
      SimpleScript SimpleScriptV2 $
      fromEvieTimelock TimeLocksInSimpleScriptV2 script
    SophieBasedEraJen ->
      ScriptInEra SimpleScriptV2InJen $
      SimpleScript SimpleScriptV2 $
      fromEvieTimelock TimeLocksInSimpleScriptV2 script
    SophieBasedEraAurum ->
      case script of
        Aurum.TimelockScript s ->
          ScriptInEra SimpleScriptV2InAurum $
          SimpleScript SimpleScriptV2 $
          fromEvieTimelock TimeLocksInSimpleScriptV2 s
        Aurum.ZerepochScript s ->
          ScriptInEra ZerepochScriptV1InAurum $
          ZerepochScript ZerepochScriptV1 $
          ZerepochScriptSerialised s


-- | Conversion for the 'Sophie.MultiSig' language used by the Sophie era.
--
toSophieMultiSig :: SimpleScript SimpleScriptV1
                  -> Sophie.MultiSig StandardCrypto
toSophieMultiSig = go
  where
    go :: SimpleScript SimpleScriptV1 -> Sophie.MultiSig StandardCrypto
    go (RequireSignature (PaymentKeyHash kh))
                        = Sophie.RequireSignature (Sophie.coerceKeyRole kh)
    go (RequireAllOf s) = Sophie.RequireAllOf (map go s)
    go (RequireAnyOf s) = Sophie.RequireAnyOf (map go s)
    go (RequireMOf m s) = Sophie.RequireMOf m (map go s)

-- | Conversion for the 'Sophie.MultiSig' language used by the Sophie era.
--
fromSophieMultiSig :: Sophie.MultiSig StandardCrypto -> SimpleScript lang
fromSophieMultiSig = go
  where
    go (Sophie.RequireSignature kh)
                                = RequireSignature
                                    (PaymentKeyHash (Sophie.coerceKeyRole kh))
    go (Sophie.RequireAllOf s) = RequireAllOf (map go s)
    go (Sophie.RequireAnyOf s) = RequireAnyOf (map go s)
    go (Sophie.RequireMOf m s) = RequireMOf m (map go s)

-- | Conversion for the 'Timelock.Timelock' language that is shared between the
-- Evie and Jen eras.
--
toEvieTimelock :: forall lang.
                     SimpleScript lang -> Timelock.Timelock StandardCrypto
toEvieTimelock = go
  where
    go :: SimpleScript lang -> Timelock.Timelock StandardCrypto
    go (RequireSignature (PaymentKeyHash kh))
                        = Timelock.RequireSignature (Sophie.coerceKeyRole kh)
    go (RequireAllOf s) = Timelock.RequireAllOf (Seq.fromList (map go s))
    go (RequireAnyOf s) = Timelock.RequireAnyOf (Seq.fromList (map go s))
    go (RequireMOf m s) = Timelock.RequireMOf m (Seq.fromList (map go s))
    go (RequireTimeBefore _ t) = Timelock.RequireTimeExpire t
    go (RequireTimeAfter  _ t) = Timelock.RequireTimeStart  t

-- | Conversion for the 'Timelock.Timelock' language that is shared between the
-- Evie and Jen eras.
--
fromEvieTimelock :: TimeLocksSupported lang
                    -> Timelock.Timelock StandardCrypto
                    -> SimpleScript lang
fromEvieTimelock timelocks = go
  where
    go (Timelock.RequireSignature kh) = RequireSignature
                                          (PaymentKeyHash (Sophie.coerceKeyRole kh))
    go (Timelock.RequireTimeExpire t) = RequireTimeBefore timelocks t
    go (Timelock.RequireTimeStart  t) = RequireTimeAfter  timelocks t
    go (Timelock.RequireAllOf      s) = RequireAllOf (map go (toList s))
    go (Timelock.RequireAnyOf      s) = RequireAnyOf (map go (toList s))
    go (Timelock.RequireMOf      i s) = RequireMOf i (map go (toList s))


-- ----------------------------------------------------------------------------
-- JSON serialisation
--

-- Remember that Zerepoch scripts do not have a JSON syntax, and so do not have
-- and JSON instances. The only JSON format they support is via the
-- HasTextEnvelope class which just wraps the binary format.
--
-- Because of this the 'Script' type also does not have any JSON instances, but
-- the 'SimpleScript' type does.

instance ToJSON (SimpleScript lang) where
  toJSON (RequireSignature pKeyHash) =
    object [ "type"    .= String "sig"
           , "keyHash" .= serialiseToRawBytesHexText pKeyHash
           ]
  toJSON (RequireTimeBefore _ slot) =
    object [ "type" .= String "before"
           , "slot" .= slot
           ]
  toJSON (RequireTimeAfter _ slot) =
    object [ "type" .= String "after"
           , "slot" .= slot
           ]
  toJSON (RequireAnyOf reqScripts) =
    object [ "type" .= String "any", "scripts" .= map toJSON reqScripts ]
  toJSON (RequireAllOf reqScripts) =
    object [ "type" .= String "all", "scripts" .= map toJSON reqScripts ]
  toJSON (RequireMOf reqNum reqScripts) =
    object [ "type" .= String "atLeast"
           , "required" .= reqNum
           , "scripts" .= map toJSON reqScripts
           ]


instance IsSimpleScriptLanguage lang => FromJSON (SimpleScript lang) where
  parseJSON = parseSimpleScript simpleScriptVersion


parseSimpleScript :: SimpleScriptVersion lang
                  -> Value -> Aeson.Parser (SimpleScript lang)
parseSimpleScript lang v = parseScriptSig          v
                       <|> parseScriptBefore  lang v
                       <|> parseScriptAfter   lang v
                       <|> parseScriptAny     lang v
                       <|> parseScriptAll     lang v
                       <|> parseScriptAtLeast lang v

parseScriptAny :: SimpleScriptVersion lang
               -> Value -> Aeson.Parser (SimpleScript lang)
parseScriptAny lang =
    Aeson.withObject "any" $ \obj -> do
      t <- obj .: "type"
      case t :: Text of
        "any" -> do vs <- obj .: "scripts"
                    RequireAnyOf <$> gatherSimpleScriptTerms lang vs
        _ -> fail "\"any\" script value not found"

parseScriptAll :: SimpleScriptVersion lang
               -> Value -> Aeson.Parser (SimpleScript lang)
parseScriptAll lang =
    Aeson.withObject "all" $ \obj -> do
      t <- obj .: "type"
      case t :: Text of
        "all" -> do vs <- obj .: "scripts"
                    RequireAllOf <$> gatherSimpleScriptTerms lang vs
        _ -> fail "\"all\" script value not found"

parseScriptAtLeast :: SimpleScriptVersion lang
                   -> Value -> Aeson.Parser (SimpleScript lang)
parseScriptAtLeast lang =
    Aeson.withObject "atLeast" $ \obj -> do
      v <- obj .: "type"
      case v :: Text of
        "atLeast" -> do
          r  <- obj .: "required"
          vs <- obj .: "scripts"
          case r of
            Number sci ->
              case toBoundedInteger sci of
                Just reqInt ->
                  do scripts <- gatherSimpleScriptTerms lang vs
                     let numScripts = length scripts
                     when
                       (reqInt > numScripts)
                       (fail $ "Required number of script signatures exceeds the number of scripts."
                             <> " Required number: " <> show reqInt
                             <> " Number of scripts: " <> show numScripts)
                     return $ RequireMOf reqInt scripts
                Nothing -> fail $ "Error in \"required\" key: "
                                <> show sci <> " is not a valid Int"
            _ -> fail "\"required\" value should be an integer"
        _        -> fail "\"atLeast\" script value not found"

gatherSimpleScriptTerms :: SimpleScriptVersion lang
                        -> Vector Value -> Aeson.Parser [SimpleScript lang]
gatherSimpleScriptTerms lang = mapM (parseSimpleScript lang) . Vector.toList

parseScriptSig :: Value -> Aeson.Parser (SimpleScript lang)
parseScriptSig =
    Aeson.withObject "sig" $ \obj -> do
      v <- obj .: "type"
      case v :: Text of
        "sig" -> do k <- obj .: "keyHash"
                    RequireSignature <$> parsePaymentKeyHash k
        _     -> fail "\"sig\" script value not found"

parseScriptBefore :: SimpleScriptVersion lang
                  -> Value -> Aeson.Parser (SimpleScript lang)
parseScriptBefore lang =
    Aeson.withObject "before" $ \obj -> do
      v <- obj .: "type"
      case v :: Text of
        "before" ->
          case timeLocksSupported lang of
            Just supported -> RequireTimeBefore supported <$> obj .: "slot"
            Nothing -> fail ("type \"before\" not supported in " ++ show lang)
        _ -> fail "\"before\" script value not found"

parseScriptAfter :: SimpleScriptVersion lang
                 -> Value -> Aeson.Parser (SimpleScript lang)
parseScriptAfter lang =
    Aeson.withObject "after" $ \obj -> do
      v <- obj .: "type"
      case v :: Text of
        "after" ->
          case timeLocksSupported lang of
            Just supported -> RequireTimeAfter supported <$> obj .: "slot"
            Nothing -> fail ("type \"after\" not supported in " ++ show lang)
        _       -> fail "\"after\" script value not found"

parsePaymentKeyHash :: Text -> Aeson.Parser (Hash PaymentKey)
parsePaymentKeyHash txt =
    case deserialiseFromRawBytesHex (AsHash AsPaymentKey) (Text.encodeUtf8 txt) of
      Just payKeyHash -> return payKeyHash
      Nothing -> fail $ "Error deserialising payment key hash: " <> Text.unpack txt
