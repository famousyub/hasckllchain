{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- The Sophie ledger uses promoted data kinds which we have to use, but we do
-- not export any from this API. We also use them unticked as nature intended.
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

-- | Complete, signed transactions
--
module Bcc.Api.Tx (

    -- * Signing transactions
    -- | Creating transaction witnesses one by one, or all in one go.
    Tx(.., Tx),
    getTxBody,
    getTxWitnesses,
    ScriptValidity(..),

    -- ** Signing in one go
    SophieSigningKey(..),
    toSophieSigningKey,
    signColeTransaction,
    signSophieTransaction,
    -- ** Incremental signing and separate witnesses
    makeSignedTransaction,
    KeyWitness(..),
    makeColeKeyWitness,
    SophieWitnessSigningKey(..),
    makeSophieKeyWitness,
    WitnessNetworkIdOrColeAddress (..),
    makeSophieBootstrapWitness,
    makeSophieSignature,
    getSophieKeyWitnessVerificationKey,

    -- * Data family instances
    AsType(AsTx, AsColeTx, AsSophieTx,
           AsKeyWitness, AsColeWitness, AsSophieWitness),
  ) where

import           Prelude

import           Data.Maybe

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import           Data.Functor.Identity (Identity)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vector
--
-- Common types, consensus, network
--
import           Bcc.Binary (Annotated (..))
import qualified Bcc.Binary as CBOR
import qualified Bcc.Prelude as CBOR (cborError)

--
-- Crypto API used by consensus and Sophie (and should be used by Cole)
--
import qualified Bcc.Crypto.DSIGN.Class as Crypto
import qualified Bcc.Crypto.Util as Crypto
import qualified Bcc.Crypto.Wallet as Crypto.HD

--
-- Cole imports
--
import qualified Bcc.Chain.Common as Cole
import qualified Bcc.Chain.UTxO as Cole
import qualified Bcc.Crypto.Hashing as Cole
import qualified Bcc.Crypto.ProtocolMagic as Cole
import qualified Bcc.Crypto.Signing as Cole

--
-- Sophie imports
--
import           Bcc.Ledger.BaseTypes (maybeToStrictMaybe, strictMaybeToMaybe)
import           Bcc.Ledger.Crypto (StandardCrypto)

import qualified Bcc.Ledger.Core as Ledger
import qualified Bcc.Ledger.Era as Ledger
import qualified Bcc.Ledger.SafeHash as Ledger
import qualified Bcc.Ledger.Sophie.Constraints as Sophie
import qualified Sophie.Spec.Ledger.TxBody as Ledger (EraIndependentTxBody)
import qualified Sophie.Spec.Ledger.Address.Bootstrap as Sophie
import qualified Bcc.Ledger.Keys as Sophie
import qualified Sophie.Spec.Ledger.Tx as Sophie

import qualified Bcc.Ledger.Aurum as Aurum
import qualified Bcc.Ledger.Aurum.Tx as Aurum
import qualified Bcc.Ledger.Aurum.TxWitness as Aurum

import           Bcc.Api.Address
import           Bcc.Api.Certificate
import           Bcc.Api.Eras
import           Bcc.Api.HasTypeProxy
import           Bcc.Api.Key
import           Bcc.Api.KeysCole
import           Bcc.Api.KeysSophie
import           Bcc.Api.NetworkId
import           Bcc.Api.SerialiseCBOR
import           Bcc.Api.SerialiseTextEnvelope
import           Bcc.Api.TxBody

-- ----------------------------------------------------------------------------
-- Signed transactions
--

data Tx era where

     ColeTx
       :: Cole.ATxAux ByteString
       -> Tx ColeEra

     SophieTx
       :: SophieBasedEra era
       -> Ledger.Tx (SophieLedgerEra era)
       -> Tx era

-- The GADT in the SophieTx case requires a custom instance
instance Eq (Tx era) where
    (==) (ColeTx txA)
         (ColeTx txB) = txA == txB

    (==) (SophieTx era txA)
         (SophieTx _   txB) =
      case era of
        SophieBasedEraSophie -> txA == txB
        SophieBasedEraEvie -> txA == txB
        SophieBasedEraJen    -> txA == txB
        SophieBasedEraAurum  -> txA == txB

    (==) ColeTx{} (SophieTx era _) = case era of {}

-- The GADT in the SophieTx case requires a custom instance
instance Show (Tx era) where
    showsPrec p (ColeTx tx) =
      showParen (p >= 11) $
        showString "ColeTx "
      . showsPrec 11 tx

    showsPrec p (SophieTx SophieBasedEraSophie tx) =
      showParen (p >= 11) $
        showString "SophieTx SophieBasedEraSophie "
      . showsPrec 11 tx

    showsPrec p (SophieTx SophieBasedEraEvie tx) =
      showParen (p >= 11) $
        showString "SophieTx SophieBasedEraEvie "
      . showsPrec 11 tx

    showsPrec p (SophieTx SophieBasedEraJen tx) =
      showParen (p >= 11) $
        showString "SophieTx SophieBasedEraJen "
      . showsPrec 11 tx

    showsPrec p (SophieTx SophieBasedEraAurum tx) =
      showParen (p >= 11) $
        showString "SophieTx SophieBasedEraAurum "
      . showsPrec 11 tx


instance HasTypeProxy era => HasTypeProxy (Tx era) where
    data AsType (Tx era) = AsTx (AsType era)
    proxyToAsType _ = AsTx (proxyToAsType (Proxy :: Proxy era))

pattern AsColeTx :: AsType (Tx ColeEra)
pattern AsColeTx   = AsTx AsColeEra
{-# COMPLETE AsColeTx #-}

pattern AsSophieTx :: AsType (Tx SophieEra)
pattern AsSophieTx = AsTx AsSophieEra
{-# COMPLETE AsSophieTx #-}


instance IsBccEra era => SerialiseAsCBOR (Tx era) where
    serialiseToCBOR (ColeTx tx) = CBOR.recoverBytes tx

    serialiseToCBOR (SophieTx era tx) =
      case era of
        SophieBasedEraSophie -> serialiseSophieBasedTx tx
        SophieBasedEraEvie -> serialiseSophieBasedTx tx
        SophieBasedEraJen    -> serialiseSophieBasedTx tx
        SophieBasedEraAurum  -> serialiseSophieBasedTx tx

    deserialiseFromCBOR _ bs =
      case bccEra :: BccEra era of
        ColeEra ->
          ColeTx <$>
            CBOR.decodeFullAnnotatedBytes
              "Cole Tx" fromCBOR (LBS.fromStrict bs)

        -- Use the same derialisation impl, but at different types:
        SophieEra -> deserialiseSophieBasedTx
                        (SophieTx SophieBasedEraSophie) bs
        EvieEra -> deserialiseSophieBasedTx
                        (SophieTx SophieBasedEraEvie) bs
        JenEra    -> deserialiseSophieBasedTx
                        (SophieTx SophieBasedEraJen) bs
        AurumEra  -> deserialiseSophieBasedTx
                        (SophieTx SophieBasedEraAurum) bs

-- | The serialisation format for the different Sophie-based eras are not the
-- same, but they can be handled generally with one overloaded implementation.
--
serialiseSophieBasedTx :: ToCBOR tx => tx -> ByteString
serialiseSophieBasedTx = CBOR.serialize'

deserialiseSophieBasedTx :: FromCBOR (CBOR.Annotator tx)
                          => (tx -> tx')
                          -> ByteString
                          -> Either CBOR.DecoderError tx'
deserialiseSophieBasedTx mkTx bs =
    mkTx <$> CBOR.decodeAnnotator "Sophie Tx" fromCBOR (LBS.fromStrict bs)


instance IsBccEra era => HasTextEnvelope (Tx era) where
    textEnvelopeType _ =
      case bccEra :: BccEra era of
        ColeEra   -> "TxSignedCole"
        SophieEra -> "TxSignedSophie"
        EvieEra -> "Tx EvieEra"
        JenEra    -> "Tx JenEra"
        AurumEra  -> "Tx AurumEra"


data KeyWitness era where

     ColeKeyWitness
       :: Cole.TxInWitness
       -> KeyWitness ColeEra

     SophieBootstrapWitness
       :: SophieBasedEra era
       -> Sophie.BootstrapWitness StandardCrypto
       -> KeyWitness era

     SophieKeyWitness
       :: SophieBasedEra era
       -> Sophie.WitVKey Sophie.Witness StandardCrypto
       -> KeyWitness era


-- The GADT in the Sophie cases requires a custom instance
instance Eq (KeyWitness era) where
    (==) (ColeKeyWitness wA)
         (ColeKeyWitness wB) = wA == wB

    (==) (SophieBootstrapWitness era wA)
         (SophieBootstrapWitness _   wB) =
      case era of
        SophieBasedEraSophie -> wA == wB
        SophieBasedEraEvie -> wA == wB
        SophieBasedEraJen    -> wA == wB
        SophieBasedEraAurum  -> wA == wB

    (==) (SophieKeyWitness era wA)
         (SophieKeyWitness _   wB) =
      case era of
        SophieBasedEraSophie -> wA == wB
        SophieBasedEraEvie -> wA == wB
        SophieBasedEraJen    -> wA == wB
        SophieBasedEraAurum  -> wA == wB

    (==) _ _ = False

-- The GADT in the SophieTx case requires a custom instance
--TODO: once we start providing custom patterns we should do the show in terms
-- of those. It'll be less verbose too!
instance Show (KeyWitness era) where
    showsPrec p (ColeKeyWitness tx) =
      showParen (p >= 11) $
        showString "ColeKeyWitness "
      . showsPrec 11 tx

    showsPrec p (SophieBootstrapWitness SophieBasedEraSophie tx) =
      showParen (p >= 11) $
        showString "SophieBootstrapWitness SophieBasedEraSophie "
      . showsPrec 11 tx

    showsPrec p (SophieBootstrapWitness SophieBasedEraEvie tx) =
      showParen (p >= 11) $
        showString "SophieBootstrapWitness SophieBasedEraEvie "
      . showsPrec 11 tx

    showsPrec p (SophieBootstrapWitness SophieBasedEraJen tx) =
      showParen (p >= 11) $
        showString "SophieBootstrapWitness SophieBasedEraJen "
      . showsPrec 11 tx

    showsPrec p (SophieBootstrapWitness SophieBasedEraAurum tx) =
      showParen (p >= 11) $
        showString "SophieBootstrapWitness SophieBasedEraAurum "
      . showsPrec 11 tx

    showsPrec p (SophieKeyWitness SophieBasedEraSophie tx) =
      showParen (p >= 11) $
        showString "SophieKeyWitness SophieBasedEraSophie "
      . showsPrec 11 tx

    showsPrec p (SophieKeyWitness SophieBasedEraEvie tx) =
      showParen (p >= 11) $
        showString "SophieKeyWitness SophieBasedEraEvie "
      . showsPrec 11 tx

    showsPrec p (SophieKeyWitness SophieBasedEraJen tx) =
      showParen (p >= 11) $
        showString "SophieKeyWitness SophieBasedEraJen "
      . showsPrec 11 tx

    showsPrec p (SophieKeyWitness SophieBasedEraAurum tx) =
      showParen (p >= 11) $
        showString "SophieKeyWitness SophieBasedEraAurum "
      . showsPrec 11 tx


instance HasTypeProxy era => HasTypeProxy (KeyWitness era) where
    data AsType (KeyWitness era) = AsKeyWitness (AsType era)
    proxyToAsType _ = AsKeyWitness (proxyToAsType (Proxy :: Proxy era))

pattern AsColeWitness :: AsType (KeyWitness ColeEra)
pattern AsColeWitness   = AsKeyWitness AsColeEra
{-# COMPLETE AsColeWitness #-}

pattern AsSophieWitness :: AsType (KeyWitness SophieEra)
pattern AsSophieWitness = AsKeyWitness AsSophieEra
{-# COMPLETE AsSophieWitness #-}


instance IsBccEra era => SerialiseAsCBOR (KeyWitness era) where
    serialiseToCBOR (ColeKeyWitness wit) = CBOR.serialize' wit

    serialiseToCBOR (SophieKeyWitness _era wit) =
      CBOR.serializeEncoding' $
      encodeSophieBasedKeyWitness wit

    serialiseToCBOR (SophieBootstrapWitness _era wit) =
      CBOR.serializeEncoding' $
      encodeSophieBasedBootstrapWitness wit

    deserialiseFromCBOR _ bs =
      case bccEra :: BccEra era of
        ColeEra ->
          ColeKeyWitness <$> CBOR.decodeFull' bs

        -- Use the same derialisation impl, but at different types:
        SophieEra -> decodeSophieBasedWitness SophieBasedEraSophie bs
        EvieEra -> decodeSophieBasedWitness SophieBasedEraEvie bs
        JenEra    -> decodeSophieBasedWitness SophieBasedEraJen    bs
        AurumEra  -> decodeSophieBasedWitness SophieBasedEraAurum  bs


encodeSophieBasedKeyWitness :: ToCBOR w => w -> CBOR.Encoding
encodeSophieBasedKeyWitness wit =
    CBOR.encodeListLen 2 <> CBOR.encodeWord 0 <> toCBOR wit

encodeSophieBasedBootstrapWitness :: ToCBOR w => w -> CBOR.Encoding
encodeSophieBasedBootstrapWitness wit =
    CBOR.encodeListLen 2 <> CBOR.encodeWord 1 <> toCBOR wit

decodeSophieBasedWitness :: forall era.
                             SophieBasedEra era
                          -> ByteString
                          -> Either CBOR.DecoderError (KeyWitness era)
decodeSophieBasedWitness era =
    CBOR.decodeAnnotator "Sophie Witness" decode . LBS.fromStrict
  where
    decode :: CBOR.Decoder s (CBOR.Annotator (KeyWitness era))
    decode =  do
      CBOR.decodeListLenOf 2
      t <- CBOR.decodeWord
      case t of
        0 -> fmap (fmap (SophieKeyWitness era)) fromCBOR
        1 -> fmap (fmap (SophieBootstrapWitness era)) fromCBOR
        _ -> CBOR.cborError $ CBOR.DecoderErrorUnknownTag
                                "Sophie Witness" (fromIntegral t)


instance IsBccEra era => HasTextEnvelope (KeyWitness era) where
    textEnvelopeType _ =
      case bccEra :: BccEra era of
        ColeEra   -> "TxWitnessCole"
        SophieEra -> "TxWitnessSophie"
        EvieEra -> "TxWitness EvieEra"
        JenEra    -> "TxWitness JenEra"
        AurumEra  -> "TxWitness AurumEra"

pattern Tx :: TxBody era -> [KeyWitness era] -> Tx era
pattern Tx txbody ws <- (getTxBodyAndWitnesses -> (txbody, ws))
  where
    Tx txbody ws = makeSignedTransaction ws txbody
{-# COMPLETE Tx #-}

getTxBodyAndWitnesses :: Tx era -> (TxBody era, [KeyWitness era])
getTxBodyAndWitnesses tx = (getTxBody tx, getTxWitnesses tx)

getTxBody :: forall era. Tx era -> TxBody era
getTxBody (ColeTx Cole.ATxAux { Cole.aTaTx = txbody }) =
    ColeTxBody txbody

getTxBody (SophieTx era tx) =
    case era of
      SophieBasedEraSophie -> getSophieTxBody tx
      SophieBasedEraEvie -> getSophieTxBody tx
      SophieBasedEraJen    -> getSophieTxBody tx
      SophieBasedEraAurum  -> getAurumTxBody ScriptDataInAurumEra TxScriptValiditySupportedInAurumEra tx
  where
    getSophieTxBody :: forall ledgerera.
                        SophieLedgerEra era ~ ledgerera
                     => Ledger.Witnesses ledgerera ~ Sophie.WitnessSetHKD Identity ledgerera
                     => Sophie.SophieBased ledgerera
                     => Sophie.Tx ledgerera
                     -> TxBody era
    getSophieTxBody Sophie.Tx {
                       Sophie.body       = txbody,
                       Sophie.auxiliaryData = txAuxiliaryData,
                       Sophie.wits = Sophie.WitnessSet
                                              _addrWits
                                               msigWits
                                              _bootWits
                     } =
      SophieTxBody era txbody
                    (Map.elems msigWits)
                    TxBodyNoScriptData
                    (strictMaybeToMaybe txAuxiliaryData)
                    TxScriptValidityNone

    getAurumTxBody :: forall ledgerera.
                       SophieLedgerEra era ~ ledgerera
                    => ScriptDataSupportedInEra era
                    -> TxScriptValiditySupportedInEra era
                    -> Aurum.ValidatedTx ledgerera
                    -> TxBody era
    getAurumTxBody scriptDataInEra txScriptValidityInEra
                    Aurum.ValidatedTx {
                      Aurum.body = txbody,
                      Aurum.wits = Aurum.TxWitness'
                                     _addrWits
                                     _bootWits
                                     txscripts
                                     txdats
                                     redeemers,
                      Aurum.auxiliaryData = auxiliaryData,
                      Aurum.isValid = isValid
                    } =
      SophieTxBody era txbody
                    (Map.elems txscripts)
                    (TxBodyScriptData scriptDataInEra txdats redeemers)
                    (strictMaybeToMaybe auxiliaryData)
                    (TxScriptValidity txScriptValidityInEra (isValidToScriptValidity isValid))


getTxWitnesses :: forall era. Tx era -> [KeyWitness era]
getTxWitnesses (ColeTx Cole.ATxAux { Cole.aTaWitness = witnesses }) =
    map ColeKeyWitness
  . Vector.toList
  . unAnnotated
  $ witnesses

getTxWitnesses (SophieTx era tx) =
    case era of
      SophieBasedEraSophie -> getSophieTxWitnesses tx
      SophieBasedEraEvie -> getSophieTxWitnesses tx
      SophieBasedEraJen    -> getSophieTxWitnesses tx
      SophieBasedEraAurum  -> getAurumTxWitnesses  tx
  where
    getSophieTxWitnesses :: forall ledgerera.
                             Ledger.Crypto ledgerera ~ StandardCrypto
                          => Ledger.Witnesses ledgerera ~ Sophie.WitnessSetHKD Identity ledgerera
                          => ToCBOR (Ledger.Witnesses ledgerera)
                          => Sophie.SophieBased ledgerera
                          => Sophie.Tx ledgerera
                          -> [KeyWitness era]
    getSophieTxWitnesses Sophie.Tx {
                            Sophie.wits =
                              Sophie.WitnessSet
                                addrWits
                               _msigWits
                                bootWits
                          } =
        map (SophieBootstrapWitness era) (Set.elems bootWits)
     ++ map (SophieKeyWitness       era) (Set.elems addrWits)

    getAurumTxWitnesses :: forall ledgerera.
                            Ledger.Crypto ledgerera ~ StandardCrypto
                         => Aurum.ValidatedTx ledgerera
                         -> [KeyWitness era]
    getAurumTxWitnesses Aurum.ValidatedTx {
                           Aurum.wits =
                             Aurum.TxWitness'
                               addrWits
                               bootWits
                               _txscripts
                               _txdats
                               _txrdmrs
                         } =
        map (SophieBootstrapWitness era) (Set.elems bootWits)
     ++ map (SophieKeyWitness       era) (Set.elems addrWits)

makeSignedTransaction :: forall era.
     [KeyWitness era]
  -> TxBody era
  -> Tx era
makeSignedTransaction witnesses (ColeTxBody txbody) =
    ColeTx
  . Cole.annotateTxAux
  $ Cole.mkTxAux
      (unAnnotated txbody)
      (Vector.fromList [ w | ColeKeyWitness w <- witnesses ])

makeSignedTransaction witnesses (SophieTxBody era txbody
                                               txscripts
                                               txscriptdata
                                               txmetadata
                                               scriptValidity
                                               ) =
    case era of
      SophieBasedEraSophie -> makeSophieSignedTransaction txbody
      SophieBasedEraEvie -> makeSophieSignedTransaction txbody
      SophieBasedEraJen    -> makeSophieSignedTransaction txbody
      SophieBasedEraAurum  -> makeAurumSignedTransaction  txbody
  where
    makeSophieSignedTransaction
      :: forall ledgerera.
         SophieLedgerEra era ~ ledgerera
      => Ledger.Crypto ledgerera ~ StandardCrypto
      => Ledger.Witnesses ledgerera ~ Sophie.WitnessSetHKD Identity ledgerera
      => Ledger.Tx ledgerera ~ Sophie.Tx ledgerera
      => ToCBOR (Ledger.Witnesses ledgerera)
      => Sophie.SophieBased ledgerera
      => Sophie.ValidateScript ledgerera
      => Ledger.TxBody ledgerera
      -> Tx era
    makeSophieSignedTransaction txbody' =
      SophieTx era $
        Sophie.Tx
          txbody'
          (Sophie.WitnessSet
            (Set.fromList [ w | SophieKeyWitness _ w <- witnesses ])
            (Map.fromList [ (Ledger.hashScript @ledgerera sw, sw)
                          | sw <- txscripts ])
            (Set.fromList [ w | SophieBootstrapWitness _ w <- witnesses ]))
          (maybeToStrictMaybe txmetadata)

    makeAurumSignedTransaction
      :: forall ledgerera.
         SophieLedgerEra era ~ ledgerera
      => Ledger.Crypto ledgerera ~ StandardCrypto
      => Ledger.Tx ledgerera ~ Aurum.ValidatedTx ledgerera
      => Ledger.Script ledgerera ~ Aurum.Script ledgerera
      => Sophie.SophieBased ledgerera
      => Sophie.ValidateScript ledgerera
      => Ledger.TxBody ledgerera
      -> Tx era
    makeAurumSignedTransaction txbody' =
      SophieTx era $
        Aurum.ValidatedTx
          txbody'
          (Aurum.TxWitness
            (Set.fromList [ w | SophieKeyWitness _ w <- witnesses ])
            (Set.fromList [ w | SophieBootstrapWitness _ w <- witnesses ])
            (Map.fromList [ (Ledger.hashScript @ledgerera sw, sw)
                          | sw <- txscripts ])
            datums
            redeemers)
          (txScriptValidityToIsValid scriptValidity)
          (maybeToStrictMaybe txmetadata)
      where
        (datums, redeemers) =
          case txscriptdata of
            TxBodyScriptData _ ds rs -> (ds, rs)
            TxBodyNoScriptData       -> (mempty, Aurum.Redeemers mempty)

makeColeKeyWitness :: forall key.
                       IsColeKey key
                    => NetworkId
                    -> TxBody ColeEra
                    -> SigningKey key
                    -> KeyWitness ColeEra
makeColeKeyWitness _ (SophieTxBody era _ _ _ _ _) = case era of {}
makeColeKeyWitness nw (ColeTxBody txbody) =
    let txhash :: Cole.Hash Cole.Tx
        txhash = Cole.hashDecoded txbody

        pm :: Cole.ProtocolMagicId
        pm = toColeProtocolMagicId nw

        -- To allow sharing of the txhash computation across many signatures we
        -- define and share the txhash outside the lambda for the signing key:
     in case coleKeyFormat :: ColeKeyFormat key of
          ColeLegacyKeyFormat ->
            \(ColeSigningKeyLegacy sk) -> witness sk pm txhash
          ColeModernKeyFormat ->
            \(ColeSigningKey sk) -> witness sk pm txhash
 where
   witness :: Cole.SigningKey
           -> Cole.ProtocolMagicId
           -> Cole.Hash Cole.Tx
           -> KeyWitness ColeEra
   witness sk pm txHash =
     ColeKeyWitness $
       Cole.VKWitness
         (Cole.toVerification sk)
         (Cole.sign pm Cole.SignTx sk (Cole.TxSigData txHash))

-- | Either a network ID or a Cole address to be used in constructing a
-- Sophie bootstrap witness.
data WitnessNetworkIdOrColeAddress
  = WitnessNetworkId !NetworkId
  -- ^ Network ID.
  --
  -- If this value is used in the construction of a Sophie bootstrap witness,
  -- the result will not consist of a derivation path. If that is required,
  -- specify a 'WitnessColeAddress' value instead.
  | WitnessColeAddress !(Address ColeAddr)
  -- ^ Cole address.
  --
  -- If this value is used in the construction of a Sophie bootstrap witness,
  -- both the network ID and derivation path will be extracted from the
  -- address and used in the construction of the witness.

makeSophieBootstrapWitness :: forall era.
                               IsSophieBasedEra era
                            => WitnessNetworkIdOrColeAddress
                            -> TxBody era
                            -> SigningKey ColeKey
                            -> KeyWitness era
makeSophieBootstrapWitness _ ColeTxBody{} _ =
    case sophieBasedEra :: SophieBasedEra era of {}

makeSophieBootstrapWitness nwOrAddr (SophieTxBody era txbody _ _ _ _) sk =
    case era of
      SophieBasedEraSophie ->
        makeSophieBasedBootstrapWitness era nwOrAddr txbody sk
      SophieBasedEraEvie ->
        makeSophieBasedBootstrapWitness era nwOrAddr txbody sk
      SophieBasedEraJen    ->
        makeSophieBasedBootstrapWitness era nwOrAddr txbody sk
      SophieBasedEraAurum  ->
        makeSophieBasedBootstrapWitness era nwOrAddr txbody sk

makeSophieBasedBootstrapWitness :: forall era.
                                    (Ledger.HashAnnotated
                                       (Ledger.TxBody (SophieLedgerEra era))
                                       Ledger.EraIndependentTxBody
                                       StandardCrypto)
                                 => SophieBasedEra era
                                 -> WitnessNetworkIdOrColeAddress
                                 -> Ledger.TxBody (SophieLedgerEra era)
                                 -> SigningKey ColeKey
                                 -> KeyWitness era
makeSophieBasedBootstrapWitness era nwOrAddr txbody (ColeSigningKey sk) =
    SophieBootstrapWitness era $
      -- Cole era witnesses were weird. This reveals all that weirdness.
      Sophie.BootstrapWitness {
        Sophie.bwKey        = vk,
        Sophie.bwSig        = signature,
        Sophie.bwChainCode  = chainCode,
        Sophie.bwAttributes = attributes
      }
  where
    -- Starting with the easy bits: we /can/ convert the Cole verification key
    -- to a the pair of a Sophie verification key plus the chain code.
    --
    (vk, chainCode) = Sophie.unpackColeVKey (Cole.toVerification sk)

    -- Now the hairy bits.
    --
    -- Cole era signing keys were all /extended/ ed25519 keys. We have to
    -- produce a signature using this extended signing key directly. They
    -- /cannot/ be converted to a plain (non-extended) signing keys. Since we
    -- now support extended signing keys for the Sophie too, we are able to
    -- reuse that here.
    --
    signature :: Sophie.SignedDSIGN StandardCrypto
                  (Sophie.Hash StandardCrypto Ledger.EraIndependentTxBody)
    signature = makeSophieSignature
                  txhash
                  -- Make the signature with the extended key directly:
                  (SophieExtendedSigningKey (Cole.unSigningKey sk))

    txhash :: Sophie.Hash StandardCrypto Ledger.EraIndependentTxBody
    txhash = Ledger.extractHash (Ledger.hashAnnotated txbody)
    --TODO: use Sophie.eraIndTxBodyHash txbody once that function has a
    -- suitably general type.

    -- And finally we need to provide the extra suffix bytes necessary to
    -- reconstruct the mini-Merkel tree that is a Cole address. The suffix
    -- bytes are the serialised address attributes.
    attributes =
      CBOR.serialize' $
        Cole.mkAttributes Cole.AddrAttributes {
          Cole.aaVKDerivationPath = derivationPath,
          Cole.aaNetworkMagic     = networkMagic
        }

    -- The 'WitnessNetworkIdOrColeAddress' value converted to an 'Either'.
    eitherNwOrAddr :: Either NetworkId (Address ColeAddr)
    eitherNwOrAddr =
      case nwOrAddr of
        WitnessNetworkId nw -> Left nw
        WitnessColeAddress addr -> Right addr

    unColeAddr :: Address ColeAddr -> Cole.Address
    unColeAddr (ColeAddress addr) = addr

    unAddrAttrs :: Address ColeAddr -> Cole.AddrAttributes
    unAddrAttrs = Cole.attrData . Cole.addrAttributes . unColeAddr

    derivationPath :: Maybe Cole.HDAddressPayload
    derivationPath =
      either
        (const Nothing)
        (Cole.aaVKDerivationPath . unAddrAttrs)
        eitherNwOrAddr

    networkMagic :: Cole.NetworkMagic
    networkMagic =
      either
        toColeNetworkMagic
        (Cole.aaNetworkMagic . unAddrAttrs)
        eitherNwOrAddr


data SophieWitnessSigningKey =
       WitnessPaymentKey         (SigningKey PaymentKey)
     | WitnessPaymentExtendedKey (SigningKey PaymentExtendedKey)
     | WitnessStakeKey           (SigningKey StakeKey)
     | WitnessStakeExtendedKey   (SigningKey StakeExtendedKey)
     | WitnessStakePoolKey       (SigningKey StakePoolKey)
     | WitnessGenesisKey         (SigningKey GenesisKey)
     | WitnessGenesisExtendedKey (SigningKey GenesisExtendedKey)
     | WitnessGenesisDelegateKey (SigningKey GenesisDelegateKey)
     | WitnessGenesisDelegateExtendedKey
                                 (SigningKey GenesisDelegateExtendedKey)
     | WitnessGenesisUTxOKey     (SigningKey GenesisUTxOKey)
     | WitnessVestedKey         (SigningKey VestedKey)
     | WitnessVestedExtendedKey (SigningKey VestedExtendedKey)
     | WitnessVestedDelegateKey (SigningKey VestedDelegateKey)
     | WitnessVestedDelegateExtendedKey
                                 (SigningKey VestedDelegateExtendedKey)
     | WitnessVestedUTxOKey     (SigningKey VestedUTxOKey)
     


makeSophieKeyWitness :: forall era
                      .  IsSophieBasedEra era
                      => TxBody era
                      -> SophieWitnessSigningKey
                      -> KeyWitness era
makeSophieKeyWitness (SophieTxBody era txbody _ _ _ _) =
    case era of
      SophieBasedEraSophie -> makeSophieBasedKeyWitness txbody
      SophieBasedEraEvie -> makeSophieBasedKeyWitness txbody
      SophieBasedEraJen    -> makeSophieBasedKeyWitness txbody
      SophieBasedEraAurum  -> makeSophieBasedKeyWitness txbody
  where
    makeSophieBasedKeyWitness :: Sophie.SophieBased (SophieLedgerEra era)
                               => Ledger.Crypto (SophieLedgerEra era)
                                    ~ StandardCrypto
                               => Ledger.TxBody (SophieLedgerEra era)
                               -> SophieWitnessSigningKey
                               -> KeyWitness era
    makeSophieBasedKeyWitness txbody' =

     let txhash :: Sophie.Hash StandardCrypto Ledger.EraIndependentTxBody
         txhash = Ledger.extractHash (Ledger.hashAnnotated txbody')

        -- To allow sharing of the txhash computation across many signatures we
        -- define and share the txhash outside the lambda for the signing key:
     in \wsk ->
        let sk        = toSophieSigningKey wsk
            vk        = getSophieKeyWitnessVerificationKey sk
            signature = makeSophieSignature txhash sk
         in SophieKeyWitness era $
              Sophie.WitVKey vk signature

makeSophieKeyWitness ColeTxBody{} =
    case sophieBasedEra :: SophieBasedEra era of {}


-- | We support making key witnesses with both normal and extended signing keys.
--
data SophieSigningKey =
       -- | A normal ed25519 signing key
       SophieNormalSigningKey   (Sophie.SignKeyDSIGN StandardCrypto)

       -- | An extended ed25519 signing key
     | SophieExtendedSigningKey Crypto.HD.XPrv


toSophieSigningKey :: SophieWitnessSigningKey -> SophieSigningKey
toSophieSigningKey key = case key of
  WitnessPaymentKey     (PaymentSigningKey     sk) -> SophieNormalSigningKey sk
  WitnessStakeKey       (StakeSigningKey       sk) -> SophieNormalSigningKey sk
  WitnessStakePoolKey   (StakePoolSigningKey   sk) -> SophieNormalSigningKey sk
  WitnessGenesisKey     (GenesisSigningKey     sk) -> SophieNormalSigningKey sk
  WitnessGenesisUTxOKey (GenesisUTxOSigningKey sk) -> SophieNormalSigningKey sk
  WitnessGenesisDelegateKey (GenesisDelegateSigningKey sk) -> SophieNormalSigningKey sk
  WitnessVestedKey       (VestedSigningKey       sk) -> SophieNormalSigningKey sk
  WitnessVestedUTxOKey   (VestedUTxOSigningKey   sk) -> SophieNormalSigningKey sk
  WitnessVestedDelegateKey   (VestedDelegateSigningKey   sk) -> SophieNormalSigningKey sk
    

  -- The cases for extended keys
  WitnessPaymentExtendedKey (PaymentExtendedSigningKey sk) ->
    SophieExtendedSigningKey sk

  WitnessStakeExtendedKey (StakeExtendedSigningKey sk) ->
    SophieExtendedSigningKey sk

  WitnessGenesisExtendedKey (GenesisExtendedSigningKey sk) ->
    SophieExtendedSigningKey sk

  WitnessGenesisDelegateExtendedKey (GenesisDelegateExtendedSigningKey sk) ->
    SophieExtendedSigningKey sk
  
  WitnessVestedExtendedKey (VestedExtendedSigningKey sk) ->
    SophieExtendedSigningKey sk

  WitnessVestedDelegateExtendedKey (VestedDelegateExtendedSigningKey sk) ->
    SophieExtendedSigningKey sk


getSophieKeyWitnessVerificationKey
  :: SophieSigningKey
  -> Sophie.VKey Sophie.Witness StandardCrypto
getSophieKeyWitnessVerificationKey (SophieNormalSigningKey sk) =
      (Sophie.coerceKeyRole :: Sophie.VKey Sophie.Payment StandardCrypto
                             -> Sophie.VKey Sophie.Witness StandardCrypto)
    . (\(PaymentVerificationKey vk) -> vk)
    . getVerificationKey
    . PaymentSigningKey
    $ sk

getSophieKeyWitnessVerificationKey (SophieExtendedSigningKey sk) =
      (Sophie.coerceKeyRole :: Sophie.VKey Sophie.Payment StandardCrypto
                             -> Sophie.VKey Sophie.Witness StandardCrypto)
    . (\(PaymentVerificationKey vk) -> vk)
    . (castVerificationKey :: VerificationKey PaymentExtendedKey
                           -> VerificationKey PaymentKey)
    . getVerificationKey
    . PaymentExtendedSigningKey
    $ sk


makeSophieSignature
  :: Crypto.SignableRepresentation tosign
  => tosign
  -> SophieSigningKey
  -> Sophie.SignedDSIGN StandardCrypto tosign
makeSophieSignature tosign (SophieNormalSigningKey sk) =
    Crypto.signedDSIGN () tosign sk

makeSophieSignature tosign (SophieExtendedSigningKey sk) =
    fromXSignature $
      Crypto.HD.sign
        BS.empty  -- passphrase for (unused) in-memory encryption
        sk
        (Crypto.getSignableRepresentation tosign)
  where
    fromXSignature :: Crypto.HD.XSignature
                   -> Sophie.SignedDSIGN StandardCrypto b
    fromXSignature =
        Crypto.SignedDSIGN
      . fromMaybe impossible
      . Crypto.rawDeserialiseSigDSIGN
      . Crypto.HD.unXSignature

    impossible =
      error "makeSophieKeyWitnessSignature: cole and sophie signature sizes do not match"


-- order of signing keys must match txins
signColeTransaction :: NetworkId
                     -> TxBody ColeEra
                     -> [SigningKey ColeKey]
                     -> Tx ColeEra
signColeTransaction nw txbody sks =
    makeSignedTransaction witnesses txbody
  where
    witnesses = map (makeColeKeyWitness nw txbody) sks

-- signing keys is a set
signSophieTransaction :: IsSophieBasedEra era
                       => TxBody era
                       -> [SophieWitnessSigningKey]
                       -> Tx era
signSophieTransaction txbody sks =
    makeSignedTransaction witnesses txbody
  where
    witnesses = map (makeSophieKeyWitness txbody) sks
