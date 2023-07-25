{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Currency values
--
module Bcc.Api.Value
  ( Entropic(..)

    -- * Multi-asset values
  , Quantity(..)
  , PolicyId(..)
  , scriptPolicyId
  , AssetName(..)
  , AssetId(..)
  , Value
  , selectAsset
  , valueFromList
  , valueToList
  , filterValue
  , negateValue
  , calcMinimumDeposit

    -- ** Bcc \/ Entropic specifically
  , quantityToEntropic
  , entropicToQuantity
  , selectEntropic
  , entropicToValue
  , valueToEntropic

    -- ** Alternative nested representation
  , ValueNestedRep(..)
  , ValueNestedBundle(..)
  , valueToNestedRep
  , valueFromNestedRep

    -- ** Rendering
  , renderValue
  , renderValuePretty

    -- * Internal conversion functions
  , toColeEntropic
  , fromColeEntropic
  , toSophieEntropic
  , fromSophieEntropic
  , fromSophieDeltaEntropic
  , toJenValue
  , fromJenValue

    -- * Data family instances
  , AsType(..)
  ) where

import           Prelude

import           Data.Aeson hiding (Value)
import qualified Data.Aeson as Aeson
import           Data.Aeson.Types (Parser, toJSONKeyText)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Merge.Strict as Map
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.String (IsString (..))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import qualified Bcc.Chain.Common as Cole

import qualified Bcc.Ledger.Coin as Sophie
import qualified Bcc.Ledger.Jen.Value as Jen
import qualified Bcc.Ledger.SophieMA.Rules.Utxo as Sophie
import           Bcc.Ledger.Crypto (StandardCrypto)

import           Bcc.Api.HasTypeProxy
import           Bcc.Api.Script
import           Bcc.Api.SerialiseCBOR
import           Bcc.Api.SerialiseRaw
import           Bcc.Api.SerialiseUsing


-- ----------------------------------------------------------------------------
-- Entropic
--

newtype Entropic = Entropic Integer
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum, Num, ToJSON, FromJSON, ToCBOR, FromCBOR)

instance Semigroup Entropic where
  Entropic a <> Entropic b = Entropic (a + b)

instance Monoid Entropic where
  mempty = Entropic 0


toColeEntropic :: Entropic -> Maybe Cole.Entropic
toColeEntropic (Entropic x) =
    case Cole.integerToEntropic x of
      Left  _  -> Nothing
      Right x' -> Just x'

fromColeEntropic :: Cole.Entropic -> Entropic
fromColeEntropic = Entropic . Cole.entropicToInteger

toSophieEntropic :: Entropic -> Sophie.Coin
toSophieEntropic (Entropic l) = Sophie.Coin l
--TODO: validate bounds

fromSophieEntropic :: Sophie.Coin -> Entropic
fromSophieEntropic (Sophie.Coin l) = Entropic l

fromSophieDeltaEntropic :: Sophie.DeltaCoin -> Entropic
fromSophieDeltaEntropic (Sophie.DeltaCoin d) = Entropic d


-- ----------------------------------------------------------------------------
-- Multi asset Value
--

newtype Quantity = Quantity Integer
  deriving newtype (Eq, Ord, Num, Show, ToJSON, FromJSON)

instance Semigroup Quantity where
  Quantity a <> Quantity b = Quantity (a + b)

instance Monoid Quantity where
  mempty = Quantity 0

entropicToQuantity :: Entropic -> Quantity
entropicToQuantity (Entropic x) = Quantity x

quantityToEntropic :: Quantity -> Entropic
quantityToEntropic (Quantity x) = Entropic x


newtype PolicyId = PolicyId ScriptHash
  deriving stock (Eq, Ord)
  deriving (Show, IsString, ToJSON, FromJSON) via UsingRawBytesHex PolicyId

instance HasTypeProxy PolicyId where
    data AsType PolicyId = AsPolicyId
    proxyToAsType _ = AsPolicyId

instance SerialiseAsRawBytes PolicyId where
    serialiseToRawBytes (PolicyId sh) = serialiseToRawBytes sh
    deserialiseFromRawBytes AsPolicyId bs =
      PolicyId <$> deserialiseFromRawBytes AsScriptHash bs

scriptPolicyId :: Script lang -> PolicyId
scriptPolicyId = PolicyId . hashScript


newtype AssetName = AssetName ByteString
    deriving stock (Eq, Ord)
    deriving newtype (Show)    

instance IsString AssetName where
    fromString s
      | let bs = Text.encodeUtf8 (Text.pack s)
      , BS.length bs <= 32 = AssetName (BSC.pack s)
      | otherwise          = error "fromString: AssetName over 32 bytes"

instance HasTypeProxy AssetName where
    data AsType AssetName = AsAssetName
    proxyToAsType _ = AsAssetName

instance SerialiseAsRawBytes AssetName where
    serialiseToRawBytes (AssetName bs) = bs
    deserialiseFromRawBytes AsAssetName bs
      | BS.length bs <= 32 = Just (AssetName bs)
      | otherwise          = Nothing

instance ToJSON AssetName where
  toJSON (AssetName an) = Aeson.String $ Text.decodeUtf8 an

instance FromJSON AssetName where
  parseJSON = withText "AssetName" (return . AssetName . Text.encodeUtf8)

instance ToJSONKey AssetName where
  toJSONKey = toJSONKeyText (\(AssetName asset) -> Text.decodeUtf8 asset)

instance FromJSONKey AssetName where
  fromJSONKey = FromJSONKeyText (AssetName . Text.encodeUtf8)


data AssetId = BccAssetId
             | AssetId !PolicyId !AssetName
  deriving (Eq, Ord, Show)


newtype Value = Value (Map AssetId Quantity)
  deriving Eq

instance Show Value where
  showsPrec d v = showParen (d > 10) $
    showString "valueFromList " . shows (valueToList v)

instance Semigroup Value where
  Value a <> Value b = Value (mergeAssetMaps a b)

instance Monoid Value where
  mempty = Value Map.empty


{-# NOINLINE mergeAssetMaps #-} -- as per advice in Data.Map.Merge docs
mergeAssetMaps :: Map AssetId Quantity
               -> Map AssetId Quantity
               -> Map AssetId Quantity
mergeAssetMaps =
    Map.merge
      Map.preserveMissing
      Map.preserveMissing
      (Map.zipWithMaybeMatched mergeQuantity)
  where
    mergeQuantity :: AssetId -> Quantity -> Quantity -> Maybe Quantity
    mergeQuantity _k a b =
      case a <> b of
        Quantity 0 -> Nothing
        c          -> Just c

instance ToJSON Value where
  toJSON = toJSON . valueToNestedRep

instance FromJSON Value where
  parseJSON v = valueFromNestedRep <$> parseJSON v


selectAsset :: Value -> (AssetId -> Quantity)
selectAsset (Value m) a = Map.findWithDefault mempty a m

valueFromList :: [(AssetId, Quantity)] -> Value
valueFromList = Value
              . Map.filter (/= 0)
              . Map.fromListWith (<>)

valueToList :: Value -> [(AssetId, Quantity)]
valueToList (Value m) = Map.toList m

-- | This lets you write @a - b@ as @a <> negateValue b@.
--
negateValue :: Value -> Value
negateValue (Value m) = Value (Map.map negate m)

filterValue :: (AssetId -> Bool) -> Value -> Value
filterValue p (Value m) = Value (Map.filterWithKey (\k _v -> p k) m)

selectEntropic :: Value -> Entropic
selectEntropic = quantityToEntropic . flip selectAsset BccAssetId

entropicToValue :: Entropic -> Value
entropicToValue = Value . Map.singleton BccAssetId . entropicToQuantity

-- | Check if the 'Value' consists of /only/ 'Entropic' and no other assets,
-- and if so then return the Entropic.
--
-- See also 'selectEntropic' to select the Entropic quantity from the Value,
-- ignoring other assets.
--
valueToEntropic :: Value -> Maybe Entropic
valueToEntropic v =
    case valueToList v of
      []                -> Just (Entropic 0)
      [(BccAssetId, q)] -> Just (quantityToEntropic q)
      _                 -> Nothing

toJenValue :: Value -> Jen.Value StandardCrypto
toJenValue v =
    Jen.Value entropic other
  where
    Quantity entropic = selectAsset v BccAssetId
      --TODO: write QC tests to show it's ok to use Map.fromAscListWith here
    other = Map.fromListWith Map.union
              [ (toJenPolicyID pid, Map.singleton (toJenAssetName name) q)
              | (AssetId pid name, Quantity q) <- valueToList v ]

    toJenPolicyID :: PolicyId -> Jen.PolicyID StandardCrypto
    toJenPolicyID (PolicyId sh) = Jen.PolicyID (toSophieScriptHash sh)

    toJenAssetName :: AssetName -> Jen.AssetName
    toJenAssetName (AssetName n) = Jen.AssetName n


fromJenValue :: Jen.Value StandardCrypto -> Value
fromJenValue (Jen.Value entropic other) =
    Value $
      --TODO: write QC tests to show it's ok to use Map.fromAscList here
      Map.fromList $
        [ (BccAssetId, Quantity entropic) | entropic /= 0 ]
     ++ [ (AssetId (fromJenPolicyID pid) (fromJenAssetName name), Quantity q)
        | (pid, as) <- Map.toList other
        , (name, q) <- Map.toList as ]
  where
    fromJenPolicyID :: Jen.PolicyID StandardCrypto -> PolicyId
    fromJenPolicyID (Jen.PolicyID sh) = PolicyId (fromSophieScriptHash sh)

    fromJenAssetName :: Jen.AssetName -> AssetName
    fromJenAssetName (Jen.AssetName n) = AssetName n

-- | Calculate cost of making a UTxO entry for a given 'Value' and
-- mininimum UTxO value derived from the 'ProtocolParameters'
calcMinimumDeposit :: Value -> Entropic -> Entropic
calcMinimumDeposit v minUTxo =
  fromSophieEntropic $ Sophie.scaledMinDeposit (toJenValue v) (toSophieEntropic minUTxo)

-- ----------------------------------------------------------------------------
-- An alternative nested representation
--

-- | An alternative nested representation for 'Value' that groups assets that
-- share a 'PolicyId'.
--
newtype ValueNestedRep = ValueNestedRep [ValueNestedBundle]
  deriving (Eq, Ord, Show)

-- | A bundle within a 'ValueNestedRep' for a single 'PolicyId', or for the
-- special case of bcc.
--
data ValueNestedBundle = ValueNestedBundleBcc Quantity
                       | ValueNestedBundle PolicyId (Map AssetName Quantity)
  deriving (Eq, Ord, Show)


valueToNestedRep :: Value -> ValueNestedRep
valueToNestedRep v =
    -- unflatten all the non-bcc assets, and add bcc separately
    ValueNestedRep $
        [ ValueNestedBundleBcc q | let q = selectAsset v BccAssetId, q /= 0 ]
     ++ [ ValueNestedBundle pId qs | (pId, qs) <- Map.toList nonBccAssets ]
  where
    nonBccAssets :: Map PolicyId (Map AssetName Quantity)
    nonBccAssets =
      Map.fromListWith (Map.unionWith (<>))
        [ (pId, Map.singleton aName q)
        | (AssetId pId aName, q) <- valueToList v ]

valueFromNestedRep :: ValueNestedRep -> Value
valueFromNestedRep (ValueNestedRep bundles) =
    valueFromList
      [ (aId, q)
      | bundle   <- bundles
      , (aId, q) <- case bundle of
                      ValueNestedBundleBcc  q  -> [ (BccAssetId, q) ]
                      ValueNestedBundle pId qs -> [ (AssetId pId aName, q)
                                                  | (aName, q) <- Map.toList qs ]
      ]

instance ToJSON ValueNestedRep where
  toJSON (ValueNestedRep bundles) = object $ map toPair bundles
    where
     toPair :: ValueNestedBundle -> (Text, Aeson.Value)
     toPair (ValueNestedBundleBcc q) = ("entropic", toJSON q)
     toPair (ValueNestedBundle pid assets) = (renderPolicyId pid, toJSON assets)

instance FromJSON ValueNestedRep where
  parseJSON =
      withObject "ValueNestedRep" $ \obj ->
        ValueNestedRep <$> sequenceA [ parsePid keyValTuple
                                   | keyValTuple <- HashMap.toList obj ]
    where
      parsePid :: (Text, Aeson.Value) -> Parser ValueNestedBundle
      parsePid ("entropic", q) = ValueNestedBundleBcc <$> parseJSON q
      parsePid (pid, q) =
        case deserialiseFromRawBytesHex AsScriptHash (Text.encodeUtf8 pid) of
          Just sHash -> ValueNestedBundle (PolicyId sHash) <$> parseJSON q
          Nothing -> fail $ "Failure when deserialising PolicyId: "
                         <> Text.unpack pid

-- ----------------------------------------------------------------------------
-- Printing and pretty-printing
--

-- | Render a textual representation of a 'Value'.
--
renderValue :: Value -> Text
renderValue  v =
    Text.intercalate
      " + "
      (map renderAssetIdQuantityPair vals)
  where
    vals :: [(AssetId, Quantity)]
    vals = valueToList v

-- | Render a \"prettified\" textual representation of a 'Value'.
renderValuePretty :: Value -> Text
renderValuePretty v =
    Text.intercalate
      ("\n" <> Text.replicate 4 " " <> "+ ")
      (map renderAssetIdQuantityPair vals)
  where
    vals :: [(AssetId, Quantity)]
    vals = valueToList v

renderAssetIdQuantityPair :: (AssetId, Quantity) -> Text
renderAssetIdQuantityPair (aId, quant) =
  Text.pack (show quant) <> " " <> renderAssetId aId

renderPolicyId :: PolicyId -> Text
renderPolicyId (PolicyId scriptHash) = serialiseToRawBytesHexText scriptHash

renderAssetId :: AssetId -> Text
renderAssetId BccAssetId = "entropic"
renderAssetId (AssetId polId (AssetName assetName))
  | BS.null assetName = renderPolicyId polId
  | otherwise         = renderPolicyId polId <> "." <> Text.decodeUtf8 assetName
