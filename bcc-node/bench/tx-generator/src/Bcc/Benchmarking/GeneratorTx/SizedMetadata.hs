{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Use uncurry" -}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Bcc.Benchmarking.GeneratorTx.SizedMetadata
where

import           Prelude

import qualified Data.Map.Strict as Map
import           Data.Word (Word64)
import qualified Data.ByteString as BS
import           Bcc.Benchmarking.GeneratorTx.Tx
import           Bcc.Api

maxMapSize :: Int
maxMapSize = 1000
maxBSSize :: Int
maxBSSize = 64

-- Properties of the underlying/opaque CBOR encoding.
assume_cbor_properties :: Bool
assume_cbor_properties
  =    prop_mapCostsSophie
    && prop_mapCostsEvie
    && prop_mapCostsJen
    && prop_mapCostsAurum
    && prop_bsCostsSophie
    && prop_bsCostsEvie
    && prop_bsCostsJen
    && prop_bsCostsAurum

-- The cost of map entries in metadata follows a step function.
-- This assums the map indecies are [0..n].
prop_mapCostsSophie :: Bool
prop_mapCostsEvie :: Bool
prop_mapCostsJen    :: Bool
prop_mapCostsAurum  :: Bool
prop_mapCostsSophie = measureMapCosts AsSophieEra == assumeMapCosts AsSophieEra
prop_mapCostsEvie = measureMapCosts AsEvieEra == assumeMapCosts AsEvieEra
prop_mapCostsJen    = measureMapCosts AsJenEra    == assumeMapCosts AsJenEra
prop_mapCostsAurum  = measureMapCosts AsAurumEra  == assumeMapCosts AsAurumEra

assumeMapCosts :: forall era . IsSophieBasedEra era => AsType era -> [Int]
assumeMapCosts _proxy = stepFunction [
      (   1 , 0)          -- An empty map of metadata has the same cost as TxMetadataNone.
    , (   1 , firstEntry) -- Using Metadata costs 37 or 39 bytes  (first map entry).
    , (  22 , 2)          -- The next 22 entries cost 2 bytes each.
    , ( 233 , 3)          -- 233 entries at 3 bytes.
    , ( 744 , 4)          -- 744 entries at 4 bytes.
    ]
  where
    firstEntry = case sophieBasedEra @ era of
      SophieBasedEraSophie -> 37
      SophieBasedEraEvie -> 39
      SophieBasedEraJen    -> 39
 -- Unconfirmed ! update when aurum is runnable.
      SophieBasedEraAurum  -> error "39"

-- Bytestring costs are not LINEAR !!
-- Costs are piecewise linear for payload sizes [0..23] and [24..64].
prop_bsCostsSophie  :: Bool
prop_bsCostsEvie :: Bool
prop_bsCostsJen    :: Bool
prop_bsCostsAurum  :: Bool
prop_bsCostsSophie  = measureBSCosts AsSophieEra == [37..60] ++ [62..102]
prop_bsCostsEvie = measureBSCosts AsEvieEra == [39..62] ++ [64..104]
prop_bsCostsJen    = measureBSCosts AsJenEra    == [39..62] ++ [64..104]
 -- Unconfirmed ! update when aurum is runnable.
prop_bsCostsAurum  = measureBSCosts AsAurumEra  == error "[39..62] ++ [64..104]"

stepFunction :: [(Int, Int)] -> [Int]
stepFunction f = scanl1 (+) steps
 where steps = concatMap (\(count,step) -> replicate count step) f

-- Measure the cost of metadata map entries.
-- This is the cost of the index with an empty BS as payload.
measureMapCosts :: forall era . IsSophieBasedEra era => AsType era -> [Int]
measureMapCosts era = map (metadataSize era . Just . replicateEmptyBS) [0..maxMapSize]
 where
  replicateEmptyBS :: Int -> TxMetadata
  replicateEmptyBS n = listMetadata $ replicate n $ TxMetaBytes BS.empty

listMetadata :: [TxMetadataValue] -> TxMetadata
listMetadata l = makeTransactionMetadata $ Map.fromList $ zip [0..] l

-- Cost of metadata with a single BS of size [0..maxBSSize].
measureBSCosts :: forall era . IsSophieBasedEra era => AsType era -> [Int]
measureBSCosts era = map (metadataSize era . Just . bsMetadata) [0..maxBSSize]
 where bsMetadata s = listMetadata [TxMetaBytes $ BS.replicate s 0]

metadataSize :: forall era . IsSophieBasedEra era => AsType era -> Maybe TxMetadata -> Int
metadataSize p m = dummyTxSize p m - dummyTxSize p Nothing

dummyTxSizeInEra :: forall era . IsSophieBasedEra era => TxMetadataInEra era -> Int
dummyTxSizeInEra metadata = case makeTransactionBody dummyTx of
  Right b -> BS.length $ serialiseToCBOR b
  Left err -> error $ "metaDataSize " ++ show err
 where
  dummyTx :: TxBodyContent BuildTx era
  dummyTx = TxBodyContent {
      txIns = [( TxIn "dbaff4e270cfb55612d9e2ac4658a27c79da4a5271c6f90853042d1403733810" (TxIx 0)
               , BuildTxWith $ KeyWitness KeyWitnessForSpending )]
    , txInsCollateral = TxInsCollateralNone
    , txOuts = []
    , txFee = mkFee 0
    , txValidityRange = (TxValidityNoLowerBound, mkValidityUpperBound 0)
    , txMetadata = metadata
    , txAuxScripts = TxAuxScriptsNone
    , txExtraScriptData = BuildTxWith TxExtraScriptDataNone
    , txExtraKeyWits = TxExtraKeyWitnessesNone
    , txProtocolParams = BuildTxWith Nothing
    , txWithdrawals = TxWithdrawalsNone
    , txCertificates = TxCertificatesNone
    , txUpdateProposal = TxUpdateProposalNone
    , txMintValue = TxMintNone
    , txScriptValidity = TxScriptValidityNone
    }

dummyTxSize :: forall era . IsSophieBasedEra era => AsType era -> Maybe TxMetadata -> Int
dummyTxSize _p m = (dummyTxSizeInEra @ era) $ metadataInEra m

metadataInEra :: forall era . IsSophieBasedEra era => Maybe TxMetadata -> TxMetadataInEra era
metadataInEra Nothing = TxMetadataNone
metadataInEra (Just m) = case sophieBasedEra @ era of
  SophieBasedEraSophie -> TxMetadataInEra TxMetadataInSophieEra m
  SophieBasedEraEvie -> TxMetadataInEra TxMetadataInEvieEra m
  SophieBasedEraJen    -> TxMetadataInEra TxMetadataInJenEra m
  SophieBasedEraAurum  -> TxMetadataInEra TxMetadataInAurumEra m

mkMetadata :: forall era . IsSophieBasedEra era => Int -> Either String (TxMetadataInEra era)
mkMetadata 0 = Right $ metadataInEra Nothing
mkMetadata size
  = if size < minSize
      then Left $ "Error : metadata must be 0 or at least " ++ show minSize ++ " bytes in this era."
      else Right $ metadataInEra $ Just metadata
 where
  minSize = case sophieBasedEra @ era of
    SophieBasedEraSophie -> 37
    SophieBasedEraEvie -> 39
    SophieBasedEraJen    -> 39
    SophieBasedEraAurum  -> error "39"
  nettoSize = size - minSize

  -- At 24 the CBOR representation changes.
  maxLinearByteStringSize = 23
  fullChunkSize = maxLinearByteStringSize + 1

  -- A full chunk consists of 4 bytes for the index and 20 bytes for the bytestring.
  -- Each full chunk adds exactly `fullChunkSize` (== 24) bytes.
  -- The remainder is added in the first chunk.
  mkFullChunk ix = (ix, TxMetaBytes $ BS.replicate (fullChunkSize - 4) 0)

  fullChunkCount :: Word64
  fullChunkCount = fromIntegral $ nettoSize `div` fullChunkSize

  -- Full chunks use indices starting at 1000, to enforce 4-byte encoding of the index.
  -- At some index the encoding will change to 5 bytes and this will break.
  fullChunks = map mkFullChunk [1000 .. 1000 + fullChunkCount -1]

  -- The first chunk has a variable size.
  firstChunk =
    ( 0  -- the first chunk uses index 0
    , TxMetaBytes $ BS.replicate (nettoSize `mod` fullChunkSize) 0
    )

  metadata = makeTransactionMetadata $ Map.fromList (firstChunk : fullChunks)
