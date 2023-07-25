{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Gen.Bcc.Api
  ( genMetadata
  , genAurumGenesis
  ) where

import           Bcc.Prelude
import           Control.Monad (MonadFail(fail))
import qualified Data.Map.Strict as Map

--TODO: why do we have this odd split? We can get rid of the old name "typed"
import           Gen.Bcc.Api.Typed (genRational)

import           Sophie.Spec.Ledger.Metadata (Metadata (..), Metadatum (..))
import qualified Bcc.Ledger.Aurum.Genesis as Aurum
import qualified Bcc.Ledger.Aurum.Language as Aurum
import qualified Bcc.Ledger.Aurum.Scripts as Aurum
import qualified Bcc.Ledger.BaseTypes as Ledger
import qualified Bcc.Ledger.Coin as Ledger

import           Hedgehog (Gen, Range)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

genMetadata :: Gen (Metadata era)
genMetadata = do
  numberOfIndicies <- Gen.integral (Range.linear 1 15)
  let indexes = map (\i -> fromIntegral i :: Word64) [1..numberOfIndicies]
  mDatums <- Gen.list (Range.singleton numberOfIndicies) genMetadatum
  return . Metadata . Map.fromList $ zip indexes mDatums

genMetadatum :: Gen Metadatum
genMetadatum = do
  int <- Gen.list (Range.linear 1 5) (I <$> Gen.integral (Range.linear 1 100))
  bytes <- Gen.list (Range.linear 1 5) (B <$> Gen.bytes (Range.linear 1 20))
  str <- Gen.list (Range.linear 1 5) (S <$> Gen.text (Range.linear 1 20) Gen.alphaNum)
  let mDatumList = int ++ bytes ++ str

  singleMetadatum <- Gen.element mDatumList

  Gen.element
    [ List mDatumList
    , Map [(singleMetadatum, singleMetadatum)]
    , Map [(List mDatumList, singleMetadatum)]
    , Map [(singleMetadatum, List mDatumList)]
    ]

genCoin :: Range Integer -> Gen Ledger.Coin
genCoin r = do
  unCoin' <- Gen.integral r
  return $ Ledger.Coin unCoin'

genPrice :: Gen Ledger.NonNegativeInterval
genPrice = do
  unPrice <- genRational
  case Ledger.boundRational unPrice of
    Nothing -> fail "genPrice: genRational should give us a bounded rational"
    Just p -> pure p

genLanguage :: Gen Aurum.Language
genLanguage = return Aurum.ZerepochV1

genPrices :: Gen Aurum.Prices
genPrices = do
  prMem'   <- genPrice
  prSteps' <- genPrice

  return Aurum.Prices
    { Aurum.prMem = prMem'
    , Aurum.prSteps = prSteps'
    }

genExUnits :: Gen Aurum.ExUnits
genExUnits = do
  exUnitsMem' <- Gen.word64 (Range.linear 0 10)
  exUnitsSteps' <- Gen.word64 (Range.linear 0 10)
  return Aurum.ExUnits
    { Aurum.exUnitsMem = exUnitsMem'
    , Aurum.exUnitsSteps = exUnitsSteps'
    }

genCostModel :: Range Int -> Gen Text -> Gen Integer -> Gen Aurum.CostModel
genCostModel r gt gi = do
  map' <- Gen.map r ((,) <$> gt <*> gi)
  return $ Aurum.CostModel map'

genAurumGenesis :: Gen Aurum.AurumGenesis
genAurumGenesis = do
  coinsPerUTxOWord <- genCoin (Range.linear 0 5)
  costmdls' <- Gen.map (Range.linear 0 5) $ (,)
    <$> genLanguage
    <*> genCostModel (Range.linear 0 5)
          (Gen.text (Range.linear 0 10) Gen.alphaNum)
          (Gen.integral (Range.linear 0 100))
  prices' <- genPrices
  maxTxExUnits' <- genExUnits
  maxBlockExUnits' <- genExUnits
  maxValSize' <- Gen.integral (Range.linear 0 10)
  collateralPercentage' <- Gen.integral (Range.linear 0 10)
  maxCollateralInputs' <- Gen.integral (Range.linear 0 10)

  return Aurum.AurumGenesis
    { Aurum.coinsPerUTxOWord = coinsPerUTxOWord
    , Aurum.costmdls = costmdls'
    , Aurum.prices = prices'
    , Aurum.maxTxExUnits = maxTxExUnits'
    , Aurum.maxBlockExUnits = maxBlockExUnits'
    , Aurum.maxValSize = maxValSize'
    , Aurum.collateralPercentage = collateralPercentage'
    , Aurum.maxCollateralInputs = maxCollateralInputs'
    }
