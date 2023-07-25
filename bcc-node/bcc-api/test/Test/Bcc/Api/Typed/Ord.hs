{-# LANGUAGE TemplateHaskell #-}

module Test.Bcc.Api.Typed.Ord
  ( tests
  ) where

import           Bcc.Api
import           Prelude
import           Bcc.Api.Sophie
import           Gen.Bcc.Api.Typed
import           Gen.Tasty.Hedgehog.Group (fromGroup)
import           Hedgehog (Property, discover, (===))
import           Test.Bcc.Api.Metadata (genTxMetadataValue)
import           Test.Tasty (TestTree)

import qualified Hedgehog as H

{- HLINT ignore "Use camelCase" -}


ord_distributive :: (Show a, Ord a, Ord b)
                      => H.Gen a -> (a -> b) -> Property
ord_distributive gen to =
    H.property $ do
      x <- H.forAll gen
      y <- H.forAll gen
      compare x y === compare (to x) (to y)


prop_ord_distributive_TxId :: Property
prop_ord_distributive_TxId =
    ord_distributive genTxId toSophieTxId

prop_ord_distributive_TxIn :: Property
prop_ord_distributive_TxIn =
    ord_distributive genTxIn toSophieTxIn

prop_ord_distributive_Address :: Property
prop_ord_distributive_Address =
    ord_distributive genAddressSophie (toSophieAddr . toAddressInAnyEra)
  where
    toAddressInAnyEra :: Address SophieAddr -> AddressInEra SophieEra
    toAddressInAnyEra = anyAddressInSophieBasedEra . toAddressAny

prop_ord_distributive_StakeAddress :: Property
prop_ord_distributive_StakeAddress =
    ord_distributive genStakeAddress toSophieStakeAddr

prop_ord_distributive_TxMetadata :: Property
prop_ord_distributive_TxMetadata =
    ord_distributive genTxMetadataValue toSophieMetadatum

prop_ord_distributive_ScriptData :: Property
prop_ord_distributive_ScriptData =
    ord_distributive genScriptData toZerepochData

-- -----------------------------------------------------------------------------

tests :: TestTree
tests = fromGroup $$discover

