{-# LANGUAGE FlexibleInstances #-}

module Main
  ( main
  ) where

import           Prelude

import qualified System.Environment as E
import qualified Test.Tasty as T
import qualified Test.Tasty.Hedgehog as H
import qualified Test.Tasty.Ingredients as T

import qualified Spec.Zerepoch.Direct.TxInLockingZerepoch
import qualified Spec.Zerepoch.Script.TxInLockingZerepoch
import qualified Spec.Zerepoch.SubmitApi.TxInLockingZerepoch

tests :: IO T.TestTree
tests = do
  pure $ T.testGroup "test/Spec.hs"
    [ T.testGroup "Spec"
      [ H.testProperty "Spec.Zerepoch.Direct.TxInLockingZerepoch" Spec.Zerepoch.Direct.TxInLockingZerepoch.hprop_zerepoch
      , H.testProperty "Spec.Zerepoch.Script.TxInLockingZerepoch" Spec.Zerepoch.Script.TxInLockingZerepoch.hprop_zerepoch
      , H.testProperty "Spec.Zerepoch.SubmitApi.TxInLockingZerepoch" Spec.Zerepoch.SubmitApi.TxInLockingZerepoch.hprop_zerepoch
      ]
    ]

ingredients :: [T.Ingredient]
ingredients = T.defaultIngredients

main :: IO ()
main = do
  args <- E.getArgs

  E.withArgs args $ tests >>= T.defaultMainWithIngredients ingredients
