{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cli.MultiAssetParsing where

import           Bcc.Prelude hiding (filter)

import qualified Data.Text as Text
import qualified Text.Parsec as Parsec (parse)

import           Hedgehog (Property, checkSequential, discover, forAll, property, tripping)
import           Hedgehog.Gen (filter)

import           Bcc.Api (valueToList, renderValue, renderValuePretty)
import           Bcc.CLI.Jen.ValueParser (parseValue)

import           Gen.Bcc.Api.Typed (genValueDefault)

prop_roundtrip_Value_parse_render :: Property
prop_roundtrip_Value_parse_render =
  property $ do
    value <- forAll $ filter (not . null . valueToList) genValueDefault
    tripping
      value
      renderValue
      (Parsec.parse parseValue "" . Text.unpack)

prop_roundtrip_Value_parse_renderPretty :: Property
prop_roundtrip_Value_parse_renderPretty =
  property $ do
    value <- forAll $ filter (not . null . valueToList) genValueDefault
    tripping
      value
      renderValuePretty
      (Parsec.parse parseValue "" . Text.unpack)

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  checkSequential $$discover
