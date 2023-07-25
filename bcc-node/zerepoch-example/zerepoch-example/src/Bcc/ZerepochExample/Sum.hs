{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Bcc.ZerepochExample.Sum
  where

import           Prelude hiding (($),(==),(+),(-))

import           Bcc.Api.Sophie (ZerepochScript (..), ZerepochScriptV1)

import           Codec.Serialise
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS

import qualified Ledger.Typed.Scripts as Scripts
import qualified Zerepoch.V1.Ledger.Scripts as Zerepoch
import qualified ZerepochTx
import           ZerepochTx.Prelude hiding (Semigroup (..), unless)


smartSum :: Integer -> Integer
smartSum a = loop a 0
 where
  loop !n !acc = if n==0
    then acc
    else loop (n - 1) (n + acc)

-- | The validation function (DataValue -> RedeemerValue -> ScriptContext -> Bool)
{-# INLINABLE validateSum #-}
validateSum :: Integer -> Integer -> x -> Bool
validateSum n s _ = isGoodSum n s

{-# INLINABLE isGoodSum #-}
isGoodSum :: Integer -> Integer -> Bool
isGoodSum n s = smartSum n == s


data SmartSum
instance Scripts.ValidatorTypes SmartSum where
    type instance RedeemerType SmartSum = Integer
    type instance DatumType SmartSum = Integer

sumInstance :: Scripts.TypedValidator SmartSum
sumInstance = Scripts.mkTypedValidator @SmartSum
    $$(ZerepochTx.compile [|| validateSum ||])
    $$(ZerepochTx.compile [|| wrap ||])
      where
        wrap = Scripts.wrapValidator @Integer @Integer

validator :: Zerepoch.Validator
validator = Scripts.validatorScript sumInstance

script :: Zerepoch.Script
script = Zerepoch.unValidatorScript validator

sumScriptShortBs :: SBS.ShortByteString
sumScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

sumScript :: ZerepochScript ZerepochScriptV1
sumScript = ZerepochScriptSerialised sumScriptShortBs
