{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Bcc.ZerepochExample.CustomDatumRedeemerGuess
  ( MyCustomDatum(..)
  , MyCustomRedeemer(..)
  , customGuessScript
  , customDatumRedeemerGuessScriptAsShortBs
  ) where

import           Prelude hiding (($), (&&), (==))

import           Bcc.Api.Sophie (ZerepochScript (..), ZerepochScriptV1)

import           Codec.Serialise
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS

import           Ledger.Contexts (ScriptContext (..))
import qualified Ledger.Typed.Scripts as Scripts
import qualified Zerepoch.V1.Ledger.Scripts as Zerepoch
import qualified ZerepochTx
import           ZerepochTx.Prelude hiding (Semigroup (..), unless)

newtype MyCustomDatum = MyCustomDatum Integer
newtype MyCustomRedeemer = MyCustomRedeemer Integer

ZerepochTx.unstableMakeIsData ''MyCustomDatum
ZerepochTx.unstableMakeIsData ''MyCustomRedeemer

data ExampleTypedValidator
instance Scripts.ValidatorTypes ExampleTypedValidator where
    type instance DatumType ExampleTypedValidator    = MyCustomDatum
    type instance RedeemerType ExampleTypedValidator = MyCustomRedeemer

{-# INLINABLE mkValidator #-}
mkValidator :: MyCustomDatum-> MyCustomRedeemer -> ScriptContext -> Bool
mkValidator (MyCustomDatum d) (MyCustomRedeemer r) _ =
  d == 42 && r == 42

inst :: Scripts.TypedValidator ExampleTypedValidator
inst = Scripts.mkTypedValidator @ExampleTypedValidator
    $$(ZerepochTx.compile [|| mkValidator ||])
    $$(ZerepochTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @MyCustomDatum @MyCustomRedeemer

validator :: Zerepoch.Validator
validator = Scripts.validatorScript inst

script :: Zerepoch.Script
script = Zerepoch.unValidatorScript validator

customDatumRedeemerGuessScriptAsShortBs :: SBS.ShortByteString
customDatumRedeemerGuessScriptAsShortBs = SBS.toShort . LBS.toStrict $ serialise script

customGuessScript :: ZerepochScript ZerepochScriptV1
customGuessScript = ZerepochScriptSerialised customDatumRedeemerGuessScriptAsShortBs

