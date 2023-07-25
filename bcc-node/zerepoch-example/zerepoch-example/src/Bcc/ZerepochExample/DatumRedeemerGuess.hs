{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Bcc.ZerepochExample.DatumRedeemerGuess
  ( guessScript
  , datumRedeemerGuessScriptShortBs
  ) where

import           Prelude hiding (($), (&&), (==))

import           Bcc.Api.Sophie (ZerepochScript (..), ZerepochScriptV1)

import           Codec.Serialise
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS

import qualified Zerepoch.V1.Ledger.Scripts as Zerepoch
import           ZerepochTx (toBuiltinData)
import qualified ZerepochTx
import           ZerepochTx.Prelude hiding (Semigroup (..), unless)

{-# INLINABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData  -> ()
mkValidator datum redeemer _txContext
  |    datum    == toBuiltinData (42 :: Integer)
    && redeemer == toBuiltinData (42 :: Integer) = ()
  | otherwise = traceError "Incorrect datum. Expected 42."

validator :: Zerepoch.Validator
validator = Zerepoch.mkValidatorScript $$(ZerepochTx.compile [|| mkValidator ||])

script :: Zerepoch.Script
script = Zerepoch.unValidatorScript validator

datumRedeemerGuessScriptShortBs :: SBS.ShortByteString
datumRedeemerGuessScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

guessScript :: ZerepochScript ZerepochScriptV1
guessScript = ZerepochScriptSerialised datumRedeemerGuessScriptShortBs

