{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Bcc.ZerepochExample.AlwaysSucceeds
  ( alwaysSucceedsScript
  , alwaysSucceedsScriptShortBs
  ) where

import           Prelude hiding (($))

import           Bcc.Api.Sophie (ZerepochScript (..), ZerepochScriptV1)

import           Codec.Serialise
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS

import qualified Zerepoch.V1.Ledger.Scripts as Zerepoch
import qualified ZerepochTx
import           ZerepochTx.Prelude hiding (Semigroup (..), unless)

{-# INLINABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator _ _ _ = ()

validator :: Zerepoch.Validator
validator = Zerepoch.mkValidatorScript $$(ZerepochTx.compile [|| mkValidator ||])

script :: Zerepoch.Script
script = Zerepoch.unValidatorScript validator

alwaysSucceedsScriptShortBs :: SBS.ShortByteString
alwaysSucceedsScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

alwaysSucceedsScript :: ZerepochScript ZerepochScriptV1
alwaysSucceedsScript = ZerepochScriptSerialised alwaysSucceedsScriptShortBs

