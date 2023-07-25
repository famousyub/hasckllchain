{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Bcc.ZerepochExample.MintingScript
  ( apiExampleZerepochMintingScript
  , mintingScriptShortBs
  ) where

import           Prelude hiding (($))

import           Bcc.Api.Sophie (ZerepochScript (..), ZerepochScriptV1)

import           Codec.Serialise
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Short as SBS

import           Ledger hiding (singleton)
import qualified Ledger.Typed.Scripts as Scripts
import qualified ZerepochTx
import           ZerepochTx.Prelude hiding (Semigroup (..), unless)


{- HLINT ignore "Avoid lambda" -}

{-# INLINABLE mkPolicy #-}
mkPolicy :: BuiltinData -> ScriptContext -> Bool
mkPolicy _redeemer _ctx = True


policy :: Scripts.MintingPolicy
policy = mkMintingPolicyScript
    $$(ZerepochTx.compile [|| Scripts.wrapMintingPolicy mkPolicy ||])


zerepochScript :: Script
zerepochScript =
  unMintingPolicyScript policy

validator :: Validator
validator =
  Validator $ unMintingPolicyScript policy

scriptAsCbor :: LB.ByteString
scriptAsCbor = serialise validator

apiExampleZerepochMintingScript :: ZerepochScript ZerepochScriptV1
apiExampleZerepochMintingScript = ZerepochScriptSerialised . SBS.toShort $ LB.toStrict scriptAsCbor

mintingScriptShortBs :: SBS.ShortByteString
mintingScriptShortBs = SBS.toShort . LB.toStrict $ scriptAsCbor

