{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Bcc.Api.Typed.Orphans () where

import           Bcc.Prelude

import           Bcc.Api.Sophie
import           Bcc.Crypto.Hash hiding (Hash)
import           Bcc.Crypto.KES
import           Bcc.Crypto.Libsodium (SodiumHashAlgorithm)

import           Test.Bcc.Crypto.Orphans ()

-- Signing Key instances

deriving instance Eq (SigningKey ColeKey)
deriving instance Eq (SigningKey PaymentKey)
deriving instance Eq (SigningKey StakeKey)
deriving instance Eq (SigningKey StakePoolKey)
deriving instance Eq (SigningKey GenesisKey)
deriving instance Eq (SigningKey GenesisDelegateKey)
deriving instance Eq (SigningKey GenesisUTxOKey)
deriving instance Eq (SigningKey KesKey)
deriving instance Eq (SigningKey VrfKey)


instance ( KESAlgorithm d
         , SodiumHashAlgorithm h
         , SizeHash h ~ SeedSizeKES d
         ) => Eq (SignKeyKES (SumKES h d)) where
  k1 == k2 = rawSerialiseSignKeyKES k1 == rawSerialiseSignKeyKES k2
