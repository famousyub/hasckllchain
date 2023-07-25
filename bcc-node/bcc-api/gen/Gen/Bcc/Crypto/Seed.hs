module Gen.Bcc.Crypto.Seed
  ( genSeed
  , genSeedForKey
  ) where

import           Bcc.Api (AsType, Key)
import           Bcc.Crypto.Seed (Seed)
import           Data.Functor ((<$>))
import           Data.Int (Int)
import           Hedgehog (MonadGen, Range)
import           Prelude (fromIntegral)

import qualified Bcc.Api as API
import qualified Bcc.Crypto.Seed as C
import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R

genSeed :: MonadGen m => Range Int -> m Seed
genSeed r = C.mkSeedFromBytes <$> G.bytes r

genSeedForKey :: (Key key, MonadGen m) => AsType key -> m Seed
genSeedForKey keyRole = genSeed (R.singleton (fromIntegral (API.deterministicSigningKeySeedSize keyRole)))
