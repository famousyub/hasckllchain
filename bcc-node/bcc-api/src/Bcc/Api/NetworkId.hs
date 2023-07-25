-- | The 'NetworkId' type and related functions
--
module Bcc.Api.NetworkId (
    -- * Network types
    NetworkId(..),
    NetworkMagic(..),
    toNetworkMagic,
    mainnetNetworkMagic,

    -- * Internal conversion functions
    toColeProtocolMagicId,
    toColeNetworkMagic,
    toColeRequiresNetworkMagic,
    toSophieNetwork,
    fromSophieNetwork,
  ) where

import           Prelude

import           Shardagnostic.Network.Magic (NetworkMagic (..))

import qualified Bcc.Crypto.ProtocolMagic as Cole
                   (ProtocolMagicId(..), RequiresNetworkMagic(..))
import qualified Bcc.Chain.Common as Cole (NetworkMagic(..))
import qualified Bcc.Chain.Genesis as Cole (mainnetProtocolMagicId)
import qualified Bcc.Ledger.BaseTypes as Sophie (Network(..))


-- ----------------------------------------------------------------------------
-- NetworkId type
--

data NetworkId = Mainnet
               | Testnet !NetworkMagic
  deriving (Eq, Show)

toNetworkMagic :: NetworkId -> NetworkMagic
toNetworkMagic (Testnet nm) = nm
toNetworkMagic Mainnet      = mainnetNetworkMagic

mainnetNetworkMagic :: NetworkMagic
mainnetNetworkMagic = NetworkMagic
                    . Cole.unProtocolMagicId
                    $ Cole.mainnetProtocolMagicId


-- ----------------------------------------------------------------------------
-- Cole conversion functions
--

toColeProtocolMagicId :: NetworkId -> Cole.ProtocolMagicId
toColeProtocolMagicId Mainnet = Cole.mainnetProtocolMagicId
toColeProtocolMagicId (Testnet (NetworkMagic pm)) = Cole.ProtocolMagicId pm

toColeNetworkMagic :: NetworkId -> Cole.NetworkMagic
toColeNetworkMagic Mainnet                     = Cole.NetworkMainOrStage
toColeNetworkMagic (Testnet (NetworkMagic nm)) = Cole.NetworkTestnet nm

toColeRequiresNetworkMagic :: NetworkId -> Cole.RequiresNetworkMagic
toColeRequiresNetworkMagic Mainnet   = Cole.RequiresNoMagic
toColeRequiresNetworkMagic Testnet{} = Cole.RequiresMagic


-- ----------------------------------------------------------------------------
-- Sophie conversion functions
--

toSophieNetwork :: NetworkId -> Sophie.Network
toSophieNetwork  Mainnet    = Sophie.Mainnet
toSophieNetwork (Testnet _) = Sophie.Testnet

fromSophieNetwork :: Sophie.Network -> NetworkMagic -> NetworkId
fromSophieNetwork Sophie.Testnet nm = Testnet nm
fromSophieNetwork Sophie.Mainnet nm
  | nm == mainnetNetworkMagic = Mainnet
  | otherwise = error "fromSophieNetwork Mainnet: wrong mainnet network magic"

