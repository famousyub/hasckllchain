
module Bcc.Api.ChainSync.Client (
      -- * Protocol type for the client
      -- | The protocol states from the point of view of the client.
      ChainSyncClient(..)
    , ClientStIdle(..)
    , ClientStNext(..)
    , ClientStIntersect(..)

      -- * Null chain sync client
    , chainSyncClientNull

      -- * Utilities
    , mapChainSyncClient
    ) where

import           Shardagnostic.Network.Protocol.ChainSync.Client
