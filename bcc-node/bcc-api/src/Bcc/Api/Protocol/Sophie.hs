-- | Node client support for the Sophie protocol
--
module Bcc.Api.Protocol.Sophie
  ( -- * Client support
    mkNodeClientProtocolSophie
  , mkSomeNodeClientProtocolSophie
  ) where


import           Shardagnostic.Consensus.Sophie.SophieHFC

import           Shardagnostic.Consensus.Sophie.Eras (StandardSophie)

import           Bcc.Api.Protocol.Types


mkNodeClientProtocolSophie :: ProtocolClientInfoArgs (SophieBlockHFC StandardSophie)
mkNodeClientProtocolSophie = ProtocolClientInfoArgsSophie


mkSomeNodeClientProtocolSophie :: SomeNodeClientProtocol
mkSomeNodeClientProtocolSophie =
    SomeNodeClientProtocol mkNodeClientProtocolSophie
