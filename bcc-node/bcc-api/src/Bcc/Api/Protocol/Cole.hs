-- | Node client support for the Cole protocol
--
module Bcc.Api.Protocol.Cole
  ( -- * Client support
    mkNodeClientProtocolCole
  , mkSomeNodeClientProtocolCole
  ) where

import           Bcc.Api.Protocol.Types (ProtocolClient(..),
                     ProtocolClientInfoArgs(ProtocolClientInfoArgsCole),
                     SomeNodeClientProtocol(..))
import           Bcc.Chain.Slotting (EpochSlots)
import           Shardagnostic.Consensus.Bcc.ColeHFC

mkNodeClientProtocolCole :: EpochSlots
                          -> ProtocolClientInfoArgs ColeBlockHFC
mkNodeClientProtocolCole = ProtocolClientInfoArgsCole

mkSomeNodeClientProtocolCole :: EpochSlots
                              -> SomeNodeClientProtocol
mkSomeNodeClientProtocolCole epochSlots =
    SomeNodeClientProtocol
      (mkNodeClientProtocolCole epochSlots)
