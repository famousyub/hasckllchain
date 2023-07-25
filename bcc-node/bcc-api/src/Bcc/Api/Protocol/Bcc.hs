-- | Node client support for the Bcc protocol
--
module Bcc.Api.Protocol.Bcc
  ( -- * Client support
    mkNodeClientProtocolBcc
  , mkSomeNodeClientProtocolBcc
  ) where

import           Bcc.Api.Protocol.Types (ProtocolClient(..),
                     ProtocolClientInfoArgs(ProtocolClientInfoArgsBcc),
                     SomeNodeClientProtocol (..))
import           Bcc.Chain.Slotting (EpochSlots)
import           Shardagnostic.Consensus.Bcc.Block (BccBlock)
import           Bcc.Ledger.Crypto (StandardCrypto)

mkNodeClientProtocolBcc :: EpochSlots
                            -> ProtocolClientInfoArgs (BccBlock StandardCrypto)
mkNodeClientProtocolBcc = ProtocolClientInfoArgsBcc

mkSomeNodeClientProtocolBcc :: EpochSlots
                                -> SomeNodeClientProtocol
mkSomeNodeClientProtocolBcc epochSlots =
    SomeNodeClientProtocol
      (mkNodeClientProtocolBcc epochSlots)
