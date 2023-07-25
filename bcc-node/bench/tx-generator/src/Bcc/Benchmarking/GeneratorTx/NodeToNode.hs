{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans -Wno-unticked-promoted-constructors -Wno-all-missed-specialisations #-}

module Bcc.Benchmarking.GeneratorTx.NodeToNode
  ( ConnectClient
  , benchmarkConnectTxSubmit
  ) where

import           Bcc.Prelude (forever, liftIO)
import           Prelude

import           Codec.Serialise (DeserialiseFailure)
import           Control.Monad.Class.MonadTimer (MonadTimer, threadDelay)
import           Control.Monad.Class.MonadSTM.Strict (newTVarIO)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.Map as Map
import           Data.Proxy (Proxy (..))
import           Network.Socket (AddrInfo (..))
import           System.Random (newStdGen)

import           Control.Tracer (Tracer, nullTracer)
import           Shardagnostic.Consensus.Cole.Ledger.Mempool (GenTx)
import           Shardagnostic.Consensus.Block.Abstract
import qualified Shardagnostic.Consensus.Bcc as Consensus (BccBlock)
import           Shardagnostic.Consensus.Ledger.SupportsMempool (GenTxId)
import           Shardagnostic.Consensus.Network.NodeToNode (Codecs (..), defaultCodecs)
import           Shardagnostic.Consensus.Node.NetworkProtocolVersion
import           Shardagnostic.Consensus.Node.Run (RunNode)
import           Shardagnostic.Consensus.Sophie.Protocol (StandardCrypto)

import           Shardagnostic.Network.Channel (Channel (..))
import           Shardagnostic.Network.DeltaQ (defaultGSV)
import           Shardagnostic.Network.Driver (runPeerWithLimits)
import           Shardagnostic.Network.KeepAlive
import           Shardagnostic.Network.Magic
import           Shardagnostic.Network.Mux (MuxPeer (..), RunMiniProtocol (..), continueForever)
import           Shardagnostic.Network.NodeToClient (chainSyncPeerNull, IOManager)
import           Shardagnostic.Network.NodeToNode (NetworkConnectTracers (..))
import qualified Shardagnostic.Network.NodeToNode as NtN
import           Shardagnostic.Network.Protocol.BlockFetch.Client (BlockFetchClient (..),
                                                               blockFetchClientPeer)
import           Shardagnostic.Network.Protocol.Handshake.Version (simpleSingletonVersions)
import           Shardagnostic.Network.Protocol.KeepAlive.Codec
import           Shardagnostic.Network.Protocol.KeepAlive.Client
import           Shardagnostic.Network.Protocol.TxSubmission.Client (TxSubmissionClient,
                                                                 txSubmissionClientPeer)
import           Shardagnostic.Network.Protocol.Trans.Hello.Util (wrapClientPeer)

import           Shardagnostic.Network.Snocket (socketSnocket)

import           Bcc.Benchmarking.Tracer (SendRecvConnect, SendRecvTxSubmission2)

type BccBlock    = Consensus.BccBlock  StandardCrypto
type ConnectClient = AddrInfo -> TxSubmissionClient (GenTxId BccBlock) (GenTx BccBlock) IO () -> IO ()

benchmarkConnectTxSubmit
  :: forall blk. (blk ~ BccBlock, RunNode blk )
  => IOManager
  -> Tracer IO SendRecvConnect
  -> Tracer IO SendRecvTxSubmission2
  -> CodecConfig BccBlock
  -> NetworkMagic
  -> AddrInfo
  -- ^ remote address information
  -> TxSubmissionClient (GenTxId blk) (GenTx blk) IO ()
  -- ^ the particular txSubmission peer
  -> IO ()

benchmarkConnectTxSubmit ioManager handshakeTracer submissionTracer codecConfig networkMagic remoteAddr myTxSubClient =
  NtN.connectTo
    (socketSnocket ioManager)
    NetworkConnectTracers {
        nctMuxTracer       = nullTracer,
        nctHandshakeTracer = handshakeTracer
      }
    peerMultiplex
    (addrAddress <$> Nothing)
    (addrAddress remoteAddr)
 where
  n2nVer :: NodeToNodeVersion
  n2nVer = NodeToNodeV_7
  blkN2nVer :: BlockNodeToNodeVersion blk
  blkN2nVer = supportedVers Map.! n2nVer
  supportedVers :: Map.Map NodeToNodeVersion (BlockNodeToNodeVersion blk)
  supportedVers = supportedNodeToNodeVersions (Proxy @blk)
  myCodecs :: Codecs blk DeserialiseFailure IO
                ByteString ByteString ByteString ByteString ByteString ByteString ByteString
  myCodecs  = defaultCodecs codecConfig blkN2nVer n2nVer
  peerMultiplex =
    simpleSingletonVersions
      n2nVer
      (NtN.NodeToNodeVersionData
       { NtN.networkMagic = networkMagic
       , NtN.diffusionMode = NtN.InitiatorOnlyDiffusionMode
       }) $
      NtN.nodeToNodeProtocols NtN.defaultMiniProtocolParameters ( \them _ ->
        NtN.NodeToNodeProtocols
          { NtN.chainSyncProtocol = InitiatorProtocolOnly $
                                      MuxPeer
                                        nullTracer
                                        (cChainSyncCodec myCodecs)
                                        chainSyncPeerNull
          , NtN.blockFetchProtocol = InitiatorProtocolOnly $
                                       MuxPeer
                                         nullTracer
                                         (cBlockFetchCodec myCodecs)
                                         (blockFetchClientPeer blockFetchClientNull)
          , NtN.keepAliveProtocol = InitiatorProtocolOnly $
                                      MuxPeerRaw
                                        (kaClient n2nVer them)
          , NtN.txSubmissionProtocol = InitiatorProtocolOnly $
                                         MuxPeer
                                           submissionTracer
                                           (cTxSubmission2Codec myCodecs)
                                           (wrapClientPeer $ txSubmissionClientPeer myTxSubClient)
          } )
        n2nVer
  -- Stolen from: Shardagnostic/Consensus/Network/NodeToNode.hs
  kaClient
    :: Ord remotePeer
    => NodeToNodeVersion
    -> remotePeer
    -> Channel IO ByteString
    -> IO ((), Maybe ByteString)
  kaClient version them channel = do
    case version of
      -- Version 1 doesn't support keep alive protocol but Blockfetch
      -- still requires a PeerGSV per peer.
      NodeToNodeV_1 -> forever (threadDelay 1000) >> return ((), Nothing)
      NodeToNodeV_2 -> forever (threadDelay 1000) >> return ((), Nothing)
      _             -> do
        keepAliveRng <- newStdGen
        peerGSVMap <- liftIO . newTVarIO $ Map.singleton them defaultGSV
        runPeerWithLimits
          nullTracer
          (cKeepAliveCodec myCodecs)
          (byteLimitsKeepAlive (const 0)) -- TODO: Real Bytelimits, see #1727
          timeLimitsKeepAlive
          channel
          $ keepAliveClientPeer
          $ keepAliveClient
              nullTracer
              keepAliveRng
              (continueForever (Proxy :: Proxy IO)) them peerGSVMap
              (KeepAliveInterval 10)

-- the null block fetch client
blockFetchClientNull
  :: forall block point m a.  MonadTimer m
  => BlockFetchClient block point m a
blockFetchClientNull
  = BlockFetchClient $ forever $ threadDelay (24 * 60 * 60) {- one day in seconds -}
