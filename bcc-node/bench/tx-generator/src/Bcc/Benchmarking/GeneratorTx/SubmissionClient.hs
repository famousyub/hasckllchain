{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bcc.Benchmarking.GeneratorTx.SubmissionClient
  ( SubmissionThreadStats(..)
  , TxSource(..)
  , txSubmissionClient
  ) where

import           Prelude (error, fail)
import           Bcc.Prelude hiding (ByteString, atomically, retry, state, threadDelay)

import           Control.Arrow ((&&&))

import qualified Data.List as L
import qualified Data.List.Extra as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

import           Control.Tracer (Tracer, traceWith)

import           Bcc.Tracing.OrphanInstances.Cole ()
import           Bcc.Tracing.OrphanInstances.Common ()
import           Bcc.Tracing.OrphanInstances.Consensus ()
import           Bcc.Tracing.OrphanInstances.Network ()
import           Bcc.Tracing.OrphanInstances.Sophie ()

import qualified Shardagnostic.Consensus.Bcc as Consensus (BccBlock)
import           Shardagnostic.Consensus.Ledger.SupportsMempool (GenTx, GenTxId, txInBlockSize)
import qualified Shardagnostic.Consensus.Ledger.SupportsMempool as Mempool
import           Shardagnostic.Consensus.Sophie.Ledger.Mempool (mkSophieTx)
import qualified Shardagnostic.Consensus.Sophie.Ledger.Mempool as Mempool (TxId(SophieTxId))
import           Shardagnostic.Consensus.Sophie.Protocol (StandardCrypto)

import           Shardagnostic.Consensus.Bcc.Block (GenTx (GenTxEvie, GenTxAurum, GenTxSophie, GenTxJen))
import qualified Shardagnostic.Consensus.Bcc.Block as Block (TxId(GenTxIdSophie, GenTxIdEvie, GenTxIdAurum, GenTxIdJen))

import           Shardagnostic.Network.Protocol.TxSubmission.Client (ClientStIdle (..),
                                                                 ClientStTxIds (..),
                                                                 ClientStTxs (..),
                                                                 TxSubmissionClient (..))
import           Shardagnostic.Network.Protocol.TxSubmission.Type (BlockingReplyList (..),
                                                               TokBlockingStyle (..), TxSizeInBytes)

import           Bcc.Api
import           Bcc.Api.Sophie (Tx(SophieTx), fromSophieTxId)

import           Bcc.Benchmarking.Tracer
import           Bcc.Benchmarking.Types
type BccBlock    = Consensus.BccBlock  StandardCrypto

data SubmissionThreadStats
  = SubmissionThreadStats
      { stsAcked       :: {-# UNPACK #-} !Ack
      , stsSent        :: {-# UNPACK #-} !Sent
      , stsUnavailable :: {-# UNPACK #-} !Unav
      }

data TxSource era
  = Exhausted
  | Active (ProduceNextTxs era)

type ProduceNextTxs era = (forall m blocking . MonadIO m => TokBlockingStyle blocking -> Req -> m (TxSource era, [Tx era]))

produceNextTxs :: forall m blocking era . MonadIO m => TokBlockingStyle blocking -> Req -> LocalState era -> m (LocalState era, [Tx era])
produceNextTxs blocking req (txProducer, unack, stats) = do
  (newTxProducer, txList) <- produceNextTxs' blocking req txProducer
  return ((newTxProducer, unack, stats), txList)

produceNextTxs' :: forall m blocking era . MonadIO m => TokBlockingStyle blocking -> Req -> TxSource era -> m (TxSource era, [Tx era])
produceNextTxs' _ _ Exhausted = return (Exhausted, [])
produceNextTxs' blocking req (Active callback) = callback blocking req

type LocalState era = (TxSource era, UnAcked (Tx era), SubmissionThreadStats)
type EndOfProtocolCallback m = SubmissionThreadStats -> m ()

txSubmissionClient
  :: forall m era tx.
     ( MonadIO m, MonadFail m
     , IsSophieBasedEra era
     , tx      ~ Tx era
     )
  => Tracer m NodeToNodeSubmissionTrace
  -> Tracer m (TraceBenchTxSubmit TxId)
  -> TxSource era
  -> EndOfProtocolCallback m
  -> TxSubmissionClient (GenTxId BccBlock) (GenTx BccBlock) m ()
txSubmissionClient tr bmtr initialTxSource endOfProtocolCallback =
  TxSubmissionClient $
    pure $ client (initialTxSource, UnAcked [], SubmissionThreadStats 0 0 0)
 where
  discardAcknowledged :: TokBlockingStyle a -> Ack -> LocalState era -> m (LocalState era)
  discardAcknowledged blocking (Ack ack) (txSource, UnAcked unAcked, stats) = do
    when (tokIsBlocking blocking && ack /= length unAcked) $ do
      let err = "decideAnnouncement: TokBlocking, but length unAcked != ack"
      traceWith bmtr (TraceBenchTxSubError err)
      fail (T.unpack err)
    let (stillUnacked, acked) = L.splitAtEnd ack unAcked
    let newStats = stats { stsAcked = stsAcked stats + Ack ack }
    traceWith bmtr $ TraceBenchTxSubServAck  (getTxId . getTxBody <$> acked)
    return (txSource, UnAcked stillUnacked, newStats)

  queueNewTxs :: [tx] -> LocalState era -> LocalState era
  queueNewTxs newTxs (txSource, UnAcked unAcked, stats)
    = (txSource, UnAcked (newTxs <> unAcked), stats)

  -- Sadly, we can't just return what we want, we instead have to
  -- communicate via IORefs, because..
  -- The () return type is forced by Shardagnostic.Network.NodeToNode.connectTo
  client ::LocalState era -> ClientStIdle (GenTxId BccBlock) (GenTx BccBlock) m ()

  client localState = ClientStIdle
    { recvMsgRequestTxIds = requestTxIds localState
    , recvMsgRequestTxs = requestTxs localState
    }

  requestTxIds :: forall blocking.
       LocalState era
    -> TokBlockingStyle blocking
    -> Word16
    -> Word16
    -> m (ClientStTxIds blocking (GenTxId BccBlock) (GenTx BccBlock) m ())
  requestTxIds state blocking ackNum reqNum = do
    let ack = Ack $ fromIntegral ackNum
        req = Req $ fromIntegral reqNum
    traceWith tr $ reqIdsTrace ack req blocking
    stateA <- discardAcknowledged blocking ack state

    (stateB, newTxs) <- produceNextTxs blocking req stateA
    let stateC@(_, UnAcked outs , stats) = queueNewTxs newTxs stateB

    traceWith tr $ idListTrace (ToAnnce newTxs) blocking
    traceWith bmtr $ TraceBenchTxSubServAnn  (getTxId . getTxBody <$> newTxs)
    traceWith bmtr $ TraceBenchTxSubServOuts (getTxId . getTxBody <$> outs)

    case blocking of
      TokBlocking -> case NE.nonEmpty newTxs of
        Nothing -> do
          traceWith tr EndOfProtocol
          endOfProtocolCallback stats
          pure $ SendMsgDone ()
        (Just txs) -> pure $ SendMsgReplyTxIds
                              (BlockingReply $ txToIdSize <$> txs)
                              (client stateC)
      TokNonBlocking ->  pure $ SendMsgReplyTxIds
                             (NonBlockingReply $ txToIdSize <$> newTxs)
                             (client stateC)
                    
  requestTxs ::
       LocalState era
    -> [GenTxId BccBlock]
    -> m (ClientStTxs (GenTxId BccBlock) (GenTx BccBlock) m ())
  requestTxs (txSource, unAcked, stats) txIds = do
    let  reqTxIds :: [TxId]
         reqTxIds = fmap fromGenTxId txIds
    traceWith tr $ ReqTxs (length reqTxIds)
    let UnAcked ua = unAcked
        uaIds = getTxId . getTxBody <$> ua
        (toSend, _retained) = L.partition ((`L.elem` reqTxIds) . getTxId . getTxBody) ua
        missIds = reqTxIds L.\\ uaIds

    traceWith tr $ TxList (length toSend)
    traceWith bmtr $ TraceBenchTxSubServReq reqTxIds
    traceWith bmtr $ TraceBenchTxSubServOuts (getTxId . getTxBody <$> ua)
    unless (L.null missIds) $
      traceWith bmtr $ TraceBenchTxSubServUnav missIds
    pure $ SendMsgReplyTxs (toGenTx <$> toSend)
      (client (txSource, unAcked,
        stats { stsSent =
                stsSent stats + Sent (length toSend)
              , stsUnavailable =
                stsUnavailable stats + Unav (length missIds)}))

  txToIdSize :: tx -> (GenTxId BccBlock, TxSizeInBytes)
  txToIdSize = (Mempool.txId &&& txInBlockSize) . toGenTx

  toGenTx :: tx -> GenTx BccBlock
  toGenTx tx = case (sophieBasedEra @ era , tx) of
    (SophieBasedEraSophie, SophieTx _ tx') -> GenTxSophie (mkSophieTx tx')
    (SophieBasedEraEvie, SophieTx _ tx') -> GenTxEvie (mkSophieTx tx')
    (SophieBasedEraJen, SophieTx _ tx') -> GenTxJen (mkSophieTx tx')
    (SophieBasedEraAurum, SophieTx _ tx') -> GenTxAurum (mkSophieTx tx')

  fromGenTxId :: GenTxId BccBlock -> TxId
  fromGenTxId (Block.GenTxIdSophie (Mempool.SophieTxId i)) = fromSophieTxId i
  fromGenTxId (Block.GenTxIdEvie (Mempool.SophieTxId i)) = fromSophieTxId i
  fromGenTxId (Block.GenTxIdJen    (Mempool.SophieTxId i)) = fromSophieTxId i
  fromGenTxId (Block.GenTxIdAurum  (Mempool.SophieTxId i)) = fromSophieTxId i
  fromGenTxId _ = error "submission.hs: fromGenTxId"

  tokIsBlocking :: TokBlockingStyle a -> Bool
  tokIsBlocking = \case
    TokBlocking    -> True
    TokNonBlocking -> False

  reqIdsTrace :: Ack -> Req -> TokBlockingStyle a -> NodeToNodeSubmissionTrace
  reqIdsTrace ack req = \case
     TokBlocking    -> ReqIdsBlocking ack req
     TokNonBlocking -> ReqIdsPrompt   ack req

  idListTrace :: ToAnnce tx -> TokBlockingStyle a -> NodeToNodeSubmissionTrace
  idListTrace (ToAnnce toAnn) = \case
     TokBlocking    -> IdsListBlocking $ length toAnn
     TokNonBlocking -> IdsListPrompt   $ length toAnn

