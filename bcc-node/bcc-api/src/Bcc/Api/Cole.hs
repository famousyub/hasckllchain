-- | This module provides a library interface that is intended to be
-- the complete API for Cole covering everything, including exposing
-- constructors for the lower level types.
--

module Bcc.Api.Cole
  ( module Bcc.Api,
    AsType(..),

    -- * Cryptographic key interface
    -- $keys
    VerificationKey(..),
    SigningKey(..),
    SomeColeSigningKey(..),

    -- * Hashes
    Hash(..),

    -- * Payment addresses
    -- | Constructing and inspecting Cole payment addresses
    Address(ColeAddress),
    NetworkId(Mainnet, Testnet),

    -- * Building transactions
    -- | Constructing and inspecting transactions
    TxBody(ColeTxBody),
    TxId(TxId),
    TxIn(TxIn),
    TxOut(TxOut),
    TxIx(TxIx),
    Entropic(Entropic),

    -- * Signing transactions
    -- | Creating transaction witnesses one by one, or all in one go.
    Tx(ColeTx),

    -- ** Incremental signing and separate witnesses
    KeyWitness (ColeKeyWitness),
    WitnessNetworkIdOrColeAddress
      ( WitnessNetworkId
      , WitnessColeAddress
      ),

    -- * Errors
    Error(..),
    FileError(..),

    -- ** Low level protocol interaction with a Bcc node
    LocalNodeConnectInfo(LocalNodeConnectInfo),
    ColeMode,
    ConsensusMode
      ( ColeMode
      ),
    LocalNodeClientProtocols(LocalNodeClientProtocols),

    -- *** Chain sync protocol
    ChainSyncClient(..),

    -- *** Local tx submission
    LocalTxSubmissionClient(LocalTxSubmissionClient),

    -- *** Local state query
    LocalStateQueryClient(..),

    -- * Address
    NetworkMagic(..),

    -- * Update Proposal
    ColeUpdateProposal(..),
    ColeProtocolParametersUpdate (..),
    makeColeUpdateProposal,
    toColeLedgerUpdateProposal,
    makeProtocolParametersUpdate,

    -- * Vote
    ColeVote(..),
    makeColeVote,
    toColeLedgertoColeVote,

    -- ** Conversions
    fromColeTxIn,
    toColeEntropic,
    toColeNetworkMagic,
    toColeProtocolMagicId,
    toColeRequiresNetworkMagic,

  ) where

import           Bcc.Api
import           Bcc.Api.Address
import           Bcc.Api.IPC
import           Bcc.Api.KeysCole
import           Bcc.Api.NetworkId
import           Bcc.Api.SpecialCole
import           Bcc.Api.Tx
import           Bcc.Api.TxBody
import           Bcc.Api.Value
