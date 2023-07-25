{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Nanocoin.Block (
  Block(..),
  BlockHeader(..),
  Blockchain,
  genesisBlock,

  -- ** Block Hashing
  hashBlock,
  hashBlockHeader,

  -- ** Validation
  InvalidBlock(..),
  validateBlock,
  applyBlock,
  validateAndApplyBlock,

  -- ** Consensus
  proofOfWork,
  checkProofOfWork,
  mineBlock,
  getLatestBlock,


) where

import Protolude

import Control.Monad (fail)

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Int (Int64)
import Data.Time.Clock.POSIX (getPOSIXTime)

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Serialize as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Crypto.Hash.MerkleTree

import Address
import Nanocoin.Ledger
import Nanocoin.Transaction (Transaction)

import qualified Hash
import qualified Key
import qualified Nanocoin.Transaction as T
import qualified Nanocoin.Ledger as Ledger

type Index      = Int
type Timestamp  = Integer
type Blockchain = [Block]

data BlockHeader = BlockHeader
  { origin       :: Key.PublicKey -- ^ Address of Block miner
  , previousHash :: ByteString    -- ^ Previous block hash
  , merkleRoot   :: ByteString    -- ^ Merkle Root of transactions
  , nonce        :: Int64         -- ^ Nonce for Proof-of-Work
  } deriving (Eq, Show)

data Block = Block
  { index        :: Index         -- ^ Block height
  , header       :: BlockHeader   -- ^ Block header
  , transactions :: [Transaction] -- ^ List of Transactions
  , signature    :: ByteString    -- ^ Block signature
  } deriving (Eq, Show, Generic, S.Serialize)

genesisBlock :: Key.KeyPair -> IO Block
genesisBlock (pubKey, privKey) = do
    signature' <- liftIO $
      Key.sign privKey (S.encode genesisBlockHeader)
    return Block
      { index     = 0
      , header    = genesisBlockHeader
      , transactions = []
      , signature = S.encode signature'
      }
  where
    genesisBlockHeader = BlockHeader
      { origin       = pubKey
      , previousHash = "0"
      , merkleRoot   = getMerkleRoot emptyHash
      , nonce        = 0
      }

-- | Get the latest block from the chain
getLatestBlock :: Blockchain -> Maybe Block
getLatestBlock = head

-------------------------------------------------------------------------------
-- Block Hashing
-------------------------------------------------------------------------------

-- | Hash a block header, to be used as the prevHash field in Block
hashBlockHeader :: BlockHeader -> ByteString
hashBlockHeader BlockHeader{..} =
  Hash.getHash $ Hash.sha256 $
    BS.concat [ rawAddress (deriveAddress origin)
              , previousHash
              , merkleRoot
              , B8.pack (show nonce)
              ]

-- | Generate a block hash
hashBlock :: Block -> ByteString
hashBlock = hashBlockHeader . header

-------------------------------------------------------------------------------
-- Validation
-------------------------------------------------------------------------------

data InvalidBlock
  = InvalidBlockSignature Text
  | InvalidBlockIndex Int
  | InvalidBlockHash
  | InvalidBlockMerkleRoot Text
  | InvalidBlockNumTxs
  | InvalidBlockTx T.InvalidTx
  | InvalidPrevBlockHash
  | InvalidFirstBlock
  | InvalidBlockTxs [T.InvalidTx]
  deriving (Show, Eq)

-- | Verify a block's ECDSA signature
verifyBlockSignature
  :: Block
  -> Either InvalidBlock ()
verifyBlockSignature b = do
  let originKey = origin $ header b
  case S.decode (signature b) of
    Left err -> Left $ InvalidBlockSignature (toS err)
    Right sig -> do
      let validSig = Key.verify originKey sig (S.encode $ header b)
      unless validSig $
        Left $ InvalidBlockSignature "Could not verify block signature."

-- | Validate a block before accepting a block as new block in chain
validateBlock
  :: Ledger
  -> Block
  -> Block
  -> Either InvalidBlock ()
validateBlock ledger prevBlock block
  | index block /= index prevBlock + 1 = Left $ InvalidBlockIndex (index block)
  | hashBlock prevBlock /= previousHash (header block) = Left InvalidPrevBlockHash
  | not (checkProofOfWork block) = Left InvalidBlockHash
  | null (transactions block) = Left InvalidBlockNumTxs
  | mRoot /= mRoot' = Left $ InvalidBlockMerkleRoot $ toS mRoot'
  | otherwise = do
      -- Verify signature of block
      verifyBlockSignature block
      -- Validate all transactions w/ respect to world state
      first InvalidBlockTx $
        T.validateTransactions ledger blockTxs
  where
    blockTxs = transactions block
    txHashes = map T.hashTransaction blockTxs
    mRoot  = merkleRoot $ header block      -- given root
    mRoot' = mtHash $ mkMerkleTree txHashes -- constr root

validateAndApplyBlock
  :: Ledger
  -> Block
  -> Block
  -> Either InvalidBlock (Ledger, [T.InvalidTx])
validateAndApplyBlock ledger prevBlock block = do
  validateBlock ledger prevBlock block
  Right $ applyBlock ledger block

-- | Apply block transactions to world state
applyBlock
  :: Ledger
  -> Block
  -> (Ledger, [T.InvalidTx])
applyBlock ledger = T.applyTransactions ledger . transactions

-------------------------------------------------------------------------------
-- Consensus
-------------------------------------------------------------------------------

-- | Generates (mines) a new block using the `proofOfWork` function
mineBlock
  :: MonadIO m
  => Block          -- ^ Previous Block in chain
  -> Key.PrivateKey -- ^ Miner's private key
  -> [Transaction]  -- ^ List of transactions
  -> m Block
mineBlock prevBlock privKey txs = do
    signature' <- liftIO $ -- Sign the serialized block header
      Key.sign privKey (S.encode blockHeader)
    return Block
      { index        = index'
      , header       = blockHeader
      , transactions = txs
      , signature    = S.encode signature'
      }
  where
    txHashes = map T.hashTransaction txs

    initBlockHeader = BlockHeader
      { origin       = origin'
      , previousHash = prevHash
      , merkleRoot   = mtHash (mkMerkleTree txHashes)
      , nonce        = 0
      }

    index'      = index prevBlock + 1
    prevHash    = hashBlock prevBlock
    origin'     = Key.toPublic privKey
    blockHeader = proofOfWork index' initBlockHeader

    now :: IO Integer
    now = round `fmap` getPOSIXTime

proofOfWork
  :: Int         -- ^ Difficulty measured by block index
  -> BlockHeader -- ^ Header to hash with nonce parameter
  -> BlockHeader
proofOfWork idx blockHeader = blockHeader { nonce = calcNonce 0 }
  where
    difficulty = calcDifficulty idx
    prefix = toS $ replicate difficulty '0'

    calcNonce n
      | prefix' == prefix = n
      | otherwise = calcNonce $ n + 1
      where
        headerHash = hashBlockHeader (blockHeader { nonce = n })
        prefix' = BS.take difficulty headerHash

-- | difficulty(block) = round(ln(index(block)))
calcDifficulty :: Int -> Int
calcDifficulty = round . logBase (2 :: Float) . fromIntegral

checkProofOfWork :: Block -> Bool
checkProofOfWork block =
    BS.isPrefixOf prefix $ hashBlock block
  where
    difficulty = calcDifficulty $ index block
    prefix = toS $ replicate difficulty '0'

-------------------------------------------------------------------------------
-- Serialization
-------------------------------------------------------------------------------

instance S.Serialize BlockHeader where
  put (BlockHeader opk ph mr n) = do
    Key.putPublicKey opk
    S.put ph
    S.put mr
    S.put n
  get = BlockHeader
    <$> Key.getPublicKey
    <*> S.get
    <*> S.get
    <*> S.get

instance ToJSON BlockHeader where
  toJSON (BlockHeader opk ph mr n) =
    let (x,y) = Key.extractPoint opk in
    object [ "origin"       .= object
               [ "x" .= (x :: Integer)
               , "y" .= (y :: Integer)
               ]
           , "previousHash" .= Hash.encode64 ph
           , "merkleRoot"   .= Hash.encode64 mr
           , "nonce"        .= toJSON n
           ]

instance ToJSON Block where
  toJSON (Block i bh txs s) =
    object [ "index"        .= i
           , "header"       .= toJSON bh
           , "transactions" .= toJSON txs
           , "signature"    .= Hash.encode64 s
           ]
