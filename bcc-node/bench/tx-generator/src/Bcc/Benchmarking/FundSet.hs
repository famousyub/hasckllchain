{-# OPTIONS_GHC -Wwarn #-}
{-# Language DataKinds #-}
{-# Language FlexibleInstances #-}
{-# Language GADTs #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language MultiParamTypeClasses #-}
{-# Language RankNTypes #-}

module Bcc.Benchmarking.FundSet
where
import           Prelude

import           Data.IxSet.Typed as IxSet
import           Data.Proxy

import           Control.Applicative ((<|>))
import           Bcc.Api as Api

-- Outputs that are available for spending.
-- When building a new TX they provide the TxIn parts.

data FundInEra era = FundInEra {
    _fundTxIn :: !TxIn
  , _fundVal  :: !(TxOutValue era)
  , _fundSigningKey :: !(SigningKey PaymentKey)
  , _fundVariant :: !Variant
  , _fundValidity :: !Validity
  } deriving (Show)

data Variant
  = PlainOldFund
  | ZerepochScriptFund
-- | DedicatedCollateral
  deriving  (Show, Eq, Ord)

data Validity
  = Confirmed
  | InFlight !Target !SeqNumber
  deriving  (Show, Eq, Ord)

newtype Target = Target String
  deriving  (Show, Eq, Ord)

newtype SeqNumber = SeqNumber Int
  deriving  (Show, Eq, Ord, Enum)

newtype Fund = Fund {unFund :: InAnyBccEra FundInEra}

getFundVariant :: Fund -> Variant
getFundVariant (Fund (InAnyBccEra _ a)) = _fundVariant a

getFundTxIn :: Fund -> TxIn
getFundTxIn (Fund (InAnyBccEra _ a)) = _fundTxIn a

getFundKey :: Fund -> SigningKey PaymentKey
getFundKey (Fund (InAnyBccEra _ a)) = _fundSigningKey a

getFundValidity :: Fund -> Validity
getFundValidity (Fund (InAnyBccEra _ a)) = _fundValidity a

getFundEntropic :: Fund -> Entropic
getFundEntropic (Fund (InAnyBccEra _ a)) = case _fundVal a of
  TxOutBccOnly _era l -> l
  TxOutValue _era v -> selectEntropic v

data IsConfirmed = IsConfirmed | IsNotConfirmed
  deriving  (Show, Eq, Ord)

isConfirmed :: Fund -> IsConfirmed
isConfirmed f = case getFundValidity f of
  Confirmed -> IsConfirmed
  InFlight _ _ -> IsNotConfirmed

instance Show Fund where
  show (Fund (InAnyBccEra _ f)) = show f

-- TxIn/fundTxOut is the primary key.
-- There must be no two entries for the same TxIn !.

instance Eq Fund where
  (==) a b = getFundTxIn a == getFundTxIn b

instance Ord Fund where
  compare a b = compare (getFundTxIn a) (getFundTxIn b)

type FundIndices = '[ TxIn, IsConfirmed, Target, SeqNumber, Entropic, Variant ]
type FundSet = IxSet FundIndices Fund

instance Indexable FundIndices Fund where
  indices = ixList
    (ixFun $ \f -> [ getFundTxIn f ])
    (ixFun $ \f -> [ isConfirmed f ])
    (ixFun $ \f -> case getFundValidity f of
      Confirmed -> []
      InFlight t _ -> [t]
    )
    (ixFun $ \f -> case getFundValidity f of
      Confirmed -> [SeqNumber (-1) ] -- Confirmed Txs get SeqNumber -1
      InFlight _ n -> [ n ]
    )
    (ixFun $ \f -> [ getFundEntropic f ])
    (ixFun $ \f -> [ getFundVariant f ])

emptyFunds :: FundSet
emptyFunds = IxSet.empty

insertFund :: FundSet -> Fund -> FundSet
insertFund s f = updateIx (getFundTxIn f) f s

deleteFund :: FundSet -> Fund -> FundSet
deleteFund s f = deleteIx (getFundTxIn f) s

liftAnyEra :: ( forall era. IsBccEra era => f1 era -> f2 era ) -> InAnyBccEra f1 -> InAnyBccEra f2
liftAnyEra f x = case x of
  InAnyBccEra ColeEra a   ->   InAnyBccEra ColeEra $ f a
  InAnyBccEra SophieEra a ->   InAnyBccEra SophieEra $ f a
  InAnyBccEra EvieEra a ->   InAnyBccEra EvieEra $ f a
  InAnyBccEra JenEra a    ->   InAnyBccEra JenEra $ f a
  InAnyBccEra AurumEra a  ->   InAnyBccEra AurumEra $ f a

type FundSelector = FundSet -> Either String [Fund]
type FundSource = IO (Either String [Fund])
type FundToStore = [Fund] -> IO ()

-- Select a number of confirmed Fund that where send to a specific Target node.
-- TODO: dont ignore target.
selectCountTarget :: Int -> Target -> FundSet -> Either String [Fund]
selectCountTarget count _target fs =
  if length funds == count
    then Right funds
    else Left "could not find enough input coins"
  where
    -- Just take confirmed coins.
    -- TODO: extend this to unconfimed coins to the same target node
    funds = take count $ toAscList ( Proxy :: Proxy Entropic) (fs @=PlainOldFund @= IsConfirmed)

-- Select Funds to cover a minimum value.
-- TODO:
-- This fails unless there is a single fund with the required value
-- Extend this to really return a list of funds.
selectMinValue :: Entropic -> FundSet -> Either String [Fund]
selectMinValue minValue fs = case coins of
    [] -> Left $ "findSufficientCoin: no single coin with min value >= " ++ show minValue
    (c:_) -> Right [c]
    where coins = toAscList ( Proxy :: Proxy Entropic) (fs @=PlainOldFund @= IsConfirmed @>= minValue)

selectZerepochFund :: FundSet -> Either String [Fund]
selectZerepochFund fs = case coins of
    [] -> Left "no Zerepoch fund found"
    (c:_) -> Right [c]
    where coins = toAscList ( Proxy :: Proxy Entropic) (fs @=ZerepochScriptFund @= IsConfirmed )

selectCollateral :: FundSet -> Either String [Fund]
selectCollateral fs = case coins of
  [] -> Left "no matching none-Zerepoch fund found"
  (c:_) -> Right [c]
 where
  coins = toAscList ( Proxy :: Proxy Entropic) (fs @=PlainOldFund @= IsConfirmed @= (1492000000 :: Entropic) )

data AllowRecycle
  = UseConfirmedOnly
  | ReuseSameTarget
-- ReuseAny can cause none-deterministic runtime errors !
-- The problematic case is the reuse of an UTxO/Tx that is not yet confirmed
-- and still waits in the mempool of an other target-node.
  | ReuseAny
  | ConfirmedBeforeReuse -- usefull for testing
  deriving (Eq, Ord, Enum, Show)

-- There are many possible heuristics to implement the selectInputs function.
-- TODO: Check that the complexity of selectInputs is good enough.
selectInputs ::
     AllowRecycle
  -> Int
  -> Entropic
  -> Variant
  -> Target
  -> FundSet
  -> Either String [Fund]
selectInputs allowRecycle count minTotalValue variant targetNode fs
  = case allowRecycle of
    UseConfirmedOnly     -> selectConfirmed
    ReuseSameTarget      -> reuseSameTarget <|> selectConfirmed
    ReuseAny             -> reuseSameTarget <|> selectConfirmed <|> reuseAnyCoin
    ConfirmedBeforeReuse -> selectConfirmed <|> reuseSameTarget
  where
  selectConfirmed = selectConfirmedSmallValue <|> selectConfirmedBigValue

  isSufficiantCoins coins = length coins == count && sum (map getFundEntropic coins) >= minTotalValue

  checkCoins :: String -> [Fund] -> Either String [Fund]
  checkCoins err coins
    = if isSufficiantCoins coins then Right coins else Left err

  -- Share intermediate results for variantIxSet confirmedIxSet and targetIxSet
  -- TODO: it unclear if this helps on the complexity or it it is even harmful.
  variantIxSet   = fs @= variant
  confirmedIxSet = variantIxSet @= IsConfirmed
  targetIxSet    = variantIxSet @= targetNode

  confirmedBigValueList = toDescList (Proxy :: Proxy Entropic) confirmedIxSet
  sameTargetList = toAscList (Proxy :: Proxy SeqNumber) targetIxSet

  selectConfirmedSmallValue
    = checkCoins
        "selectConfirmedSmall: not enought coins available"
        (take count $ toAscList (Proxy :: Proxy Entropic) confirmedIxSet)

  selectConfirmedBigValue
    = checkCoins
        "selectConfirmedSmall: not enought coins available"
        (take count confirmedBigValueList)

  -- reuseSameTargetStrict is problematic: It fails if the coins in the queues are too small. But it will never consume the small coins.
  -- therefore: (reuseSameTargetStrict <|> reuseSameTargetWithBackup)
  reuseSameTargetStrict
    = checkCoins
        "reuseSameTargetStrict: not enought coins available"
        (take count sameTargetList)

  -- reuseSameTargetWithBackup can collect some dust.
  -- reuseSameTargetWithBackup works fine if there is at least one sufficiant confirmed UTxO available.
  reuseSameTargetWithBackup = checkCoins "reuseSameTargetWithBackup: not enought coins available" (backupCoin ++ targetCoins)
    where
      -- targetCoins and backupCoins must be disjoint.
      -- This is case because IsConfirmed \= InFlight target.
      backupCoin = take 1 $ toAscList (Proxy :: Proxy Entropic) (confirmedIxSet @> minTotalValue)
      targetCoins = take (count - 1) sameTargetList

  reuseSameTarget = reuseSameTargetStrict <|> reuseSameTargetWithBackup

  -- reuseAnyCoin is the last resort !
  reuseAnyCoin
    = checkCoins
        "reuseAnyTarget: not enought coins available"
        (take count $ confirmedBigValueList ++ inFlightCoins)
    where
      -- inFlightCoins and confirmedCoins are disjoint
      inFlightCoins = toAscList (Proxy :: Proxy SeqNumber) (variantIxSet @=IsNotConfirmed)

-- Todo: check sufficant funds and minimumValuePerUtxo
inputsToOutputsWithFee :: Entropic -> Int -> [Entropic] -> [Entropic]
inputsToOutputsWithFee fee count inputs = map (quantityToEntropic . Quantity) outputs
  where
    (Quantity totalAvailable) = entropicToQuantity $ sum inputs - fee
    (out, rest) = divMod totalAvailable (fromIntegral count)
    outputs = (out + rest) : replicate (count-1) out
