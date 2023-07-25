{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Parameters fixed in the genesis file: 'GenesisParameters'
--
module Bcc.Api.GenesisParameters (

    -- * Protocol paramaters fixed in the genesis file
    GenesisParameters(..),
    EpochSize(..),

    -- * Internal conversion functions
    fromSophieGenesis,

  ) where

import           Prelude

import           Data.Time (NominalDiffTime, UTCTime)

import           Data.Word (Word64)

import           Bcc.Slotting.Slot (EpochSize (..))

import qualified Bcc.Ledger.BaseTypes as Ledger
import qualified Sophie.Spec.Ledger.Genesis as Sophie

import           Bcc.Api.NetworkId
import           Bcc.Api.ProtocolParameters
import           Bcc.Api.Value


-- ----------------------------------------------------------------------------
-- Genesis parameters
--

data GenesisParameters =
     GenesisParameters {

       -- | The reference time the system started. The time of slot zero.
       -- The time epoch against which all Shardagnostic time slots are measured.
       --
       protocolParamSystemStart :: UTCTime,

       -- | The network identifier for this blockchain instance. This
       -- distinguishes the mainnet from testnets, and different testnets from
       -- each other.
       --
       protocolParamNetworkId :: NetworkId,

       -- | The Shardagnostic Optimum active slot coefficient, aka @f@.
       --
       protocolParamActiveSlotsCoefficient :: Rational,

       -- | The Shardagnostic security paramaters, aka @k@. This is the maximum
       -- number of blocks the node would ever be prepared to roll back by.
       --
       -- Clients of the node following the chain should be prepared to handle
       -- the node switching forks up to this long.
       --
       protocolParamSecurity :: Int,

       -- | The Vested Sentry Protocol vestMultiple 
       -- 
       protocolVestMultiple :: Word64,

       -- | The number of Shardagnostic time slots in an Shardagnostic epoch.
       --
       protocolParamEpochLength :: EpochSize,

       -- | The time duration of a slot.
       --
       protocolParamSlotLength :: NominalDiffTime,

       -- | For Shardagnostic Optimum, the length of a KES period as a number of time
       -- slots. The KES keys get evolved once per KES period.
       --
       protocolParamSlotsPerKESPeriod :: Int,

       -- | The maximum number of times a KES key can be evolved before it is
       -- no longer considered valid. This can be less than the maximum number
       -- of times given the KES key size. For example the mainnet KES key size
       -- would allow 64 evolutions, but the max KES evolutions param is 62.
       --
       protocolParamMaxKESEvolutions ::  Int,

       -- | In the Sophie era, prior to decentralised governance, this is the
       -- number of genesis key delegates that need to agree for an update
       -- proposal to be enacted.
       --
       protocolParamUpdateQuorum ::  Int,

       -- | The maximum supply for Entropic. This determines the initial value
       -- of the reserves.
       --
       protocolParamMaxEntropicSupply :: Entropic,

       -- | The initial values of the updateable 'ProtocolParameters'.
       --
       protocolInitialUpdateableProtocolParameters :: ProtocolParameters
     }


-- ----------------------------------------------------------------------------
-- Conversion functions #TODO conversion fx VestedDelegs verify pertinenance
--

fromSophieGenesis :: Sophie.SophieGenesis era -> GenesisParameters
fromSophieGenesis
    Sophie.SophieGenesis {
      Sophie.sgSystemStart
    , Sophie.sgNetworkMagic
    , Sophie.sgNetworkId
    , Sophie.sgActiveSlotsCoeff
    , Sophie.sgSecurityParam
    , Sophie.sgVestMultiple
    , Sophie.sgEpochLength
    , Sophie.sgSlotsPerKESPeriod
    , Sophie.sgMaxKESEvolutions
    , Sophie.sgSlotLength
    , Sophie.sgUpdateQuorum
    , Sophie.sgMaxEntropicSupply
    , Sophie.sgProtocolParams
    , Sophie.sgGenDelegs    = _  -- unused, might be of interest
    , Sophie.sgVestedDelegs  = _  -- unused, might be if interest
    , Sophie.sgInitialFunds = _  -- unused, not retained by the node
    , Sophie.sgStaking      = _  -- unused, not retained by the node
    } =
    GenesisParameters {
      protocolParamSystemStart            = sgSystemStart
    , protocolParamNetworkId              = fromSophieNetwork sgNetworkId
                                              (NetworkMagic sgNetworkMagic)
    , protocolParamActiveSlotsCoefficient = Ledger.unboundRational
                                              sgActiveSlotsCoeff
    , protocolParamSecurity               = fromIntegral sgSecurityParam
    , protocolVestMultiple                  = sgVestMultiple
    , protocolParamEpochLength            = sgEpochLength
    , protocolParamSlotLength             = sgSlotLength
    , protocolParamSlotsPerKESPeriod      = fromIntegral sgSlotsPerKESPeriod
    , protocolParamMaxKESEvolutions       = fromIntegral sgMaxKESEvolutions
    , protocolParamUpdateQuorum           = fromIntegral sgUpdateQuorum
    , protocolParamMaxEntropicSupply         = Entropic
                                              (fromIntegral sgMaxEntropicSupply)
    , protocolInitialUpdateableProtocolParameters = fromSophiePParams
                                                      sgProtocolParams
    }
