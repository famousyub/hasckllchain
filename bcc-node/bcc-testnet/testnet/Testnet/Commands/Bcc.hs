{-# LANGUAGE TypeApplications #-}

module Testnet.Commands.Bcc
  ( BccOptions(..)
  , cmdBcc
  , runBccOptions
  ) where

import           GHC.Enum
import           Data.Eq
import           Data.Function
import           Data.Int
import           Data.Maybe
import           Data.Semigroup
import           Options.Applicative
import           System.IO (IO)
import           Testnet.Bcc
import           Testnet.Run (runTestnet)
import           Text.Read
import           Text.Show

import qualified Options.Applicative as OA

data BccOptions = BccOptions
  { maybeTestnetMagic :: Maybe Int
  , testnetOptions :: TestnetOptions
  } deriving (Eq, Show)

optsTestnet :: Parser TestnetOptions
optsTestnet = TestnetOptions
  <$> OA.option auto
      (   OA.long "num-bft-nodes"
      <>  OA.help "Number of BFT nodes"
      <>  OA.metavar "COUNT"
      <>  OA.showDefault
      <>  OA.value (numBftNodes defaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "num-pool-nodes"
      <>  OA.help "Number of pool nodes"
      <>  OA.metavar "COUNT"
      <>  OA.showDefault
      <>  OA.value (numPoolNodes defaultTestnetOptions)
      )
  <*> OA.option (OA.eitherReader readEither)
      (   OA.long "era"
      <>  OA.help ("Era to upgrade to.  " <> show @[Era] [minBound .. maxBound])
      <>  OA.metavar "ERA"
      <>  OA.showDefault
      <>  OA.value (era defaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "epoch-length"
      <>  OA.help "Epoch length"
      <>  OA.metavar "MILLISECONDS"
      <>  OA.showDefault
      <>  OA.value (epochLength defaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "slot-length"
      <>  OA.help "Slot length"
      <>  OA.metavar "SECONDS"
      <>  OA.showDefault
      <>  OA.value (slotLength defaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "active-slots-coeff"
      <>  OA.help "Active slots co-efficient"
      <>  OA.metavar "DOUBLE"
      <>  OA.showDefault
      <>  OA.value (activeSlotsCoeff defaultTestnetOptions)
      )

optsBcc :: Parser BccOptions
optsBcc = BccOptions
  <$> optional
      ( OA.option auto
        (   long "testnet-magic"
        <>  help "Testnet magic"
        <>  metavar "INT"
        )
      )
  <*> optsTestnet

runBccOptions :: BccOptions -> IO ()
runBccOptions options = runTestnet (maybeTestnetMagic options) $
  Testnet.Bcc.testnet (testnetOptions options)

cmdBcc :: Mod CommandFields (IO ())
cmdBcc = command "bcc"  $ flip info idm $ runBccOptions <$> optsBcc
