module Testnet.Commands.Sophie
  ( SophieOptions(..)
  , cmdSophie
  , runSophieOptions
  ) where


import           Data.Eq
import           Data.Function
import           Data.Int
import           Data.Maybe
import           Data.Semigroup
import           Options.Applicative
import           System.IO (IO)
import           Testnet.Run (runTestnet)
import           Testnet.Sophie
import           Text.Show

import qualified Options.Applicative as OA

data SophieOptions = SophieOptions
  { maybeTestnetMagic :: Maybe Int
  , testnetOptions :: TestnetOptions
  } deriving (Eq, Show)

optsTestnet :: Parser TestnetOptions
optsTestnet = TestnetOptions
  <$> OA.option auto
      (   OA.long "num-optimum-nodes"
      <>  OA.help "Number of OPTIMUM nodes"
      <>  OA.metavar "COUNT"
      <>  OA.showDefault
      <>  OA.value (numOptimumNodes defaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "num-pool-nodes"
      <>  OA.help "Number of pool nodes"
      <>  OA.metavar "COUNT"
      <>  OA.showDefault
      <>  OA.value (numPoolNodes defaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "active-slots-coeff"
      <>  OA.help "Active slots co-efficient"
      <>  OA.metavar "DOUBLE"
      <>  OA.showDefault
      <>  OA.value (activeSlotsCoeff defaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "security-param"
      <>  OA.help "Security param"
      <>  OA.metavar "INT"
      <>  OA.showDefault
      <>  OA.value (securityParam defaultTestnetOptions)
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
      <>  OA.metavar "MILLISECONDS"
      <>  OA.showDefault
      <>  OA.value (slotLength defaultTestnetOptions)
      )
  <*> OA.option auto
      (   OA.long "max-entropic-supply"
      <>  OA.help "Max entropic supply"
      <>  OA.metavar "INTEGER"
      <>  OA.showDefault
      <>  OA.value (maxEntropicSupply defaultTestnetOptions)
      )

optsSophie :: Parser SophieOptions
optsSophie = SophieOptions
  <$> optional
      ( OA.option auto
        (   long "testnet-magic"
        <>  help "Testnet magic"
        <>  metavar "INT"
        )
      )
  <*> optsTestnet

runSophieOptions :: SophieOptions -> IO ()
runSophieOptions options = runTestnet (maybeTestnetMagic options) $
  Testnet.Sophie.testnet (testnetOptions options)

cmdSophie :: Mod CommandFields (IO ())
cmdSophie = command "sophie"  $ flip info idm $ runSophieOptions <$> optsSophie
