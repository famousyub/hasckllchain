module Bcc.Benchmarking.Script.Example
where

import           Prelude
import           Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Word
import           Data.Dependent.Sum ((==>) )

import           Bcc.Api (AnyBccEra(..), BccEra(..), Quantity(..), SlotNo(..), quantityToEntropic )
import           Bcc.Node.Types
import           Shardagnostic.Network.NodeToClient (withIOManager)

import           Bcc.Benchmarking.Types
import           Bcc.Benchmarking.Script.Action
import           Bcc.Benchmarking.Script.Aeson
import           Bcc.Benchmarking.Script.Env
import           Bcc.Benchmarking.Script.Store
import           Bcc.Benchmarking.Script.Setters

runTestScript :: IO (Either Error (), Env, ())
runTestScript = withIOManager $ runActionM (forM_ testScript action)

printJSON :: IO ()
printJSON = BSL.putStrLn $ prettyPrint testScript

txConfig :: [Action]
txConfig = map Set [
    TNumberOfInputsPerTx  ==> NumberOfInputsPerTx 1
  , TNumberOfOutputsPerTx ==> NumberOfOutputsPerTx 1
  , TNumberOfTxs          ==> NumberOfTxs 500
  , TTxAdditionalSize     ==> TxAdditionalSize 0
  , TFee                  ==> quantityToEntropic (Quantity 0)
  , TTTL                  ==> SlotNo 1000000
  ]

testScript :: [Action]
testScript =
  txConfig
  ++
  [
    StartProtocol "configuration/configuration-generator.yaml"
  , Set $ TEra ==> AnyBccEra JenEra
  , Set $ TLocalSocket ==> "logs/sockets/1"
  , ReadSigningKey passPartout "configuration/genesis-sophie/utxo-keys/utxo1.skey"
  , SecureGenesisFund genFund passPartout passPartout
  , Delay 10
  , SplitFund outputFunds passPartout genFund
  , Delay 10
  , SplitFundToList fundList passPartout f1
  , PrepareTxList txList passPartout fundList
  , Set $ TTargets ==> makeTargets [ 3000, 3001, 3002]
  , AsyncBenchmark threadName txList (TPSRate 10)
  , WaitForEra $ AnyBccEra ColeEra
  , CancelBenchmark threadName
  , ImportGenesisFund passPartout passPartout
  , CreateChange (quantityToEntropic 10000) 1000
  , RunBenchmark (ThreadName "walletThread") (NumberOfTxs 1000) (TPSRate 10)
  , Reserved []
  ]
 where
  passPartout = KeyName "pass-partout"
  genFund = FundName "genFund"
  outputFunds = map FundName ["fund1", "fund2", "fund3", "fund4"]
  f1= head outputFunds
  fundList = FundListName "fundList"
  txList = TxListName "txlist"
  threadName = ThreadName "thread1"
  makeTargets = NonEmpty.fromList . map (\p -> makeAddr ("127.0.0.1", p))

  makeAddr :: (String, Word16) -> NodeIPv4Address
  makeAddr (a,b) = NodeAddress (NodeHostIPv4Address $ read a) (fromIntegral b)
