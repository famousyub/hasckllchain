import           Bcc.Prelude

import           Hedgehog.Main (defaultMain)

import qualified Test.Golden.Cole.SigningKeys
import qualified Test.Golden.Cole.Tx
import qualified Test.Golden.Cole.UpdateProposal
import qualified Test.Golden.Cole.Vote
import qualified Test.Golden.Sophie
import qualified Test.Golden.TxView

main :: IO ()
main =
  defaultMain
    [ Test.Golden.Cole.SigningKeys.tests
    , Test.Golden.Cole.Tx.txTests
    , Test.Golden.Cole.UpdateProposal.updateProposalTest
    , Test.Golden.Cole.Vote.voteTests
    , Test.Golden.Sophie.keyTests
    , Test.Golden.Sophie.certificateTests
    , Test.Golden.Sophie.keyConversionTests
    , Test.Golden.Sophie.metadataTests
    , Test.Golden.Sophie.multiSigTests
    , Test.Golden.Sophie.txTests
    , Test.Golden.TxView.txViewTests
    ]
