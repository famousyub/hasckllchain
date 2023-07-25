"""Usage: cluster-test.py [--network-magic=INT] [--state-dir=DIR] [--num-delegs=INT]

Options:
    -n, --network-magic <magic>  network magic [default: 42]
    -d, --num-delegs <int>  number of delegators [default: 1]
    -s, --state-dir <dir>  state directory [default: "./state-cluster-test"]
"""

from docopt import docopt
from bcclib import BccCluster,BccCLIWrapper

arguments = docopt(__doc__)
cluster = BccCluster(int(arguments['--network-magic']), arguments["--state-dir"], int(arguments['--num-delegs']), "sophie")
cluster.cli.refresh_pparams()
