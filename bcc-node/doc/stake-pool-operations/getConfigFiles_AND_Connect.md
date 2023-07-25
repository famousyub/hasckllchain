# Get configuration files

Starting the node and connecting it to the network requires 3 configuration files:

* topology.json
* genesis.json
* config.json

You can download the configuration files from:

 [https://hydra.tbco.io/job/Bcc/bcc-node/bcc-deployment/latest-finished/download/1/index.html](https://hydra.tbco.io/job/Bcc/bcc-node/bcc-deployment/latest-finished/download/1/index.html)


From the CLI you can use

For Bcc testnet

    wget https://hydra.tbco.io/job/Bcc/bcc-node/bcc-deployment/latest-finished/download/1/testnet-config.json
    wget https://hydra.tbco.io/job/Bcc/bcc-node/bcc-deployment/latest-finished/download/1/testnet-cole-genesis.json
    wget https://hydra.tbco.io/job/Bcc/bcc-node/bcc-deployment/latest-finished/download/1/testnet-sophie-genesis.json
    wget https://hydra.tbco.io/job/Bcc/bcc-node/bcc-deployment/latest-finished/download/1/testnet-topology.json

For Mainnet:

    wget https://hydra.tbco.io/job/Bcc/bcc-node/bcc-deployment/latest-finished/download/1/mainnet-config.json
    wget https://hydra.tbco.io/job/Bcc/bcc-node/bcc-deployment/latest-finished/download/1/mainnet-cole-genesis.json
    wget https://hydra.tbco.io/job/Bcc/bcc-node/bcc-deployment/latest-finished/download/1/mainnet-sophie-genesis.json
    wget https://hydra.tbco.io/job/Bcc/bcc-node/bcc-deployment/latest-finished/download/1/mainnet-topology.json

Starting the node uses the command `bcc-node run` and a set of options.

Get the complete list of available options with `bcc-node run --help`

	--topology FILEPATH             The path to a file describing the topology.
  	--database-path FILEPATH        Directory where the state is stored.
  	--socket-path FILEPATH          Path to a bcc-node socket
  	--host-addr IP-ADDRESS          Optionally limit node to one IPv4 address
  	--host-ipv6-addr IP-ADDRESS     Optionally limit node to one IPv6 address
  	--port PORT                     The port number
  	--config NODE-CONFIGURATION     Configuration file for the bcc-node
  	--validate-db                   Validate all on-disk database files
  	--shutdown-ipc FD               Shut down the process when this inherited FD reaches EOF
  	--shutdown-on-slot-synced SLOT  Shut down the process after ChainDB is synced up to the
  	                                specified slot
    -h,--help                       Show this help text

**Note**: If you do not specify `--host-addr` nor `--host-ipv6-addr`, node will use the default IPv4 and IPv6 addresses (depending which addresses are available).  If one specifies one of them only that address will be used, in particular if you only provide an IPv4 address, the node will not connect over IPv6.
To start a passive node:

     bcc-node run \
       --topology path/to/mainnet-topology.json \
       --database-path path/to/db \
       --socket-path path/to/db/node.socket \
       --host-addr x.x.x.x \
       --port 3001 \
       --config path/to/mainnet-config.json

**Replace x.x.x.x with your public IP and indicate the correct paths to the required files.**

Many commands rely on the environment variable BCC_NODE_SOCKET_PATH:

    export BCC_NODE_SOCKET_PATH=path/to/db/node.socket

Check that the node is syncing by fetching the current tip. When syncing `slot` should be increasing.

    bcc-cli query tip --mainnet

    {
        "epoch": 259,
        "hash": "dbf5104ab91a7a0b405353ad31760b52b2703098ec17185bdd7ff1800bb61aca",
        "slot": 26633911,
        "block": 5580350
    }

**Note**`--mainnet` identifies the Bcc mainnet, for testnets use `--testnet-magic 1097911063` instead.
