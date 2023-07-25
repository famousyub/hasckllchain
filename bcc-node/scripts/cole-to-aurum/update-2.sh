#!/usr/bin/env bash

set -e
# set -x

# This script will initiate the transition to protocol version 2 (Sophie).

# In order for this to be successful, you need to already be in protocol version
# 1 (which happens one or two epoch boundaries after invoking update-1.sh).
# Also, you need to restart the nodes after running this script in order for the
# update to be endorsed by the nodes.

ROOT=example

pushd ${ROOT}

export BCC_NODE_SOCKET_PATH=node-bft1/node.sock

bcc-cli cole submit-update-proposal \
            --testnet-magic 42 \
            --filepath update-proposal-1

sleep 2
bcc-cli cole submit-proposal-vote  \
            --testnet-magic 42 \
            --filepath update-vote-1.000
bcc-cli cole submit-proposal-vote  \
            --testnet-magic 42 \
            --filepath update-vote-1.001

sed -i configuration.yaml \
    -e 's/LastKnownBlockVersion-Major: 1/LastKnownBlockVersion-Major: 2/' \

popd

echo "Restart the nodes now to endorse the update."
