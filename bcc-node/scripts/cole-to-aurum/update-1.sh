#!/usr/bin/env bash

set -e
# set -x

# This script will
# - move funds out of the Cole genesis address, so that we can use them later in Sophie
# - iniate the transition to protocol version 1 (Cole, OBFT)

ROOT=example

pushd ${ROOT}

export BCC_NODE_SOCKET_PATH=node-bft1/node.sock

# move funds out of Cole genesis
bcc-cli submit-tx \
            --testnet-magic 42 \
            --tx tx0.tx
bcc-cli submit-tx \
            --testnet-magic 42 \
            --tx tx1.tx

# submit update proposal
bcc-cli cole submit-update-proposal \
            --testnet-magic 42 \
            --filepath update-proposal
sleep 2

# vote on proposal
bcc-cli cole submit-proposal-vote  \
            --testnet-magic 42 \
            --filepath update-vote.000
bcc-cli cole submit-proposal-vote  \
            --testnet-magic 42 \
            --filepath update-vote.001

popd
