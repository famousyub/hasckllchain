#!/usr/bin/env bash

set -e
# set -x

# This script will initiate the transition to protocol version 4 (Jen).

# You need to provide the current epoch as a positional argument (the Sophie
# update system requires this to be includded in the update proposal).


# In order for this to be successful, you need to already be in protocol version
# 3 (which happens one or two epoch boundaries after invoking update-3.sh).
# Also, you need to restart the nodes after running this script in order for the
# update to be endorsed by the nodes.

if [ ! "$1" ]; then echo "update-4.sh: expects an <N> epoch argument"; exit; fi

EPOCH=$1
VERSION=4

ROOT=example
COINS_IN_INPUT=1000000000

pushd ${ROOT}

export BCC_NODE_SOCKET_PATH=node-bft1/node.sock

TXID2=$(bcc-cli transaction txid --tx-file tx2.tx)


# Create the update proposal to change the protocol version to 4

bcc-cli governance create-update-proposal \
            --out-file update-proposal-jen \
            --epoch ${EPOCH} \
            --genesis-verification-key-file sophie/genesis-keys/genesis1.vkey \
            --genesis-verification-key-file sophie/genesis-keys/genesis2.vkey \
            --protocol-major-version ${VERSION} \
            --protocol-sentry-version 0

# Create a transaction body containing the update proposal.

bcc-cli transaction build-raw \
            --evie-era \
            --fee 0 \
            --tx-in $TXID2#0\
            --tx-in $TXID2#1\
            --tx-out $(cat addresses/user1.addr)+$((${COINS_IN_INPUT} / 2)) \
            --tx-out $(cat addresses/user1.addr)+$((${COINS_IN_INPUT} / 2)) \
            --update-proposal-file update-proposal-jen \
            --out-file tx3.txbody

# Sign the transaction body with the two genesis delegate keys,
# and the the uxto spending key.

bcc-cli transaction sign \
            --signing-key-file addresses/user1.skey \
            --signing-key-file sophie/delegate-keys/delegate1.skey \
            --signing-key-file sophie/delegate-keys/delegate2.skey \
            --testnet-magic 42 \
            --tx-body-file  tx3.txbody \
            --out-file      tx3.tx


bcc-cli transaction submit --tx-file tx3.tx --testnet-magic 42

sed -i configuration.yaml \
    -e 's/LastKnownBlockVersion-Major: 3/LastKnownBlockVersion-Major: 4/' \


popd

echo "Restart the nodes now to endorse the update."
