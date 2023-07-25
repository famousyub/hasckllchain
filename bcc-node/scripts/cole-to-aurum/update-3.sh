#!/usr/bin/env bash

set -e
# set -x

# This script will initiate the transition to protocol version 3 (Evie).

# It will also set up a working stake pool (including delegating to it).

# You need to provide the current epoch as a positional argument (the Sophie
# update system requires this to be includded in the update proposal).


# In order for this to be successful, you need to already be in protocol version
# 2 (which happens one or two epoch boundaries after invoking update-2.sh).
# Also, you need to restart the nodes after running this script in order for the
# update to be endorsed by the nodes.

if [ ! "$1" ]; then echo "update-3.sh: expects an <N> epoch argument"; exit; fi

EPOCH=$1
VERSION=3

ROOT=example
COINS_IN_INPUT=1000000000

pushd ${ROOT}

export BCC_NODE_SOCKET_PATH=node-bft1/node.sock

TXID0=$(bcc-cli cole transaction txid --tx tx0.tx)
TXID1=$(bcc-cli cole transaction txid --tx tx1.tx)

bcc-cli governance create-update-proposal \
            --out-file update-proposal-evie \
            --epoch ${EPOCH} \
            --genesis-verification-key-file sophie/genesis-keys/genesis1.vkey \
            --genesis-verification-key-file sophie/genesis-keys/genesis2.vkey \
            --protocol-major-version ${VERSION} \
            --protocol-sentry-version 0

bcc-cli key convert-cole-key \
            --cole-signing-key-file cole/payment-keys.000.key \
            --out-file cole/payment-keys.000-converted.key \
            --cole-payment-key-type

bcc-cli key convert-cole-key \
            --cole-signing-key-file cole/payment-keys.001.key \
            --out-file cole/payment-keys.001-converted.key \
            --cole-payment-key-type

# Now we'll construct one whopper of a transaction that does everything
# just to show off that we can, and to make the script shorter

# We'll transfer all the funds to the user1, which delegates to pool1
# We'll register certs to:
#  1. register the pool-owner1 stake address
#  2. register the stake pool 1
#  3. register the user1 stake address
#  4. delegate from the user1 stake address to the stake pool
# We'll include the update proposal

bcc-cli transaction build-raw \
            --sophie-era \
            --invalid-hereafter 100000 \
            --fee 0 \
            --tx-in ${TXID0}#0\
            --tx-in ${TXID1}#0\
            --tx-out $(cat addresses/user1.addr)+$((${COINS_IN_INPUT} / 2)) \
            --tx-out $(cat addresses/user1.addr)+$((${COINS_IN_INPUT} / 2)) \
            --certificate-file addresses/pool-owner1-stake.reg.cert \
            --certificate-file node-pool1/registration.cert \
            --certificate-file addresses/user1-stake.reg.cert \
            --certificate-file addresses/user1-stake.deleg.cert \
            --update-proposal-file update-proposal-evie \
            --out-file tx2.txbody

# So we'll need to sign this with a bunch of keys:
# 1. the initial utxo spending key, for the funds
# 2. the user1 stake address key, due to the delegatation cert
# 3. the pool1 owner key, due to the pool registration cert
# 4. the pool1 operator key, due to the pool registration cert
# 5. the genesis delegate keys, due to the update proposal

bcc-cli transaction sign \
            --signing-key-file sophie/utxo-keys/utxo1.skey \
            --signing-key-file addresses/user1-stake.skey \
            --signing-key-file node-pool1/owner.skey \
            --signing-key-file node-pool1/sophie/operator.skey \
            --signing-key-file sophie/genesis-keys/genesis1.skey \
            --signing-key-file sophie/genesis-keys/genesis2.skey \
            --signing-key-file sophie/delegate-keys/delegate1.skey \
            --signing-key-file sophie/delegate-keys/delegate2.skey \
            --signing-key-file cole/payment-keys.000-converted.key \
            --signing-key-file cole/payment-keys.001-converted.key \
            --testnet-magic 42 \
            --tx-body-file tx2.txbody \
            --out-file tx2.tx


bcc-cli transaction submit --tx-file tx2.tx --testnet-magic 42

sed -i configuration.yaml \
    -e 's/LastKnownBlockVersion-Major: 2/LastKnownBlockVersion-Major: 3/' \


popd

echo "Restart the nodes now to endorse the update."
