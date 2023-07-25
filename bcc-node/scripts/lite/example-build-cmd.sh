#!/usr/bin/env bash

# Unoffiical bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -e
set -o pipefail

export WORK="${WORK:-example/work}"
export BASE="${BASE:-.}"
export BCC_CLI="${BCC_CLI:-bcc-cli}"
export BCC_NODE_SOCKET_PATH="${BCC_NODE_SOCKET_PATH:-example/node-bft1/node.sock}"
export TESTNET_MAGIC="${TESTNET_MAGIC:-42}"
export UTXO_VKEY="${UTXO_VKEY:-example/sophie/utxo-keys/utxo1.vkey}"
export UTXO_SKEY="${UTXO_SKEY:-example/sophie/utxo-keys/utxo1.skey}"
export RESULT_FILE_TARGET="${RESULT_FILE:-$WORK/target.out}"
export RESULT_FILE_CHANGE="${RESULT_FILE:-$WORK/change.out}"

echo "Socket path: $BCC_NODE_SOCKET_PATH"
echo "Socket path: $(pwd)"

ls -al "$BCC_NODE_SOCKET_PATH"


mkdir -p "$WORK"

utxoaddr=$($BCC_CLI address build --testnet-magic "$TESTNET_MAGIC" --payment-verification-key-file "$UTXO_VKEY")

$BCC_CLI query utxo --address "$utxoaddr" --bcc-mode --testnet-magic "$TESTNET_MAGIC" --out-file $WORK/utxo-1.json
cat $WORK/utxo-1.json

txin=$(jq -r 'keys[]' $WORK/utxo-1.json)
entropicattxin=$(jq -r ".[\"$txin\"].value.entropic" $WORK/utxo-1.json)
entropicattxindiv2=$(expr $entropicattxin / 2)

changeaddr=addr_test1qpmxr8d8jcl25kyz2tz9a9sxv7jxglhddyf475045y8j3zxjcg9vquzkljyfn3rasfwwlkwu7hhm59gzxmsyxf3w9dps8832xh
targetaddr=addr_test1vpqgspvmh6m2m5pwangvdg499srfzre2dd96qq57nlnw6yctpasy4

$BCC_CLI transaction build \
  --aurum-era \
  --bcc-mode \
  --testnet-magic "$TESTNET_MAGIC" \
  --change-address "$changeaddr" \
  --tx-in $txin \
  --tx-out "$targetaddr+10000000" \
  --out-file $WORK/build.body

$BCC_CLI transaction sign \
  --tx-body-file $WORK/build.body \
  --testnet-magic "$TESTNET_MAGIC" \
  --signing-key-file $UTXO_SKEY \
  --out-file $WORK/build.tx

# SUBMIT
$BCC_CLI transaction submit --tx-file $WORK/build.tx --testnet-magic "$TESTNET_MAGIC"
echo "Pausing for 5 seconds..."
sleep 5

echo "Querying UTxO at change address and target address."
echo ""
echo "Target address"
echo ""
$BCC_CLI query utxo --address "$targetaddr"  --testnet-magic "$TESTNET_MAGIC" \
  | tee "$RESULT_FILE_TARGET"
echo "Change address"
echo ""
$BCC_CLI query utxo --address "$changeaddr"  --testnet-magic "$TESTNET_MAGIC" \
  | tee "$RESULT_FILE_CHANGE"
