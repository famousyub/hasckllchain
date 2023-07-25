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
export RESULT_FILE="${RESULT_FILE:-$WORK/result.out}"

echo "Socket path: $BCC_NODE_SOCKET_PATH"
echo "Socket path: $(pwd)"

ls -al "$BCC_NODE_SOCKET_PATH"

# NB: This zerepoch script uses a "typed" redeemer and "typed" datum.
zerepochscriptinuse="$BASE/scripts/zerepoch/scripts/context-equivalance-test.zerepoch"
# This datum hash is the hash of the typed 42
scriptdatumhash="fcaa61fb85676101d9e3398a484674e71c45c3fd41b492682f3b0054f4cf3273"
datumfilepath="$BASE/scripts/zerepoch/data/typed-42.datum"
redeemerfilepath="$BASE/scripts/zerepoch/data/script-context.redeemer"


# Step 1: Create a tx ouput with a datum hash at the script address. In order for a tx ouput to be locked
# by a zerepoch script, it must have a datahash. We also need collateral tx inputs so we split the utxo
# in order to accomodate this.


zerepochscriptaddr=$($BCC_CLI address build --payment-script-file "$zerepochscriptinuse"  --testnet-magic "$TESTNET_MAGIC")

mkdir -p "$WORK"

utxoaddr=$($BCC_CLI address build --testnet-magic "$TESTNET_MAGIC" --payment-verification-key-file "$UTXO_VKEY")

$BCC_CLI query utxo --address "$utxoaddr" --bcc-mode --testnet-magic "$TESTNET_MAGIC" --out-file $WORK/utxo-1.json
cat $WORK/utxo-1.json

txin=$(jq -r 'keys[]' $WORK/utxo-1.json)
entropicattxin=$(jq -r ".[\"$txin\"].value.entropic" $WORK/utxo-1.json)
entropicattxindiv3=$(expr $entropicattxin / 3)

$BCC_CLI query protocol-parameters --testnet-magic "$TESTNET_MAGIC" --out-file $WORK/pparams.json

$BCC_CLI transaction build \
  --aurum-era \
  --bcc-mode \
  --testnet-magic "$TESTNET_MAGIC" \
  --change-address "$utxoaddr" \
  --tx-in "$txin" \
  --tx-out "$zerepochscriptaddr+$entropicattxindiv3" \
  --tx-out-datum-hash "$scriptdatumhash" \
  --tx-out "$utxoaddr+$entropicattxindiv3" \
  --protocol-params-file "$WORK/pparams.json" \
  --out-file "$WORK/create-datum-output.body"

$BCC_CLI transaction sign \
  --tx-body-file $WORK/create-datum-output.body \
  --testnet-magic "$TESTNET_MAGIC" \
  --signing-key-file $UTXO_SKEY \
  --out-file $WORK/create-datum-output.tx

# SUBMIT
$BCC_CLI transaction submit --tx-file $WORK/create-datum-output.tx --testnet-magic "$TESTNET_MAGIC"
echo "Pausing for 5 seconds..."
sleep 5

# Step 2
# After "locking" the tx output at the script address, we can now can attempt to spend
# the "locked" tx output below.

$BCC_CLI query utxo --address $zerepochscriptaddr --testnet-magic "$TESTNET_MAGIC" --out-file $WORK/zerepochutxo.json

zerepochutxotxin=$(jq -r 'keys[]' $WORK/zerepochutxo.json)

$BCC_CLI query utxo --address $utxoaddr --bcc-mode --testnet-magic "$TESTNET_MAGIC" --out-file $WORK/utxo-2.json
cat $WORK/utxo-2.json
txinCollateral=$(jq -r 'keys[0]' $WORK/utxo-2.json)


dummyaddress=addr_test1vpqgspvmh6m2m5pwangvdg499srfzre2dd96qq57nlnw6yctpasy4

entropicatzerepochscriptaddr=$(jq -r ".[\"$zerepochutxotxin\"].value.entropic" $WORK/zerepochutxo.json)

echo "Zerepoch txin"
echo "$zerepochutxotxin"

echo "Collateral"
echo "$txinCollateral"

# We need to generate a dummy redeemer in order to create a txbody from which we can generate
# a tx and then derive the correct redeemer.
create-script-context --generate

correctredeemer="$WORK/script-context.redeemer"

# DUMMY TX!
$BCC_CLI transaction build \
  --aurum-era \
  --bcc-mode \
  --testnet-magic "$TESTNET_MAGIC" \
  --script-invalid \
  --change-address "$utxoaddr" \
  --certificate-file "example/addresses/user1-stake.reg.cert" \
  --invalid-before 1 \
  --invalid-hereafter 3000 \
  --tx-in "$zerepochutxotxin" \
  --tx-in-collateral "$txinCollateral" \
  --tx-out "$dummyaddress+10000000" \
  --tx-in-script-file "$zerepochscriptinuse" \
  --tx-in-datum-file "$datumfilepath"  \
  --protocol-params-file "$WORK/pparams.json" \
  --tx-in-redeemer-file "$correctredeemer" \
  --out-file $WORK/test-aurum.body

$BCC_CLI transaction sign \
  --tx-body-file $WORK/test-aurum.body \
  --testnet-magic "$TESTNET_MAGIC" \
  --signing-key-file "${UTXO_SKEY}" \
  --out-file $WORK/test-aurum.tx

# Generate the "real" redeeemer!

create-script-context \
  --generate-tx "$WORK/test-aurum.tx" \
  --bcc-mode \
  --testnet-magic 42 \

# REAL TX!
$BCC_CLI transaction build \
  --aurum-era \
  --bcc-mode \
  --testnet-magic "$TESTNET_MAGIC" \
  --script-valid \
  --invalid-before 1 \
  --invalid-hereafter 3000 \
  --change-address "$utxoaddr" \
  --certificate-file "example/addresses/user1-stake.reg.cert" \
  --tx-in "$zerepochutxotxin" \
  --tx-in-collateral "$txinCollateral" \
  --tx-out "$dummyaddress+10000000" \
  --tx-in-script-file "$zerepochscriptinuse" \
  --tx-in-datum-file "$datumfilepath"  \
  --protocol-params-file "$WORK/pparams.json" \
  --tx-in-redeemer-file "$correctredeemer" \
  --out-file $WORK/test-aurum-final.body

$BCC_CLI transaction sign \
  --tx-body-file $WORK/test-aurum-final.body \
  --testnet-magic "$TESTNET_MAGIC" \
  --signing-key-file "${UTXO_SKEY}" \
  --out-file $WORK/aurum.tx

# SUBMIT $WORK/aurum.tx containing the correct redeemer

echo "Submit the tx with zerepoch script and wait 5 seconds..."
$BCC_CLI transaction submit --tx-file $WORK/aurum.tx --testnet-magic "$TESTNET_MAGIC"
sleep 5
echo ""
echo "Querying UTxO at $dummyaddress. If there is ADA at the address the Zerepoch script successfully executed!"
echo ""
$BCC_CLI query utxo --address "$dummyaddress"  --testnet-magic "$TESTNET_MAGIC" \
  | tee "$RESULT_FILE"
