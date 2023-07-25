#!/usr/bin/env bash

set -e
# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail

# This is an example zerepoch minting script derived from the validator in
# Bcc.CLI.Zerepoch.SimpleMintingScript. Essentially we demand that the
# script owner is allowed to mint "MillarCoin" and you can only mint
# a single "MillarCoin" per tx.

work=example/minting
mkdir -p $work

# Step 1 - Send BCC to token script owner

export BCC_NODE_SOCKET_PATH="${BCC_NODE_SOCKET_PATH:-example/node-bft1/node.sock}"
zerepochscriptinuse=scripts/zerepoch/scripts/anyone-can-mint.zerepoch

utxovkey=example/sophie/utxo-keys/utxo1.vkey
utxoskey=example/sophie/utxo-keys/utxo1.skey
utxoaddr=$(bcc-cli address build --testnet-magic 42 --payment-verification-key-file $utxovkey)
bcc-cli query utxo --address $utxoaddr --bcc-mode --testnet-magic 42 --out-file utxo.json
txin=$(jq -r 'keys[]' utxo.json)

entropicattxin=$(jq -r ".[\"$txin\"].value.entropic" utxo.json)
entropicattxindiv3=$(expr $entropicattxin / 3)

bcc-cli address key-gen \
  --normal-key \
  --verification-key-file "$work/minting.vkey" \
  --signing-key-file "$work/minting.skey"

targetvkey="$work/minting.vkey"
targetskey="$work/minting.skey"
targetaddr=$(bcc-cli address build --testnet-magic 42 --payment-verification-key-file $targetvkey)

bcc-cli query protocol-parameters --testnet-magic 42 --out-file example/pparams.json

bcc-cli transaction build \
  --aurum-era \
  --bcc-mode \
  --testnet-magic 42 \
  --change-address "$utxoaddr" \
  --tx-in "$txin" \
  --tx-out "$targetaddr+$entropicattxindiv3" \
  --tx-out "$targetaddr+$entropicattxindiv3" \
  --protocol-params-file example/pparams.json \
  --out-file "$work/fund-script-owner.body"

bcc-cli transaction sign \
  --tx-body-file "$work/fund-script-owner.body" \
  --testnet-magic 42 \
  --signing-key-file "$utxoskey" \
  --out-file "$work/fund-script-owner.tx"

# SUBMIT
bcc-cli transaction submit --tx-file "$work/fund-script-owner.tx" --testnet-magic 42

echo "Pausing for 5 seconds..."
sleep 5

bcc-cli query utxo --address "$targetaddr" --testnet-magic 42 --out-file "$work/updatedutxo.json"
scriptownertxin=$(jq -r 'keys[0]' "$work/updatedutxo.json")
scriptownerCollateral=$(jq -r 'keys[1]' "$work/updatedutxo.json")

# Step 2: Mint a single MillarCoin

# We need the script policy ID
policyid=$(bcc-cli transaction policyid --script-file $zerepochscriptinuse)
redeemer=scripts/zerepoch/data/42.redeemer
entropicatzerepochscriptaddr=$(jq -r ".[\"$scriptownertxin\"].value.entropic" "$work/updatedutxo.json")

dummyaddress=addr_test1vpqgspvmh6m2m5pwangvdg499srfzre2dd96qq57nlnw6yctpasy4

echo "Entropic at address: $entropicatzerepochscriptaddr"


bcc-cli transaction build \
  --aurum-era \
  --bcc-mode \
  --testnet-magic 42 \
  --change-address "$utxoaddr" \
  --tx-in "$scriptownertxin" \
  --tx-in-collateral "$scriptownerCollateral" \
  --mint-script-file "$zerepochscriptinuse" \
  --mint-redeemer-file "$redeemer" \
  --tx-out "$dummyaddress+1000000 + 5 $policyid.MillarCoin" \
  --mint "5 $policyid.MillarCoin" \
  --protocol-params-file example/pparams.json \
  --out-file "$work/zerepochmint.body"

bcc-cli transaction sign \
  --tx-body-file "$work/zerepochmint.body" \
  --testnet-magic 42 \
  --signing-key-file "$targetskey" \
  --out-file "$work/zerepochmint.tx"

# SUBMIT
bcc-cli transaction submit --tx-file "$work/zerepochmint.tx" --testnet-magic 42

echo "Pausing for 5 seconds..."
sleep 5
bcc-cli query utxo --whole-utxo --testnet-magic 42