#!/usr/bin/env bash

# This script is intended to be as simple as possible i.e execute and have a
# cluster of 3 sophie nodes up and running.

ROOT="$(realpath "$(dirname "$0")/../..")"

configuration="${ROOT}/scripts/lite/configuration"

if [ "$1" == "" ];then
   data_dir="$(mktemp).d"
else
   data_dir=$1
fi

mkdir -p "${data_dir}"

# Generate sophie genesis spec
ARGSSPEC=(
  --genesis-dir           "${data_dir}/genesis"
  --testnet-magic 42
)

cabal run exe:bcc-cli -- genesis create "${ARGSSPEC[@]}"

OS=$(uname -s)

case $OS in
  Darwin )       DATE="gdate"; SED='gsed';;
  * )            DATE="date";  SED='sed' ;;
esac


# We're going to use really quick epochs (300 seconds), by using short slots 1s
# and K=10, but we'll keep long KES periods so we don't have to bother
# cycling KES keys
$SED -i ${data_dir}/genesis/genesis.spec.json \
    -e 's/"slotLength": 1/"slotLength": 1/' \
    -e 's/"activeSlotsCoeff": 5.0e-2/"activeSlotsCoeff": 0.1/' \
    -e 's/"securityParam": 2160/"securityParam": 10/' \
    -e 's/"epochLength": 432000/"epochLength": 1500/' \
    -e 's/"maxEntropicSupply": 0/"maxEntropicSupply": 9000/' \
    -e 's/"decentralisationParam": 1.0/"decentralisationParam": 0.7/'

# Generate sophie genesis "for real"

ARGS=(
  --genesis-dir           "${data_dir}/genesis"
  --gen-genesis-keys      3
  --gen-utxo-keys         3
  --testnet-magic 42
)

cabal run exe:bcc-cli -- genesis create "${ARGS[@]}"

# Compute genesis hash
cabal run exe:bcc-cli -- genesis hash --genesis "${data_dir}/genesis/genesis.json" | tail -1 > "${data_dir}"/genesis/GENHASH

# Ensure the node is built
cabal run --no-stats bcc-node bcc-node --help >/dev/null || true

for i in 1 2 3; do
  # Use copy default configuration and topolgy to configuration directory for a particular node instance
  cp -af "${configuration}/sophie-$i.yaml" "${data_dir}"
  cp -af "${configuration}/topology-node-$i.json" "${data_dir}"
  db_dir="${data_dir}/db/node-$i"
  socket_dir="${data_dir}/socket"

  mkdir -p "${db_dir}"
  mkdir -p "${socket_dir}"

  esc=$(printf '\033')

  # We need the following for a sophie node to be able to mint blocks:
  # - KES signing key
  # - VRF signing key
  # - Operational certificate
  # VRF keys have already been generated in the sophie genesis create command


  # Generate a KES keys
  mkdir -p "${data_dir}/node-$i"
  cabal run exe:bcc-cli -- node key-gen-KES \
    --verification-key-file "${data_dir}/node-$i/kes.vkey" \
    --signing-key-file      "${data_dir}/node-$i/kes.skey"

  # Move genesis delegate keys generated in sophie genesis create command to its
  # respective node folder.

  mv "${data_dir}/genesis/delegate-keys/delegate$i.skey"      "${data_dir}/node-$i/hotkey.skey"
  mv "${data_dir}/genesis/delegate-keys/delegate$i.vkey"      "${data_dir}/node-$i/hotkey.vkey"
  mv "${data_dir}/genesis/delegate-keys/delegate$i.counter"   "${data_dir}/node-$i/counterFile.counter"
  mv "${data_dir}/genesis/delegate-keys/delegate$i.vrf.skey"  "${data_dir}/node-$i/vrf.skey"
  mv "${data_dir}/genesis/delegate-keys/delegate$i.vrf.vkey"  "${data_dir}/node-$i/vrf.vkey"

  # Set permissions for the vrf private key file: read for owner only
  chmod gou-rwx "${data_dir}/node-$i/vrf.skey"
  chmod u+r "${data_dir}/node-$i/vrf.skey"

  # Issue an operational certificate:
  cabal run exe:bcc-cli -- node issue-op-cert \
      --kes-period 0 \
      --kes-verification-key-file                  "${data_dir}/node-$i/kes.vkey"  \
      --cold-signing-key-file                      "${data_dir}/node-$i/hotkey.skey" \
      --operational-certificate-issue-counter-file "${data_dir}/node-$i/counterFile.counter" \
      --out-file                                   "${data_dir}/node-$i/opcert"

  # Launch a node
  cabal run exe:bcc-node -- run \
    --database-path "${db_dir}" \
    --socket-path "${socket_dir}/node-$i-socket" \
    --port "300$i" \
    --config "${data_dir}/sophie-$i.yaml" \
    --topology "${data_dir}/topology-node-$i.json" \
    --sophie-vrf-key "${data_dir}/node-$i/vrf.skey" \
    --sophie-kes-key "${data_dir}/node-$i/kes.skey" \
    --sophie-operational-certificate "${data_dir}/node-$i/opcert" \
    | sed "s|^|${esc}[$((31+$i))m[node-$i]${esc}[0m |g" &
done

rm -r  "${data_dir}/genesis/delegate-keys"

function cleanup()
{
  for child in $(jobs -p); do
    echo kill "$child" && kill "$child"
  done
}

cat

trap cleanup EXIT
