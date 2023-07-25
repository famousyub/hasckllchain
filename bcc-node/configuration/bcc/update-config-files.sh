#!/bin/bash

set -e

OUT=$(dirname $(realpath $0))
ROOT=$(realpath ${OUT}/../..)
SRC=$(nix-build ${ROOT}/release.nix -A bcc-deployment)

copyFile() {
  echo $1
  cp ${SRC}/$1 ${OUT}/$1
}

echo "#################"
echo "# Copying files #"
echo "#################"

copyFile "mainnet-aurum-genesis.json"
copyFile "mainnet-cole-genesis.json"
copyFile "mainnet-config.json"
copyFile "mainnet-sophie-genesis.json"
copyFile "mainnet-topology.json"
