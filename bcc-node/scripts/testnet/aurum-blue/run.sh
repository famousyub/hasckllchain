#!/usr/bin/env bash

mkdir -p testnet/aurum-blue

cp -r scripts/testnet/aurum-blue/*.json testnet/aurum-blue
cp -r scripts/testnet/aurum-blue/*.yaml testnet/aurum-blue

bcc-node run \
  --socket-path testnet/aurum-blue/socket \
  --topology testnet/aurum-blue/topology.yaml \
  --database-path testnet/aurum-blue/db \
  --port 50001 \
  --config testnet/aurum-blue/configuration.yaml
