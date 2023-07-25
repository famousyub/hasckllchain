
## Build Bcc Node + Image

https://docs.bcc.org/projects/bcc-node/en/latest/getting-started/building-the-node-using-nix.html

```
# Build + Install the bcc node
nix-build -A scripts.mainnet.node -o ~/bin/bcc-node

# Build + Install the bcc Docker image
docker load -i $(nix-build -A dockerImage --no-out-link) \
  && GITHASH=`git log -n1 --pretty='%H'` \
  && docker tag tbco/bcc-node:$GITHASH tbco/bcc-node:dev \
  && docker rmi tbco/bcc-node:$GITHASH

GITTAG=`git describe --exact-match --tags $GITHASH`
if [ $? -eq 0 ]; then
  echo "Current tag: $GITTAG"
  docker tag tbco/bcc-node:dev tbco/bcc-node:$GITTAG
fi

# Bash into the node to look around
docker run --rm -it --entrypoint=bash \
  -v node-data:/opt/bcc/data \
  tbco/bcc-node:dev

bcc-node run \
  --config /opt/bcc/config/mainnet-config.json \
  --topology /opt/bcc/config/mainnet-topology.json \
  --socket-path /opt/bcc/ipc/socket \
  --database-path /opt/bcc/data \
  --host-addr 0.0.0.0 \
  --port 3001
```

## Manual Testing

1. Run -e NETWORK=mainnet and check graceful shutdown SIGINT with -it
2. Run -e NETWORK=mainnet and check graceful shutdown SIGTERM with --detach
3. Run without -e NETWORK and check graceful shutdown SIGINT with -it
4. Run without -e NETWORK and check graceful shutdown SIGTERM with --detach
5. Check bcc-cli access

### Run with NETWORK

Run -e NETWORK=mainnet and check graceful shutdown SIGINT with -it

```
docker run --rm -it \
  -p 3001:3001 \
  -e NETWORK=mainnet \
  -v node-data:/data/db \
  tbco/bcc-node:dev
```

Run -e NETWORK=mainnet and check graceful shutdown SIGTERM with --detach

```
docker rm -f relay
docker run --detach \
  --name=relay \
  -p 3001:3001 \
  -e NETWORK=mainnet \
  -v node-data:/data/db \
  tbco/bcc-node:dev

docker logs -f relay
```

### Run without NETWORK

Check graceful shutdown SIGINT with -it

```
docker run --rm -it \
  -p 3001:3001 \
  -v node-data:/opt/bcc/data \
  tbco/bcc-node:dev run
```

Check graceful shutdown SIGTERM with --detach

```
docker rm -f relay
docker run --detach \
  --name=relay \
  -p 3001:3001 \
  -v node-data:/opt/bcc/data \
  -v node-ipc:/opt/bcc/ipc \
  tbco/bcc-node:dev run

docker logs -f relay
```

### Check bcc-cli

```
alias bcc-cli="docker run --rm -it \
  -v node-ipc:/opt/bcc/ipc \
  tbco/bcc-node:dev cli"

bcc-cli query tip --mainnet
```
