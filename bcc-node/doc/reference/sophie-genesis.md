# Making a Sophie blockchain from scratch

**Last validated: 2020/10/12**

## Preliminaries

This assumes that you already have built and installed the executables from
this repository. So `bcc-cli` and `bcc-node` should be on your `$PATH`.

We also assume a Linux system, though it should work fine on OSX too.

```bash
$ bcc-cli version
bcc-cli 1.21.1 - linux-x86_64 - ghc-8.6
```

Everything will be done in the Sophie era.

```bash
$ bcc-cli
bcc-cli - utility to support a variety of key operations (genesis
generation, migration, pretty-printing..) for different system generations.

Usage: bcc-cli (Era based commands | Cole specific commands |
                     Miscellaneous commands)

Available options:
  --version                Show the bcc-cli version
  -h,--help                Show this help text

Era based commands
  address                  Payment address commands
  stake-address            Stake address commands
  key                      Key utility commands
  transaction              Transaction commands
  node                     Node operation commands
  stake-pool               Stake pool commands
  query                    Node query commands. Will query the local node whose
                           Unix domain socket is obtained from the
                           BCC_NODE_SOCKET_PATH enviromnent variable.
  genesis                  Genesis block commands
  governance               Governance commands
  text-view                Commands for dealing with Sophie TextView files.
                           Transactions, addresses etc are stored on disk as
                           TextView files.

Cole specific commands
  cole                    Cole specific commands

Miscellaneous commands
  version                  Show the bcc-cli version
```

We'll put all files under an `example` directory.

## Making a genesis file manually

To start a new blockchain we of course need a genesis file. A Sophie genesis
file is a JSON file.

There is a manual method and a semi-automagic method, but we'll start by
explaining the manual method since that makes it easier to explain what the
things are and what they are for. So read this section even if you want to use
the automagic method.

So when doing it for real, we have to use the manual method since the different
steps are run by different people in different locations.

A real chain should use several genesis keys, and they should be created
separately by the members of the federation bootstrapping the system. They
should be created offline, kept offline and only the verification keys shared.

For a demo we're going to put all the keys together in one place.

To start with, we will set up our template directory:

```bash
$ bcc-cli genesis create --testnet-magic 42 --genesis-dir example/
```

This gives us

```bash
$ ls example/*
example/genesis.json  example/genesis.spec.json

example/delegate-keys:

example/genesis-keys:

example/utxo-keys:
```

Note that it created both a `genesis.spec.json` and a `genesis.json`. This
command can be re-run at any time and it will re-generate the `genesis.json`
based on the `genesis.spec.json` (which we can edit by hand) and any keys
placed in the three sub-directories.

Our next steps will be to create the various keys, adjust the
`genesis.spec.json` to our liking and re-generate the `genesis.json`.

### Genesis keys

Sophie supports decentralised block production but not yet decentralised
governance, so we still have genesis keys with special governance powers.

So the first step will be to make the genesis keys.

```bash
$ bcc-cli genesis key-gen-genesis
Usage: bcc-cli genesis key-gen-genesis --verification-key-file FILE
                                           --signing-key-file FILE
  Create a Sophie genesis key pair

Available options:
  --verification-key-file FILE
                           Output filepath of the verification key.
  --signing-key-file FILE  Output filepath of the signing key.
  -h,--help                Show this help text
```

So let's make two example genesis key pairs

```bash
$ bcc-cli genesis key-gen-genesis \
    --verification-key-file example/genesis-keys/genesis1.vkey \
    --signing-key-file example/genesis-keys/genesis1.skey
$ bcc-cli genesis key-gen-genesis \
    --verification-key-file example/genesis-keys/genesis2.vkey \
    --signing-key-file example/genesis-keys/genesis2.skey
```

### Semi-readable file formats

You can look at these files, they are semi-readable


```json
$ cat example/genesis-keys/genesis1.vkey
{
    "type": "GenesisVerificationKey_ed25519",
    "description": "Genesis Verification Key",
    "cborHex": "5820562ede753089653ee876b7f97c6f93435a320f6581423b73441ad24838c940fd"
}
```


The "type" must not be edited. This is used as a sanity check. The "description"
field is free-form and you can use it for whatever purpose you like, such as
identifying different keys. Don't edit the binary data of course, but you
can inspect it using any CBOR tool, e.g. [cbor.me] or the `bcc-cli`
itself:

```bash
$ bcc-cli text-view decode-cbor
Usage: bcc-cli text-view decode-cbor --in-file FILE [--out-file FILE]
  Print a TextView file as decoded CBOR.

Available options:
  --in-file FILE           CBOR input file.
  --out-file FILE          Optional output file. Default is to write to stdout.
  -h,--help                Show this help text
```

Like so

```bash
$ bcc-cli text-view decode-cbor \
    --file example/genesis-keys/genesis1.vkey

58 20 46 4e f4 95 59 f4 e3 6f b7 02 1f cb 12 71
c5 ba 84 f3 66 22 0a 15 0e 66 bb a8 71 87 2f 27
7c ed  # bytes(32)
```

So we can see this is just a 32 byte string. Not surprising, since this is of
course just an ed25519 verification key.

[cbor.me]: http://cbor.me/

### Genesis delegate keys

When we start a Sophie blockchain it will not be in decentralised block
production mode. Initially all blocks will be created by designated genesis
delegate nodes in the BFT overlay schedule. These genesis delegate nodes are
similar to stake pool nodes (but take part in the BFT overlay and don't get
rewards). The genesis file contains a special mapping from genesis keys to
genesis delegate keys.

So we need to make genesis delegate keys, as many as you made genesis keys
(just two in our example).

```bash
$ bcc-cli genesis key-gen-delegate
Usage: bcc-cli genesis key-gen-delegate --verification-key-file FILE
                                            --signing-key-file FILE
                                            --operational-certificate-issue-counter-file FILE
  Create a Sophie genesis delegate key pair

Available options:
  --verification-key-file FILE
                           Output filepath of the verification key.
  --signing-key-file FILE  Output filepath of the signing key.
  --operational-certificate-issue-counter-file FILE
                           The file with the issue counter for the operational
                           certificate.
  -h,--help                Show this help text
```

Much the same as for genesis keys, but there is an additional output, the
operational certificate issue counter. We will talk about this later.

Let's make two genesis delegate key pairs, to use with our two genesis keys

```bash
$ bcc-cli genesis key-gen-delegate \
    --verification-key-file example/delegate-keys/delegate1.vkey \
    --signing-key-file example/delegate-keys/delegate1.skey \
    --operational-certificate-issue-counter example/delegate-keys/delegate-opcert1.counter
$ bcc-cli genesis key-gen-delegate \
    --verification-key-file example/delegate-keys/delegate2.vkey \
    --signing-key-file example/delegate-keys/delegate2.skey \
    --operational-certificate-issue-counter example/delegate-keys/delegate-opcert2.counter
```

Let's see what's in that counter file

```json
$ cat example/delegate-keys/delegate-opcert1.counter
{
    "type": "NodeOperationalCertificateIssueCounter",
    "description": "Next certificate issue number: 0",
    "cborHex": "820058205325dad1168439e748e66b9151bd5c389ecdb7a46959639c3f97901128c7104b"
}
```

Yes, we count from zero. We will talk about what this counter is for later.


### Initial UTxO

We need to start the system with some money or it will be very boring. The
genesis file can list number of initial addresses and values, but we need
keys for those addresses and later to sign transactions to spend the initial
UTxO values.

So we need to make genesis initial UTxO keys.

```bash
$ bcc-cli genesis key-gen-utxo
Usage: bcc-cli genesis key-gen-utxo --verification-key-file FILE
                                        --signing-key-file FILE
  Create a Sophie genesis UTxO key pair

Available options:
  --verification-key-file FILE
                           Output filepath of the verification key.
  --signing-key-file FILE  Output filepath of the signing key.
  -h,--help                Show this help text
```

We can make as many as is useful. Let's make two.

```bash
$ bcc-cli genesis key-gen-utxo \
    --verification-key-file example/utxo-keys/utxo1.vkey \
    --signing-key-file example/utxo-keys/utxo1.skey
$ bcc-cli genesis key-gen-utxo \
    --verification-key-file example/utxo-keys/utxo2.vkey \
    --signing-key-file example/utxo-keys/utxo2.skey
```

### The genesis file itself

When we set up our template using the `create` command, it generated an
example genesis template for us in `example/genesis.spec.json`:

```json
{
    "activeSlotsCoeff": 5.0e-2,
    "protocolParams": {
        "vestedMultiple": 1,
        "poolDeposit": 0,
        "vestMultiple": 1,
        "protocolVersion": {
            "sentry": 0,
            "major": 0
        },
        "minUTxOValue": 0,
        "decentralisationParam": 1,
        "maxTxSize": 16384,
        "minPoolCost": 0,
        "minFeeA": 0,
        "maxBlockBodySize": 65536,
        "minFeeB": 0,
        "eMax": 18,
        "extraEntropy": {
            "tag": "NeutralNonce"
        },
        "maxBlockHeaderSize": 1100,
        "keyDeposit": 0,
        "nOpt": 100,
        "rho": 0,
        "tau": 0,
        "a0": 0
    },
    "vestedDelegs": {},
    "genDelegs": {},
    "vestedDelegs": {},
    "updateQuorum": 5,
    "networkId": "Testnet",
    "initialFunds": {},
    "maxEntropicSupply": 0,
    "networkMagic": 42,
    "epochLength": 432000,
    "staking": {
        "pools": {},
        "stake": {}
    },
    "systemStart": "1970-01-01T00:00:00Z",
    "slotsPerKESPeriod": 129600,
    "slotLength": 1,
    "maxKESEvolutions": 60,
    "securityParam": 2160
}
```

TODO: the generated file puts the fields in an unhelpful order.

We will mostly use these defaults for this demo. The meaning of all the ones
we do not edit here will be covered elsewhere.

When we regenerate the genesis file it will fill in the:

 * `genDelegs`
 * `initialFunds`
 * `systemStart`
 * and optionally it can override the `maxEntropicSupply`

We need to generate VRF keys to prove that the node has the right to create a block in this slot.

So let's do that too

```bash
$ bcc-cli node key-gen-VRF \
    --verification-key-file example/delegate-keys/delegate1.vrf.vkey \
    --signing-key-file example/delegate-keys/delegate1.vrf.skey

$ bcc-cli node key-gen-VRF \
    --verification-key-file example/delegate-keys/delegate2.vrf.vkey \
    --signing-key-file example/delegate-keys/delegate2.vrf.skey
```

Let's regenerate the genesis file (note, this command does not set an initial
Entropic supply, that will be done later)

```bash
$ bcc-cli genesis create --testnet-magic 42 --genesis-dir example/
```

and then look at it and understand what the command has done

```json
$ cat example/genesis.json
{   "vestMultiple": 1,
    "vestedDelegs": {}, 
    "activeSlotsCoeff": 5.0e-2,
    "protocolParams": {
        "vestedMultiple": 1,
        "poolDeposit": 0,
        "protocolVersion": {
            "sentry": 0,
            "major": 0
        },
        "minUTxOValue": 0,
        "decentralisationParam": 1,
        "maxTxSize": 16384,
        "minPoolCost": 0,
        "minFeeA": 0,
        "maxBlockBodySize": 65536,
        "minFeeB": 0,
        "eMax": 18,
        "extraEntropy": {
            "tag": "NeutralNonce"
        },
        "maxBlockHeaderSize": 1100,
        "keyDeposit": 0,
        "nOpt": 100,
        "rho": 0,
        "tau": 0,
        "a0": 0
    },
    "vestedDelegs": {},
    "genDelegs": {
        "f42b0eb14056134323d9756fa693dba5e421acaaf84fdaff922a4c0f": {
            "delegate": "e446c231ace1f29eb83827f29cb4a19e4c324229d59472c8d2dbb958",
            "vrf": "e5b6b13eacc21968953ecb78eb900c1eaa2b4744ffead8719f9064f4863e1813"
        },
        "3d59ef27a268cd2deeec005b27a0fac78fb3a3945325ce46c3c63f39": {
            "delegate": "1968838d5f545f6c49e18a4a356ccf62163e4ae39b871537a7dffef0",
            "vrf": "351e18df241d5bfd9f7ca0c169c432702cda840d93467cfcbf1e4ddd9b7e4ff4"
        }
    },
    "updateQuorum": 5,
    "networkId": "Testnet",
    "initialFunds": {
        "600547e1d85598a728f577497a122c98f42a56d7411e23e97ed4d3956c": 0,
        "6003662510383a9901958f7a16ceb977917d8102eb2013f4ba5e0b0763": 0
    },
    "maxEntropicSupply": 0,
    "networkMagic": 42,
    "epochLength": 432000,
    "staking": {
        "pools": {},
        "stake": {}
    },
    "systemStart": "2020-10-12T13:37:58.004306094Z",
    "slotsPerKESPeriod": 129600,
    "slotLength": 1,
    "maxKESEvolutions": 60,
    "securityParam": 2160
}
```

The `genDelegs` is the mapping from genesis keys to genesis delegates. The
representation in the JSON file is between key hashes.

So to understand where it got the key hashes from we can use a command to
get the key hash for each key:

```bash
$ bcc-cli genesis key-hash
Usage: bcc-cli genesis key-hash --verification-key-file FILE
  Print the identifier (hash) of a public key

Available options:
  --verification-key-file FILE
                           Input filepath of the verification key.
  -h,--help                Show this help text
```

Let's do that for our genesis key and genesis delegate key

```bash
$ bcc-cli genesis key-hash \
    --verification-key-file example/genesis-keys/genesis1.vkey
  f42b0eb14056134323d9756fa693dba5e421acaaf84fdaff922a4c0f

$ bcc-cli genesis key-hash \
    --verification-key-file example/delegate-keys/delegate1.vkey
  e446c231ace1f29eb83827f29cb4a19e4c324229d59472c8d2dbb958

$ bcc-cli node key-hash-VRF \
    --verification-key-file example/delegate-keys/delegate1.vrf.vkey
  e5b6b13eacc21968953ecb78eb900c1eaa2b4744ffead8719f9064f4863e1813
```

So now we can see where the hashes from the `genDelegs` came from

```
"genDelegs": {
    "f42b0eb14056134323d9756fa693dba5e421acaaf84fdaff922a4c0f": {
        "delegate": "e446c231ace1f29eb83827f29cb4a19e4c324229d59472c8d2dbb958",
        "vrf": "e5b6b13eacc21968953ecb78eb900c1eaa2b4744ffead8719f9064f4863e1813"
    }
```

Next it's a similar deal with the `initialFunds`. This is a mapping from the
initial _addresses_ to the initial values at those address. So we need a
command to get the address corresponding to an initial UTxO verification key:

```bash
$ bcc-cli genesis initial-addr
Usage: bcc-cli genesis initial-addr --verification-key-file FILE
                                        (--mainnet | --testnet-magic NATURAL)
                                        [--out-file FILE]
  Get the address for an initial UTxO based on the verification key

Available options:
  --verification-key-file FILE
                           Input filepath of the verification key.
  --mainnet                Use the mainnet magic id.
  --testnet-magic NATURAL  Specify a testnet magic id.
  --out-file FILE          Optional output file. Default is to write to stdout.
  -h,--help                Show this help text
```

So let's do that for the UTxO key

```bash
$ bcc-cli genesis initial-addr \
    --verification-key-file example/utxo-keys/utxo1.vkey \
    --testnet-magic 42

$ bcc-cli genesis initial-addr \
    --verification-key-file example/utxo-keys/utxo2.vkey \
    --testnet-magic 42
```

And if we compare this with the `initialFunds` from the generated file we see

```json
    "initialFunds": {
        "600547e1d85598a728f577497a122c98f42a56d7411e23e97ed4d3956c": 0,
        "6003662510383a9901958f7a16ceb977917d8102eb2013f4ba5e0b0763": 0
    },
```

This means we'll start with 0 entropic in a special genesis UTxO at that
address.

Ok, so zero entropic is not that useful. We can however edit the
`genesis.spec.json` and set the `maxEntropicSupply` there, or we specify the
initial supply when we re-generate the genesis file. Either way, it will be
split equally between all the utxo keys.

```bash
$ bcc-cli genesis create \
    --testnet-magic 42 \
    --genesis-dir example/ \
    --supply 1000000
```

Yes [ONE MILLION ENTROPIC].

If we look again at the generated genesis file now we'll see

```json
    "initialFunds": {
        "600547e1d85598a728f577497a122c98f42a56d7411e23e97ed4d3956c": 500000,
        "6003662510383a9901958f7a16ceb977917d8102eb2013f4ba5e0b0763": 500000
    },
```

So we see the amount split between our two initial addresses.

You will also see that the `maxEntropicSupply` is set to this same supply. If
you edit this manually note that it has to be at least as big as the sum total
from our `initialFunds`, but it can be bigger to allow for monetary expansion
later for stake rewards.

Finally there is the `systemStart`, which is the agreed time of slot zero.
By default the `create` command filled this in to be 30s into the
future, but you can also specify this manually with `--start-time UTC_TIME`
or edit it manually afterwards. It needs to be set to a time in the near future
or near past. It cannot be too far in the past otherwise the system would start
having missed a very large number of slots.


## Making a genesis file semi-automagically

If you jumped straight in here, skipping the manual method, do go back and
review that section covers the concepts about what these keys are all for.

Also remember: **when doing it for real you cannot use the automagic method.**
It is not secure to because it makes all the keys in one place.
When doing it for real the people involved have to follow the manual method
where keys are generated separately on secure offline machines.

But for demos it is fine

```bash
$ bcc-cli genesis
Usage: bcc-cli genesis (key-gen-genesis | key-gen-delegate | key-gen-vested | key-gen-vesteddelegate |key-gen-utxo |
                             key-hash | get-ver-key | initial-addr |
                             initial-txin | create | create-staked | hash)
  Genesis block commands

Available options:
  -h,--help                Show this help text

Available commands:
  key-gen-genesis          Create a Sophie genesis key pair
  key-gen-delegate         Create a Sophie genesis delegate key pair
  key-gen-vested           Create a Sophie genesis vested key pair
  key-gen-vesteddelegate   Create a Sophie genesis vesteddelegate key pair
  key-gen-utxo             Create a Sophie genesis UTxO key pair
  key-hash                 Print the identifier (hash) of a public key
  get-ver-key              Derive the verification key from a signing key
  initial-addr             Get the address for an initial UTxO based on the
                           verification key
  initial-txin             Get the TxIn for an initial UTxO based on the
                           verification key
  create                   Create a Sophie genesis file from a genesis template
                           and genesis/delegation/spending keys.
  create-staked            Create a staked Sophie genesis file from a genesis
                           template and genesis/delegation/spending keys.
  hash                     Compute the hash of a genesis file
```

The automagic method uses the `create` command and all the others
are for the manual method.

```bash
$ bcc-cli genesis create
Usage: bcc-cli genesis create --genesis-dir DIR [--gen-genesis-keys INT] [--gen-vested-keys INT]
                                  [--gen-utxo-keys INT] [--start-time UTC-TIME]
                                  [--supply ENTROPIC]
                                  (--mainnet | --testnet-magic NATURAL)
  Create a Sophie genesis file from a genesis template and
  genesis/delegation/spending keys.

Available options:
  --genesis-dir DIR        The genesis directory containing the genesis template
                           and required genesis/delegation/spending keys.
  --gen-genesis-keys INT   The number of genesis keys to make [default is 0].
  --gen-vested-keys INT    The number of vested keys to make [default is 0]
  --gen-utxo-keys INT      The number of UTxO keys to make [default is 0].
  --start-time UTC-TIME    The genesis start time in YYYY-MM-DDThh:mm:ssZ
                           format. If unspecified, will be the current time +30
                           seconds.
  --supply ENTROPIC        The initial coin supply in Entropic which will be
                           evenly distributed across initial, non-delegating
                           stake holders.
  --mainnet                Use the mainnet magic id.
  --testnet-magic NATURAL  Specify a testnet magic id.
  -h,--help                Show this help text
```

This command will generate a genesis file. It can also generate all the keys,
or it can pick up keys you created manually.

It follows this file layout convention:

 * `${genesisdir}/genesis.json`
 * `${genesisdir}/genesis.spec.json`
 * `${genesisdir}/genesis-keys/genesis${N}.{vkey,skey}`
 * `${genesisdir}/genesis-keys/vested${N}.{vkey,skey}`
 * `${genesisdir}/delegate-keys/delegate${N}.{vkey,skey}`
 * `${genesisdir}/delegate-keys/delegate-opcert${N}.counter`
 * `${genesisdir}/vesteddelegate-keys/vesteddelegate${N}.{vkey,skey}`
 * `${genesisdir}/vesteddelegate-keys/vesteddelegate-opcert${N}.counter`
 * `${genesisdir}/utxo-keys/utxo${N}.{vkey,skey}`

By default it will not create any keys for you, and will pick up any that you
have created manually following the file layout convention.

You can set up the directory layout with a default genesis spec file

```bash
$ bcc-cli genesis create --testnet-magic 42 --genesis-dir example/
```

The `create` command can also create all the necessary keys for you.
The optional `--gen-genesis-keys`, `--gen-vested-keys`, and `--gen-utxo-keys` flags can be used to
specify the number of keys of each kind to generate.

We still need a genesis spec to start from. Here's the default genesis spec
file `example/genesis.spec.json`

```json
{
    "vestMultiple": 1,
    "vestedDelegs": {},
    "securityParam": 2160,
    "slotsPerKESPeriod": 129600,
    "updateQuorum": 5,
    "activeSlotsCoeff": 5.0e-2,
    "protocolParams": {
        "poolDeposit": 0,
        "protocolVersion": {
            "sentry": 0,
            "major": 0
        },
        "minUTxOValue": 0,
        "decentralisationParam": 1,
        "maxTxSize": 16384,
        "minPoolCost": 0,
        "minFeeA": 0,
        "maxBlockBodySize": 65536,
        "minFeeB": 0,
        "eMax": 18,
        "extraEntropy": {
            "tag": "NeutralNonce"
        },
        "maxBlockHeaderSize": 1100,
        "keyDeposit": 0,
        "nOpt": 100,
        "rho": 0,
        "tau": 0,
        "a0": 0
    },
    "genDelegs": {},
    "networkId": "Testnet",
    "maxKESEvolutions": 60,
    "initialFunds": {},
    "maxEntropicSupply": 0,
    "networkMagic": 42,
    "slotLength": 1,
    "systemStart": "1970-01-01T00:00:00Z",
    "epochLength": 432000,
    "staking": {
        "pools": {},
        "stake": {}
    },
}
```

The `create` will read the `genesis.spec.json` and produce the
`genesis.json` by filling in the:

 * `genDelegs`
 * `initialFunds`
 * `systemStart`
 * and optionally it can override the `maxEntropicSupply`

Everything else we have to fill in manually, either in the template or
afterwards.

So let's try it:

```bash
$ bcc-cli genesis create \
    --genesis-dir example/ \
    --supply 1000000000 \
    --gen-genesis-keys 2 \
    --gen-utxo-keys 2 \
    --testnet-magic 42
```

We're going for more zeros on our money supply this time, after all
[why make trillions when we could make billions?]

Let's have a look at the result

```json
$ cat example/genesis.json
{
    "vestMultiple": 1,
    "securityParam": 2160,
    "slotsPerKESPeriod": 129600,
    "updateQuorum": 5,
    "activeSlotsCoeff": 5.0e-2,
    "protocolParams": {
        "minUTxOValue": 0,
        "eMax": 18,
        "extraEntropy": {
            "tag": "NeutralNonce"
        },
        "minFeeB": 0,
        "tau": 0.0,
        "maxBlockBodySize": 65536,
        "maxTxSize": 16384,
        "minPoolCost": 0,
        "minFeeA": 1,
        "nOpt": 100,
        "maxBlockHeaderSize": 1100,
        "keyDeposit": 0,
        "poolDeposit": 0,
        "protocolVersion": {
            "sentry": 0,
            "major": 0
        },
        "a0": 0.0,
        "rho": 0.0,
        "decentralisationParam": 1.0
    },
    "maxEntropicSupply": 0,
    "networkMagic": 42,
    "initialFunds": {
        "6089d93346c322ba1487051c793dc1d90071efd2ccb063fdb05ec5cf09": 0,
        "6060a1cca04000f8b427992878794dda0adfd36bfe0708bd6e17b7326e": 0
    },
    "networkId": "Testnet",
    "maxKESEvolutions": 60,
    "vestedDelegs": {},
    "genDelegs": {
        "40b6b2c3920e04948baa3a0d73307d106bb38752a11e3a4a71a84889": {
            "delegate": "a615747a919f1d1c40de03bcc97b4d7a7ca9c5feab3e2b6f5187ca90",
            "vrf": "3fe07080e0c0e81152a3057cd597ee92693d5abf8c2b893072420e437e8bd6f9"
        },
        "9855f0c73dbe25d13fc343651a999ed427168d91af341ee300325e94": {
            "delegate": "43d573b721045c984d5377ef1d4ed3804f27a6d6b607be521f7f7426",
            "vrf": "711f0e2e9ec3edf2f76aeed19c9096718eea7e98764e8454574c9c3a136f2729"
        }
    },
    "slotLength": 1,
    "systemStart": "2022-02-24T16:04:13.997535103Z",
    "epochLength": 432000,
    "staking": {
        "pools": {},
        "stake": {}
    }
}
```

And the files it made
```bash
$ ls example/*/
example-default/delegate-keys/:
delegate1.counter  delegate1.vkey      delegate1.vrf.vkey  delegate2.skey  delegate2.vrf.skey
delegate1.skey     delegate1.vrf.skey  delegate2.counter   delegate2.vkey  delegate2.vrf.vkey

example-default/genesis-keys/:
genesis1.skey  genesis1.vkey  genesis2.skey  genesis2.vkey

example-default/utxo-keys/:
utxo1.skey  utxo1.vkey  utxo2.skey  utxo2.vkey

example/vesteddelegate-keys:
vesteddelegate1.skey  vesteddelegate1.vrf.skey  vesteddelegate2.skey  vesteddelegate2.vrf.skey  vesteddelegate-opcert1.counter
vesteddelegate1.vkey  vesteddelegate1.vrf.vkey  vesteddelegate2.vkey  vesteddelegate2.vrf.vkey  vesteddelegate-opcert2.counter
```

You'll notice that the automagic method has divided the total supply amongst
the initial UTxO keys, but you can still edit this file manually to adjust that
if you want.

## Node operational keys and certificates

Armed just with the genesis file we could now start a node, however it would
raise difficult philosophical questions about the nature of a blockchain with
no blocks. To avoid such questions, let's aim to have some nodes that can create
blocks. Of course this means everyone's favourite: more keys.

Sophie uses a "hot key / cold key" scheme for block producing nodes:

 * the cold key is intended to be kept securely offline (hence "cold"),
 * while the hot key is kept on the node itself and used to sign block headers.

Rather than cold and hot, we typically refer to the operator's offline key
(cold) and their operational key (hot).

The basic idea of such a scheme is that if the operational key is compromised
then a new one can be issued and the old one invalidated. This involves
establishing the link between the operator's offline key and their operational
key. This is done by means of a certificate. The certificate identifies the
current operational key, and is signed by the offline key. The certificate also
contains an issue counter number so that all other nodes can see when a new
certificate is being used and old certificates should be considered invalid.

The act of "issuing" a new certificate simply means the act of signing a new
certificate using the offline key.

To make things even more fun, Sophie uses *two* operational keys:

 * A KES key, using magic crypto;
 * A VRF key, using even more magic crypto.

They are both used in block headers. The KES key is used to prove that the
node is who it says it is, just like a normal signature. The VRF key is used
to prove that the node has the right to create a block in this slot.

The use of a VRF key is special to Shardagnostic Optimum. In a "normal" proof-of-stake
blockchain (like Shardagnostic Classic or BFT) one simply knows who has the right to
make the block in each slot, because we *know* what the slot leader schedule is:
that is the slot leader schedule is public. So in that case you only have to
prove you are who you say you are, and everyone can check that the slot leader
schedule says if you're the slot leader or not. Shardagnostic Optimum has a *private*
slot leader schedule. This means that nobody knows in advance who is going to
be the slot leader, but once someone is, they can prove to everyone else that
they are. And that is what the VRF key is for: proving that.

KES stands for **K**ey **E**volving **S**ignature. It is like a normal
signature scheme, but with the "forward-security" property. The signing key is
"evolved" after a number of slots (e.g. the number of slots equivalent to 24
hours) to give a new signing key, and the old key is forgotten. It means that
if someone breaks into a server running a node, while they can steal the
current signing key, they should not be able to recover the signing keys from
earlier periods. This means that an attacker cannot sign blocks for this node
for the past, only for the current and future. This helps to prevent the
creation of large false alternative histories of the blockchain.

As is normal security practice, hot or operational keys should be cycled after
a while, and new operational keys issued. There is also a technical limitation
that KES signing keys can only be evolved a finite number of times. The larger
that choice of the maximum number of evolutions, the larger the signatures
become. The signatures are included in block headers, and for good performance
we want to keep block headers small. In Sophie, the KES keys will need to
be reissued before 90 days, but they can always be reissued earlier.

So in order to run a Sophie node we will need to:

- generate an operator's offline key;
- generate a KES operational key;
- generate a VRF operational key; and
- issue an operational certificate

The latter three are needed by the node itself, and issuing the operational
certificate needs the operator's offline key.

Now if you followed the previous section on constructing a genesis file, then
you have already generated the operator offline keys: the genesis delegates are
exactly that. The other operator offline keys are stake pool keys. For most
purposes the genesis delegate and stake pool operator offline keys are the same:
both get used to issue operational certs.

We can create stake pool operator keys using:

```bash
$ bcc-cli node key-gen
Usage: bcc-cli node key-gen --cold-verification-key-file FILE
                                --cold-signing-key-file FILE
                                --operational-certificate-issue-counter-file FILE
  Create a key pair for a node operator's offline key and a new certificate
  issue counter

Available options:
  --cold-verification-key-file FILE
                           Filepath of the cold verification key.
  --cold-signing-key-file FILE
                           Filepath of the cold signing key.
  --operational-certificate-issue-counter-file FILE
                           The file with the issue counter for the operational
                           certificate.
  -h,--help                Show this help text
```

For now we will ignore stake pools however since we need to get the system
bootstrapped with the BFT overlay.

### KES Keys

There's a command to generate new KES keys

```bash
$ bcc-cli node key-gen-KES
Usage: bcc-cli node key-gen-KES --verification-key-file FILE
                                    --signing-key-file FILE
  Create a key pair for a node KES operational key

Available options:
  --verification-key-file FILE
                           Output filepath of the verification key.
  --signing-key-file FILE  Output filepath of the signing key.
  -h,--help                Show this help text
```

So let's go ahead and create a KES key for our first two nodes

```bash
$ mkdir example/{node1,node2}

$ bcc-cli node key-gen-KES \
    --verification-key-file example/node1/kes.vkey \
    --signing-key-file example/node1/kes.skey

$ bcc-cli node key-gen-KES \
    --verification-key-file example/node2/kes.vkey \
    --signing-key-file example/node2/kes.skey
```
If you look at these files, you'll see that KES signing keys are quite chunky,
especially compared to our normal ed25519 keys.

### VRF Keys

And there's a command to generate new VRF keys

```bash
$ bcc-cli node key-gen-VRF
Usage: bcc-cli node key-gen-VRF --verification-key-file FILE
                                    --signing-key-file FILE
  Create a key pair for a node VRF operational key

Available options:
  --verification-key-file FILE
                           Output filepath of the verification key.
  --signing-key-file FILE  Output filepath of the signing key.
  -h,--help                Show this help text
```

However, the `genesis create` command has already generated VRF keys for you at: `example/delegate-keys/delegate{1,2}.vrf.{vkey,skey}`
and the corresponding key hashes exist in the `genesis.json` file in the `vrf` key.

### Issuing an operational certificate

Now we get to the stage of wanting to issue an operational certificate.

When doing this for real, the operator's offline key should of course be
offline, so we would issue the certificate on the offline machine with
the offline key, and copy the resulting certificate to the operational machine.

```bash
$ bcc-cli node issue-op-cert
Usage: bcc-cli node issue-op-cert (--kes-verification-key STRING |
                                        --kes-verification-key-file FILE)
                                      --cold-signing-key-file FILE
                                      --operational-certificate-issue-counter-file FILE
                                      --kes-period NATURAL --out-file FILE
  Issue a node operational certificate

Available options:
  --kes-verification-key STRING
                           A Bech32 or hex-encoded hot KES verification key.
  --kes-verification-key-file FILE
                           Filepath of the hot KES verification key.
  --cold-signing-key-file FILE
                           Filepath of the cold signing key.
  --operational-certificate-issue-counter-file FILE
                           The file with the issue counter for the operational
                           certificate.
  --kes-period NATURAL     The start of the KES key validity period.
  --out-file FILE          The output file.
  -h,--help                Show this help text
```

There's a few things here to understand.

As discussed above, a certificate identifies an operational KES key that we
will be using to sign block headers, so we need its verification key. It is
signed by the operator's offline key so we need that signing key.

As mentioned, certificates have an issue counter number that is used to
inform other nodes that older certificates are now invalid. This certificate
issue counter must be kept with the operator's offline key. You'll notice they
got created when we created the genesis delegate keys, or if you create a new
stake pool key.

Finally we have a confusing flag for the KES period. Each operational
certificate specifies when the certificate is valid from. This is like a date
but is specified in terms of KES periods, which is some number of slots long.
Frankly, this needs improving in the CLI tools and/or documentation. For now
we are creating a system from scratch so we can start with period 0.

So let's go ahead and issue ourselves an operational certificate. We will sign
using use the genesis delegate keys we created earlier.

```bash
$ bcc-cli node issue-op-cert \
    --kes-verification-key-file example/node1/kes.vkey \
    --cold-signing-key-file example/delegate-keys/delegate1.skey \
    --operational-certificate-issue-counter example/delegate-keys/delegate-opcert1.counter \
    --kes-period 0 \
    --out-file example/node1/cert

$ bcc-cli node issue-op-cert \
    --kes-verification-key-file example/node2/kes.vkey \
    --cold-signing-key-file example/delegate-keys/delegate2.skey \
    --operational-certificate-issue-counter example/delegate-keys/delegate-opcert2.counter \
    --kes-period 0 \
    --out-file example/node2/cert
```

## Starting a node

Now that we have generated our `genesis.json`, KES and VRF operational keys and
issued ourselves operational certificates, we are close to being able to run
our nodes.

The last things we need are node configuration and topology files.

For the configuration file, we can start with the default
[Cole mainnet configuration]

```bash
$ cp configuration/defaults/cole-mainnet/configuration.yaml \
     example/
```

and make a couple tweaks

```bash
$ sed -i 's/^Protocol: RealPBFT/Protocol: Optimum/' example/configuration.yaml
$ sed -i 's/^minSeverity: Info/minSeverity: Debug/' example/configuration.yaml
$ sed -i 's/^TraceBlockchainTime: False/TraceBlockchainTime: True/' example/configuration.yaml
```

We will share this `configuration.yaml` file between both nodes we run.

The topology files tell nodes which other nodes to talk to. We will need one
for each node we run. For this demo we will be running two nodes on the same
machine, one on port 3001 and the other on 3002. We will configure them to
talk to each other.

So create the following two files as `example/node1/topology.json` and
`example/node2/topology.json`.

```json
{
  "Producers": [
    {
      "addr": "127.0.0.1",
      "port": 3002,
      "valency": 1
    }
  ]
}
```

```json
{
  "Producers": [
    {
      "addr": "127.0.0.1",
      "port": 3001,
      "valency": 1
    }
  ]
}
```

So node1 will listen on port 3001 and contact node2 on port 3002, and the other
way around for node2.

Now we are ready to run our two nodes. With the way we have set them up should
both be block-producing nodes that take part in the BFT overlay schedule.

One final tweak before we start: it has probably been some time since we
generated the `genesis.json`, so the start time is probably now some time in
the past. It is a bit nicer if we set the start time to be shortly in the
future and then start up our nodes. That way they will all wait for the start
time and then make blocks from the beginning.

So let's just re-generate our `genesis.json` which will set the start time to
be 30 seconds into the future, and then we can start our nodes. This assumes
you did not do any manual tweaking of the generated `genesis.json`, as it will
be overwritten.

```bash
$ bcc-cli genesis create --testnet-magic 42 --genesis-dir example/
```

So, now in two separate terminal windows we can launch our nodes. Node 1:

```bash
$ bcc-node run \
    --config example/configuration.yaml \
    --topology example/node1/topology.json \
    --database-path example/node1/db \
    --socket-path example/node1/node.sock \
    --sophie-kes-key example/node1/kes.skey \
    --sophie-vrf-key example/delegate-keys/delegate1.vrf.skey \
    --sophie-operational-certificate example/node1/cert \
    --port 3001
```

And node 2:

```bash
$ bcc-node run \
    --config example/configuration.yaml \
    --topology example/node2/topology.json \
    --database-path example/node2/db \
    --socket-path example/node2/node.sock \
    --sophie-kes-key example/node2/kes.skey \
    --sophie-vrf-key example/delegate-keys/delegate2.vrf.skey \
    --sophie-operational-certificate example/node2/cert \
    --port 3002
```

The default configuration will log everything to stdout, and we turned the
log level up so we'll see even debug messages, so it will be pretty voluminous.

If you did manage to start the nodes before the start time, you'll see them
waiting:

```bash
[localhost:bcc.node:Debug:5] [2020-05-10 21:46:40.77 UTC]
  Waiting 23.921899468s until genesis start time at 2020-05-10 21:47:04.701779756 UTC
```

After that, you should see the nodes start to alternately create blocks and
adopt each others blocks. Remember that we have configured slots to be 1 second
long but only 1 in 20 slots will have a block in it.

[Cole mainnet configuration]: https://github.com/The-Blockchain-Company/bcc-node/blob/master/configuration/defaults/cole-mainnet/configuration.yaml


## Querying the node

Now that our nodes are running, let's poke them and see if they're doing what
we expect. We'll need a third terminal.

### Querying protocol parameters

We'll start with querying the node to see the current set of protocol parameters.
```bash
$ bcc-cli query protocol-parameters
Usage: bcc-cli query protocol-parameters [--sophie-mode | --cole-mode
                                               [--epoch-slots NATURAL] |
                                               --bcc-mode
                                               [--epoch-slots NATURAL]]
                                             (--mainnet |
                                               --testnet-magic NATURAL)
                                             [--out-file FILE]
  Get the node's current protocol parameters

Available options:
  --sophie-mode           For talking to a node running in Sophie-only mode.
  --cole-mode             For talking to a node running in Cole-only mode.
  --epoch-slots NATURAL   The number of slots per epoch for the Cole era.
                          (default: 21600)
  --bcc-mode              For talking to a node running in full Bcc mode
                          (default).
  --epoch-slots NATURAL   The number of slots per epoch for the Cole era.
                          (default: 21600)
  --mainnet               Use the mainnet magic id.
  --testnet-magic NATURAL Specify a testnet magic id.
  --out-file FILE         Optional output file. Default is to write to stdout.
  -h,--help               Show this help text
```

The only surprising extra flag is the "network magic". This is the
`networkMagic` from the `genesis.json`. In the default we generated, that was
given as 42. The network "magic" number is used as a simple sanity check (not a
security measure of course) when nodes connect to each other, to stop nodes
accidentally connecting to nodes running different blockchains, e.g. testnet
vs mainnet. We have the same sanity check when we connect to the local node.
So we have to specify `--testnet-magic 1097911063`, otherwise it defaults to mainnet
and then the handshake would fail.

This command of course connects to a local node. The socket for the local
node is set via an environment variable `BCC_NODE_SOCKET_PATH`. Typically one
only runs one node on a machine, and so it would make sense to set this for
the whole terminal session:

```bash
export BCC_NODE_SOCKET_PATH=$PWD/example/node1/node.sock
```

In this demo we are running two nodes however so we'll specify the env var each
time in the command.

```json
$ BCC_NODE_SOCKET_PATH=example/node1/node.sock \
    bcc-cli query protocol-parameters \
    --testnet-magic 42 \
    --sophie-mode
{    
    "vestMultiple": 1,
    "poolDeposit": 0,
    "protocolVersion": {
        "sentry": 0,
        "major": 0
    },
    "minUTxOValue": 0,
    "decentralisationParam": 1,
    "maxTxSize": 16384,
    "minPoolCost": 0,
    "minFeeA": 0,
    "maxBlockBodySize": 65536,
    "minFeeB": 0,
    "eMax": 18,
    "extraEntropy": {
        "tag": "NeutralNonce"
    },
    "maxBlockHeaderSize": 1100,
    "keyDeposit": 0,
    "nOpt": 100,
    "rho": 0,
    "tau": 0,
    "a0": 0
}
```

As we can see, it spits out the same set of protocol parameters from the
`genesis.json` file we started with.

### Querying the UTxO

For our next trick we will inspect the UTxO. Of course we have not done
any transactions yet so we expect just the initial UTxO as specified in
the `genesis.json`.

There is a command to query the UTxO and return all the UTxOs at a given
address.

```bash
$ bcc-cli query utxo
Usage: bcc-cli query utxo [--sophie-mode | --cole-mode
                                [--epoch-slots NATURAL] |
                                --bcc-mode [--epoch-slots NATURAL]]
                              [(--address ADDRESS)]
                              (--mainnet | --testnet-magic NATURAL)
                              [--out-file FILE]
  Get the node's current UTxO with the option of filtering by address(es)

Available options:
  --sophie-mode           For talking to a node running in Sophie-only mode.
  --cole-mode             For talking to a node running in Cole-only mode.
  --epoch-slots NATURAL    The number of slots per epoch for the Cole era.
                           (default: 21600)
  --bcc-mode           For talking to a node running in full Bcc mode
                           (default).
  --epoch-slots NATURAL    The number of slots per epoch for the Cole era.
                           (default: 21600)
  --address ADDRESS        Filter by Bcc address(es) (Bech32-encoded).
  --mainnet                Use the mainnet magic id.
  --testnet-magic NATURAL  Specify a testnet magic id.
  --out-file FILE          Optional output file. Default is to write to stdout.
  -h,--help                Show this help text
```

So what address do we need? We need to build the bech32 address of a utxo verification key as follows:

```bash
$ bcc-cli -- sophie address build \
    --payment-verification-key-file example/utxo-keys/utxo1.vkey \
    --testnet-magic 42

addr_test1vz6drvkn3rx4v3zd9ftdu8n2g7yuyqgnd92wafv235rwdjq30677a
```
Obviously you will have to adjust this command to use the right address(es) from your `genesis.json`.
```bash
$ BCC_NODE_SOCKET_PATH=example/node1/node.sock \
    bcc-cli query utxo \
    --testnet-magic 42 \
    --sophie-mode \
    --address addr_test1vz6drvkn3rx4v3zd9ftdu8n2g7yuyqgnd92wafv235rwdjq30677a

                           TxHash                                 TxIx        Entropic
----------------------------------------------------------------------------------------
e727f95ad8eedf5153405f4f3eb6fb797aba94f8d4ca18b09918459fccb798b8     0         500000000
```

So this tells us that there is exactly one UTxO entry at that address, with one
unspent output (output zero).

The next step will be to spend this.

## Submitting a Genesis initial UTxO transaction

### Preparing the ingredients

There are a few things we need to make our first transaction:

 * A UTxO to spend
 * The signing key for the UTxO we're spending
 * An address to spend to

Spending the Genesis initial UTxOs is a little special because of the way the
initial UTxO is constructed. But we have seen above that we can find the UTxOs
that we want to spend.

We also have the signing key from when we constructed the genesis, e.g. in
`example/utxo-keys/utxo1.skey`.

For an output address, we will need to make one.

### Finding the initial UTxO TxIn

To avoid confusion about which UTxO goes with which key, we have this handy
command

```bash
$ bcc-cli genesis initial-txin
Usage: bcc-cli genesis initial-txin --verification-key-file FILE
                                        (--mainnet | --testnet-magic NATURAL)
                                        [--out-file FILE]
  Get the TxIn for an initial UTxO based on the verification key

Available options:
  --verification-key-file FILE
                           Input filepath of the verification key.
  --mainnet                Use the mainnet magic id.
  --testnet-magic NATURAL  Specify a testnet magic id.
  --out-file FILE          Optional output file. Default is to write to stdout.
  -h,--help                Show this help text
```

Which we can use with our `example/utxo-keys/utxo1.vkey`
```bash
$ bcc-cli genesis initial-txin \
    --verification-key-file example/utxo-keys/utxo1.vkey \
    --testnet-magic 42

e727f95ad8eedf5153405f4f3eb6fb797aba94f8d4ca18b09918459fccb798b8#0
```

Note the TxIn syntax of "long hash # ix", here with index 0.

### Making new keys and addresses

We will make a key pair and then build an address from that. Let's start with
the keypair.

```bash
$ bcc-cli address key-gen
Usage: bcc-cli address key-gen [--normal-key | --extended-key | --cole-key]
                                   --verification-key-file FILE
                                   --signing-key-file FILE
  Create an address key pair.

Available options:
  --normal-key             Use a normal Sophie-era key (default).
  --extended-key           Use an extended ed25519 Sophie-era key.
  --cole-key               Use a Cole-era key.
  --verification-key-file FILE
                           Output filepath of the verification key.
  --signing-key-file FILE  Output filepath of the signing key.
  -h,--help                Show this help text
```

So let's do it
```bash
$ bcc-cli address key-gen \
    --verification-key-file example/addr1.vkey \
    --signing-key-file example/addr1.skey
```

We can now build an address from our key.

```bash
$ bcc-cli address build
Usage: bcc-cli address build (--payment-verification-key STRING |
                                   --payment-verification-key-file FILE)
                                 [--stake-verification-key STRING |
                                   --stake-verification-key-file FILE]
                                 (--mainnet | --testnet-magic NATURAL)
                                 [--out-file FILE]
  Build a Sophie payment address, with optional delegation to a stake address.

Available options:
  --payment-verification-key STRING
                           Payment verification key (Bech32-encoded)
  --payment-verification-key-file FILE
                           Filepath of the payment verification key.
  --stake-verification-key STRING
                           Stake verification key (Bech32 or hex-encoded).
  --stake-verification-key-file FILE
                           Filepath of the staking verification key.
  --mainnet                Use the mainnet magic id.
  --testnet-magic NATURAL  Specify a testnet magic id.
  --out-file FILE          Optional output file. Default is to write to stdout.
  -h,--help                Show this help text
```

This command can also build payment addresses that are associated with stake
addresses, and thus have the ability to delegate the stake rights.

For now however let's see our boring address with no stake rights:
```bash
$ bcc-cli address build \
    --payment-verification-key-file example/addr1.vkey \
    --testnet-magic 42

addr_test1vzrqr58zm3un86sfeze6039gj8v406p3zt4su0qkemc5vyqrs09az
```

The CLI uses a low-tech hex encoding for addresses, rather than fancy bech32.

### Building an unsigned transaction body

We will be using the low level `build-raw` command because a more convenient
command is not available yet, and because for spending a Genesis UTxO we have
to use the low level command to select the exact UTxO to spend.

```bash
$ bcc-cli transaction build-raw
Usage: bcc-cli transaction build-raw [--cole-era | --sophie-era |
                                           --evie-era | --jen-era]
                                         (--tx-in TX-IN
                                         [--txin-script-file FILE])
                                         [--tx-out TX-OUT]
                                         [--mint VALUE
                                           (--minting-script-file FILE)]
                                         [--invalid-before SLOT]
                                         [--invalid-hereafter SLOT]
                                         [--fee ENTROPIC]
                                         [--certificate-file CERTIFICATEFILE
                                           [--certificate-script-file FILE]]
                                         [--withdrawal WITHDRAWAL
                                           [--withdrawal-script-file FILE]]
                                         [--json-metadata-no-schema |
                                           --json-metadata-detailed-schema]
                                         [--auxiliary-script-file FILE]
                                         [--metadata-json-file FILE |
                                           --metadata-cbor-file FILE]
                                         [--update-proposal-file FILE]
                                         --out-file FILE
  Build a transaction (low-level, inconvenient)

Available options:
  --cole-era                          Specify the Cole era
  --sophie-era                        Specify the Sophie era
  --evie-era                          Specify the Evie era
  --jen-era                           Specify the Jen era (default)
  --tx-in TX-IN                       TxId#TxIx
  --txin-script-file FILE             Filepath of the spending script witness
  --tx-out TX-OUT                     The transaction output as Address+Entropic where
                                      Address is the Bech32-encoded address followed by the
                                      amount in Entropic.
  --mint VALUE                        Mint multi-asset value(s) with the multi-asset cli
                                      syntax. You must specifiy a script witness.
  --minting-script-file FILE          Filepath of the multi-asset witness script.
  --invalid-before SLOT               Time that transaction is valid from (in slots).
  --invalid-hereafter SLOT            Time that transaction is valid until (in slots).
  --fee ENTROPIC                      The fee amount in Entropic.
  --certificate-file CERTIFICATEFILE  Filepath of the certificate. This encompasses all
                                      types of certificates (stake pool certificates, stake
                                      key certificates etc). Optionally specify a script
                                      witness.
  --certificate-script-file FILE      Filepath of the certificate script witness
  --withdrawal WITHDRAWAL             The reward withdrawal as StakeAddress+Entropic where
                                      StakeAddress is the Bech32-encoded stake address
                                      followed by the amount in Entropic. Optionally
                                      specify a script witness.
  --withdrawal-script-file  FILE      Filepath of the withdrawal script witness.
  --json-metadata-no-schema           Use the "no schema" conversion from JSON to tx metadata.
  --json-metadata-detailed-schema     Use the "detailed schema" conversion from JSON to tx
                                      metadata.
  --auxiliary-script-file FILE        Filepath of auxiliary script(s)
  --metadata-json-file FILE           Filepath of the metadata file, in JSON format.
  --metadata-cbor-file FILE           Filepath of the metadata, in raw CBOR format.
  --update-proposal-file FILE         Filepath of the update proposal.
  --out-file FILE                     Output filepath of the JSON TxBody.
  -h,--help                           Show this help text
```

Yes, this is a very inconvenient way to build transactions, but at least you'll
be able to say you understand the UTxO model a little better.

So we have the transaction input and output. The TxIn uses "TxId#TxIx" syntax.
That will be the TxIn we got from the `initial-txin` command. The TxOut uses
"TxOut+Entropic" syntax. We'll build that by taking the address from the
`address build` command from the previous section and adding "+500000000" for
the value we want to move to that address.

Next we have the TTL. In Sophie every transaction has an expiry slot. If the
transaction does not make it into the chain by this slot then it will be
considered invalid and will never be added to the chain (on that fork). This is
generally helpful to avoid potentially valid transactions from floating around
for ever. It let's us guarantee that after some point we definitely do not have
to worry about them any more. For boring technical reasons it is required
rather than optional. Pick a slot number that is sufficiently far into the
future. You can look at the node log files or query the node's tip to find the
current slot number.

Finally we have the fee. In Sophie the transaction fee is explicit, rather
than implicitly being the difference between the inputs and the outputs.
Furthermore the inputs and outputs + fee, must balance *exactly*. You can still
choose the fee, it just has to be above the minimum fee. With the demo genesis
we made, the fees are zero so we can ignore this complication for now.

So we build the unsigned transaction and place it in `example/tx1.txbody`

```bash
$ bcc-cli transaction build-raw \
    --sophie-era \
    --tx-in  e727f95ad8eedf5153405f4f3eb6fb797aba94f8d4ca18b09918459fccb798b8#0 \
    --tx-out addr_test1vzrqr58zm3un86sfeze6039gj8v406p3zt4su0qkemc5vyqrs09az+500000000 \
    --invalid-hereafter 3600 \
    --fee 0 \
    --tx-body-file example/tx1.txbody
```

### Making a signed transaction

The next step is to sign it. Sophie transactions sometimes need lots of
signatures, e.g. when registering stake pools, or spending from multi-sig
addresses. Typically however we just need one signature for each address we use
in inputs (that's one per address, not one per input UTxO, if there are multiple
inputs from the same address).

```bash
$ bcc-cli transaction sign
Usage: bcc-cli transaction sign --tx-body-file FILE
                                    (--signing-key-file FILE
                                      [--address STRING] |
                                      --script-file FILE)
                                    [--mainnet | --testnet-magic NATURAL]
                                    --out-file FILE
  Sign a transaction

Available options:
  --tx-body-file FILE      Input filepath of the JSON TxBody.
  --signing-key-file FILE  Input filepath of the signing key (one or more).
  --address STRING         Cole address (Base58-encoded).
  --script-file FILE       Filepath of the script.
  --mainnet                Use the mainnet magic id.
  --testnet-magic NATURAL  Specify a testnet magic id.
  --out-file FILE          Output filepath of the JSON Tx.
  -h,--help                Show this help text
```

In our example we need just the one signature, using the `utxo1.skey`.
```bash
$ bcc-cli transaction sign \
  --tx-body-file example/tx1.txbody \
  --signing-key-file example/utxo-keys/utxo1.skey \
  --testnet-magic 1097911063 \
  --tx-file example/tx1.tx
```

### Submitting the signed transaction

Finally we need to submit the signed transaction

```bash
$ bcc-cli transaction submit
Usage: bcc-cli transaction submit [--sophie-mode | --cole-mode
                                        [--epoch-slots NATURAL] |
                                        --bcc-mode [--epoch-slots NATURAL]]
                                      (--mainnet | --testnet-magic NATURAL)
                                      --tx-file FILE
  Submit a transaction to the local node whose Unix domain socket is obtained
  from the BCC_NODE_SOCKET_PATH enviromnent variable.

Available options:
  --sophie-mode           For talking to a node running in Sophie-only mode.
  --cole-mode             For talking to a node running in Cole-only mode.
  --epoch-slots NATURAL    The number of slots per epoch for the Cole era.
                           (default: 21600)
  --bcc-mode           For talking to a node running in full Bcc mode
                           (default).
  --epoch-slots NATURAL    The number of slots per epoch for the Cole era.
                           (default: 21600)
  --mainnet                Use the mainnet magic id.
  --testnet-magic NATURAL  Specify a testnet magic id.
  --tx-file FILE           Filepath of the transaction you intend to submit.
```

This command also needs the `BCC_NODE_SOCKET_PATH` like the other commands
that need to talk to a local node. And as mentioned above in the section on
querying the node, we have to specify `--testnet-magic 1097911063`, otherwise it
defaults to mainnet and then the handshake with the node would fail.

So let's do it.
```bash
BCC_NODE_SOCKET_PATH=example/node1/node.sock \
    bcc-cli transaction submit \
      --sophie-mode \
      --tx-file example/tx1.tx \
      --testnet-magic 42
```

If we now go look at the node logs (or stdout) for node1 we should see that
the transaction was accepted into the mempool, and some seconds later we should
see that the next block included the transaction.

We can also check using the UTxO query, but now using the new address we moved
the funds to
```bash
BCC_NODE_SOCKET_PATH=example/node1/node.sock \
    bcc-cli query utxo \
      --sophie-mode \
      --testnet-magic 42 \
      --address addr_test1vzrqr58zm3un86sfeze6039gj8v406p3zt4su0qkemc5vyqrs09az

                           TxHash                                 TxIx        Entropic
----------------------------------------------------------------------------------------
d17b4303135a76574f18b28fda25bc82cf29c72eb52e12ad317319714a5aafdb     0         500000000
```

So since the fees were zero we move the full amount, and we have a new UTxO
entry at our target address.

From here we can do more "normal" transactions to move funds to other addresses
or build and submit transactions with special certificates in them.

## Submitting a "normal" transaction

TODO: normal txs, much like above, then certs, stake addresses etc.
