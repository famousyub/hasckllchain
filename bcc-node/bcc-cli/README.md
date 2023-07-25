# bcc-cli


A CLI utility to support a variety of key material operations (genesis, migration, pretty-printing..) for different system generations.

The general synopsis is as follows:

```
   Usage: bcc-cli (Genesis related CMDs | Key related CMDs | Delegation related CMDs | Transaction related CMDs | Local node related CMDs)
```

The top-level commands are as shown below.

```bash
$ bcc-cli --help
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

Cole-specific commands

```bash
$ bcc-cli cole --help
Usage: bcc-cli cole (key | transaction | query | genesis | governance |
                           miscellaneous)
  Cole specific commands

Available options:
  -h,--help                Show this help text

Available commands:
  key                      Cole key utility commands
  transaction              Cole transaction commands
  query                    Cole node query commands.
  genesis                  Cole genesis block commands
  governance               Cole governance commands
  miscellaneous            Cole miscellaneous commands
```

## How to build

### Cabal

Use [Cabal - Version 3.4.0.0](https://www.haskell.org/cabal/) to build and/or install this project:

```
$ cd bcc-cli
$ cabal build
$ cabal install
```

It may be necessary to specify the installation directory when installing the command using the `--installdir` option.
