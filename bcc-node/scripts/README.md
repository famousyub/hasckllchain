# Scripts Overview
The `scripts` directory consists of the following directories:
- [benchmarking](#benchmarking)
- [buildkite](#buildkite)
- [cole-sophie-evie-jen](#cole-sophie-evie-jen)
- [lite](#lite)
- [sophie-from-scratch](#sophie-from-scratch)

#### benchmarking
Contains all the scripts relevant to benchmarking `bcc-node`. See the benchmarking [README](benchmarking/README.md).

#### buildkite
Contains scripts relevant to TBCO's CI.

#### cole-sophie-evie-jen
Contains a script that sets up a cluster beginning in the Cole era and can transition to the Sophie era. You can also start a cluster in the Sophie, Evie or Jen era by supplying an argument to `mk-files.sh`.
E.g
```bash
./scripts/cole-to-jen/mk-files.sh sophie # Starts nodes in Sophie era
./scripts/cole-to-jen/mk-files.sh evie # Starts nodes in Evie era
./scripts/cole-to-jen/mk-files.sh jen    # Starts nodes in Jen era
```
#### lite
Contains scripts that can start various clusters and intended to be as simple as possible. Note that using the sophie only era testnet clusters breaks compatibility with some cli commands.

#### sophie-from-scratch
Contains a script that creates all the necessary keys etc to create a sophie cluster from scratch.

