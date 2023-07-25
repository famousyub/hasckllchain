

.. raw:: html

  <p align="center">
    <a href="https://github.com/The-Blockchain-Company/bcc-node/releases"><img src="https://img.shields.io/github/release-pre/The-Blockchain-Company/bcc-node.svg?style=for-the-badge" /></a>
    <a href="https://buildkite.com/The-Blockchain-Company/bcc-node"><img src="https://img.shields.io/buildkite/a978cbb4def7018be3d0a004127da356f4db32f1c318c1a48a/master?label=BUILD&style=for-the-badge"/></a>
  </p>

  <table align="center">
    <tr><td colspan="2" align="center">Github Actions</td></tr>
    <tr>
      <td>
        <a href="https://github.com/The-Blockchain-Company/bcc-node/actions/workflows/haskell.yml"><img alt="GitHub Workflow Status (master)" src="https://img.shields.io/github/workflow/status/The-Blockchain-Company/bcc-node/Haskell%20CI/master" /></a>
        <a href="https://github.com/The-Blockchain-Company/bcc-node/actions/workflows/haskell.yml"><img alt="GitHub Workflow Status (branch)" src="https://img.shields.io/github/workflow/status/The-Blockchain-Company/bcc-node/Haskell%20CI/nightly?label=nightly" /></a>
      </td>
    </tr>
  </table>

=================
**IMPORTANT**
=================

THIS IS NOT A PRODUCTION BUILD, THIS IS A WORK IN PROGRESS AND A DEVELOPMENT TOOL FOR QUANTUM ONE DAO. THE QUANTUMONE.NETWORK DOMAIN HAS CHANGED TO QUANTUMONE.IO. EXISTING REFERENCES TO QUANTUMONE.NETWORK ARE STILL IN THE PROCESS OF BEING CLEANED UP. PLEASE KEEP IN MIND THAT BCC CHAIN AND ECOSYSTEM DEMO WAS BUILT BY ONE PERSON - GITHUB USER RMOUREY26 - AND LARGELY UNCHANGED SINCE JANUARY 2022.  

PRESENTLY THE BCC CHAIN IS COMPRISED OF ROUGHLY 7K OF 'NEW' HASKELL BLOCKCHAIN CODE AND INTEGRATED WITH A MODIFIED FORK OF CARDANO INCLUDING BYRON THROUGH ALONZO ERAS. ADDITIONAL FEATURES INCLUDE
- VESTED AND VESTED DELEGATE KEY ROLES AND OPTIONAL GENESIS PARAMETERS
- VEST MULTIPLE PROTOCOL PARAMETER 
- 2ND METADATA POINTER
- REDUCED NUMBER OF PROTOCOL VERSIONS TO 2 

Why? The original idea was to create an alternative reward structure where genesis delegates and vested delegates could co-exist and receive different reward %. Vested Delegates could be anything from bond holders, angel investors, to charity groups, and more. The Sentry protocol version could further secure and differentiate Vested Delegate reward mechanisms from Genesis and traditional staking delegates, VestedMultiples, and or other functional representation of chain time via changing and entropic chain data. Additional logic must be implemented in order to realize alternate reward payouts. 

WHAT WORKS? 
- Testchain Initiation 
- Cole to Aurum era transition voting transactions 1-3. 
-- Transaction 4 throws an error which seems to he due to the need for additional logic handling the reduction in the number of protocols down to 2. 

WHATS THE GOAL?
- THIS ALL BEGAN AS A REQUIREMENT FROM A CLIENT WHO WANTED A FORK OF CARDANO. AND 5 MONTHS LATER, THIS IS WHAT I EMDED UP CREATING OUTSIDE OF THE REQUIREMENT. I WONDERED IF IT WAS POSSIBLE TO SIGNIFICANTLY MODIFY CARDANO AND CREATE A NEW CHAIN AND OR PRIVATE VERSIONS OF CARDANO ITSELF. THIS EXPERIENCE AND DEMO ECOSYSTEM WILL BE USED DURING THE DEVELOPMENT OF QUANTUM ONE. QUANTUM ONE WILL NOT BECOME A FORK OF CARDANO.

=================
API Documentation
=================
The API documentation is published `here <https://The-Blockchain-Company.github.io/>`_.

The documentation is built with each push, but is only published from `master` branch.  In order to
test if the documentation is working, build the documentation locally with `./scripts/haddocs.sh` and
open `haddocks/index.html` in the browser.

*************************
``bcc-node`` Overview
*************************
**** Compile, build and, run bcc-node and BCC ecosystem using Cabal and GHC on  Nixos, Linux or WSL2. The quick build method using Ubuntu 20.04 is the most efficient way to test the Bcc-Chain. NixOs and other Linux distros may require some tinkering.   ****

Integration of the `ledger <https://github.com/The-Blockchain-Company/bcc-ledger-specs>`_, `consensus <https://github.com/The-Blockchain-Company/shardagnostic-network/tree/master/shardagnostic-consensus>`_,
`networking <https://github.com/The-Blockchain-Company/shardagnostic-network/tree/master/shardagnostic-network>`_ and
`node shell <https://github.com/The-Blockchain-Company/bcc-shell>`_ repositories.

`Logging <https://github.com/The-Blockchain-Company/tbco-monitoring-framework>`_ is provided as a
`feature <https://github.com/The-Blockchain-Company/bcc-shell/blob/master/app/Bcc/Shell/Features/Logging.hs>`_ by the node shell to the other packages.

- The bcc-node is the top level for the node and
  aggregates the other components from other packages: consensus, ledger and
  networking, with configuration, CLI, logging and monitoring.

- The node no longer incorporates wallet or explorer functionality. The wallet
  backend and explorer backend are separate components that run in separate
  external processes that communicate with the node via local IPC.


Network Configuration, Genesis and Topology Files - THIS LINK IS NOT VALID. 
=================================================

The latest supported networks can be found at `<https://hydra.quantumone.network/job/Bcc/bcc-node/bcc-deployment/latest-finished/download/1/index.html>`_

Quick Build Ubuntu 20.04 (Other distros will build with tinkering)
==================================================================

.. code-block:: console

    sudo apt-get update -y

    sudo apt-get upgrade -y

    sudo apt-get install git jq bc make automake rsync htop curl build-essential pkg-config libffi-dev libgmp-dev libssl-dev libtinfo-dev libsystemd-dev zlib1g-dev make g++ wget libncursesw5 libtool autoconf -y

    mkdir $HOME/git
    cd $HOME/git
    git clone https://github.com/The-Blockchain-Company/libsodium
    cd libsodium
    git checkout 66f017f1
    ./autogen.sh
    ./configure
    make
    sudo make install

Debian OS: extra lib linking may be required

.. code-block:: console

    sudo ln -s /usr/local/lib/libsodium.so.23.3.0 /usr/lib/libsodium.so.23

AWS Linux CentOS: clearing the lib cache may be required.

.. code-block:: console

    sudo ldconfig

Raspberry Pi 4 with Ubuntu: extra lib linking may be required

.. code-block:: console

    sudo apt-get install libnuma-dev

Install Cabal & dependencies

.. code-block:: console

    sudo apt-get -y install pkg-config libgmp-dev libssl-dev libtinfo-dev libsystemd-dev zlib1g-dev build-essential curl libgmp-dev libffi-dev libncurses-dev libtinfo5

    curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

Respond 'NO' to install Haskell-Language-Server (HLS) 
Respond 'Yes' to automatically add required PATH variable to .bashrc

.. code-block:: console

    cd $HOME
    source .bashrc
    ghcup upgrade
    ghcup install cabal 3.4.0.0
    ghcup set cabal 3.4.0.0

Install GHC

.. code-block:: console

    ghcup install ghc 8.10.4
    ghcup set ghc 8.10.4

    echo PATH="$HOME/.local/bin:$PATH" >> $HOME/.bashrc
    echo export LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH" >> $HOME/.bashrc
    echo export NODE_HOME=$HOME/bcc-my-node >> $HOME/.bashrc
    echo export NODE_CONFIG=mainnet>> $HOME/.bashrc
    source $HOME/.bashrc

TestNet guidance - to set to testnet rather then mainnet...

.. code-block:: console

    echo export NODE_CONFIG=testnet>> $HOME/.bashrc
    source $HOME/.bashrc

and wherever you see 

'--mainnet' 

in the CLI command instructions, replace it with 

'--testnet-magic 1097911063' 

Update Cabal and Verify Install 

.. code-block:: console

    cabal update
    cabal --version
    ghc --version

Cabal version should be 3.4.0.0 and ghc should be 8.10.4 

** re: code block below --> FYI git fetch not necessary, git checkout releases not necessary nor will it work as there are no github releases as of yet, will update readme when release pushed ** 

.. code-block:: console

    cd $HOME/git
    git clone https://github.com/The-Blockchain-Company/bcc-node.git
    cd Bcc-node
    git fetch --all --recurse-submodules --tags
    git checkout $(curl -s https://api.github.com/repos/The-Blockchain-Company/bcc-node/releases/latest | jq -r .tag_name)

    cabal configure -O0 -w ghc-8.10.4

    echo -e "package bcc-crypto-optimum\n flags: -external-libsodium-vrf" > cabal.project.local
    sed -i $HOME/.cabal/config -e "s/overwrite-policy:/overwrite-policy: always/g"
    rm -rf $HOME/git/bcc-node/dist-newstyle/build/x86_64-linux/ghc-8.10.4 to reset previous build folder
    cabal build all

Copy Node and CLI files to bin

.. code-block:: console

    sudo cp $(find $HOME/git/bcc-node/dist-newstyle/build -type f -name "bcc-cli") /usr/local/bin/bcc-cli

    sudo cp $(find $HOME/git/bcc-node/dist-newstyle/build -type f -name "bcc-node") /usr/local/bin/bcc-node

Test Versioning with

.. code-block:: console

    bcc-cli --version 
    bcc-node --version


Additional build methods -
========================

Docker image - 
============

You can pull the docker image with the latest version of bcc-node from `here <https://hub.docker.com/r/tbco/bcc-node>`_.

.. code-block:: console

    docker pull tbco/bcc-node


Additional documentation for building the node will be available soon. 

Linux Executable - Soon available at hydra.quantumone.network 
==================

You can download the latest version of ``bcc-node`` and ``bcc-cli``:

* `linux <https://hydra.tbco.io/job/Bcc/bcc-node/bcc-node-linux/latest-finished>`_
* `win64 <https://hydra.tbco.io/job/Bcc/bcc-node/bcc-node-win64/latest-finished>`_
* `macos <https://hydra.tbco.io/job/Bcc/bcc-node/bcc-node-macos/latest-finished>`_

Windows Executable - NOT AVAILABLE
==================

Download
--------

You can download `here <https://hydra.tbco.io/job/Bcc/bcc-node/bcc-node-win64/latest-finished>`_.

Instructions
------------

The download includes bcc-node.exe and a .dll. To run the node with bcc-node run you need to reference a few files and directories as arguments. These can be copied from the bcc-node repo into the executables directory. The command to run the node on mainnet looks like this:

.. code-block:: console

    bcc-node.exe run --topology ./mainnet-topology.json --database-path ./state --port 3001 --config ./configuration-mainnet.yaml --socket-path \\.\pipe\bcc-node


``bcc-node``
================
This refers to the client that is used for running a node.

The general synopsis is as follows:

.. code-block:: console

   Usage: bcc-node run [--topology FILEPATH] [--database-path FILEPATH]
                           [--socket-path FILEPATH]
                           [--cole-delegation-certificate FILEPATH]
                           [--cole-signing-key FILEPATH]
                           [--sophie-kes-key FILEPATH]
                           [--sophie-vrf-key FILEPATH]
                           [--sophie-operational-certificate FILEPATH]
                           [--host-addr IPV4-ADDRESS]
                           [--host-ipv6-addr IPV6-ADDRESS]
                           [--port PORT]
                           [--config NODE-CONFIGURATION] [--validate-db]
     Run the node.

* ``--topology`` - Filepath to a topology file describing which peers the node should connect to.

* ``--database-path`` - Path to the blockchain database.

* ``--cole-delegation-certificate`` - Optional path to the Cole delegation certificate. The delegation certificate allows the delegator (the issuer of said certificate) to give his/her own block signing rights to somebody else (the delegatee). The delegatee can then sign blocks on behalf of the delegator.

* ``--cole-signing-key`` - Optional path to the Cole signing key.

* ``--sophie-signing-key`` - Optional path to the Sophie signing key.

* ``--sophie-kes-key`` - Optional path to the Sophie KES signing key.

* ``--sophie-vrf-key`` - Optional path to the Sophie VRF signing key.

* ``--sophie-operational-certificate`` - Optional path to the Sophie operational certificate.

* ``--socket-path`` - Path to the socket file.

* ``--host-addr`` - Optionally specify your node's IPv4 address.

* ``--host-ipv6-addr`` - Optionally specify your node's IPv6 address.

* ``--port`` - Specify which port to assign to the node.

* ``--config`` - Specify the filepath to the config ``.yaml`` file. This file is responsible for all the other node's required settings. See examples in ``configuration`` (e.g. `config-0.yaml <configuration/defaults/simpleview/config-0.yaml>`_).

* ``--validate-db`` - Flag to revalidate all on-disk database files

Configuration ``.yaml`` files
=============================

The ``--config`` flag points to a ``.yaml`` file that is responsible to configuring the logging & other important settings for the node. E.g. see the Cole mainnet configuration in this
`configuration.yaml <https://github.com/The-Blockchain-Company/bcc-node/blob/master/configuration/defaults/cole-mainnet/configuration.yaml>`_.
Some of the more important settings are as follows:

* ``Protocol: RealPBFT`` -- Protocol the node will execute

* ``RequiresNetworkMagic``: RequiresNoMagic -- Used to distinguish between mainnet (``RequiresNoMagic``) and testnets (``RequiresMagic``)


Logging
========

Logs are output to the ``logs/`` dir.

Profiling & statistics
======================

Profiling data and RTS run stats are stored in the ``profile/`` dir.

Please see ``scripts/README.md`` for how to obtain profiling information using the scripts.

Scripts
=======

Please see ``scripts/README.md`` for information on the various scripts.

``bcc-cli``
===============

A CLI utility to support a variety of key material operations (genesis, migration, pretty-printing..) for different system generations.
Usage documentation can be found at ``bcc-cli/README.md``.

The general synopsis is as follows:

.. code-block:: console

   Usage: bcc-cli (Era based commands | Cole specific commands | Miscellaneous commands)

> NOTE: the exact invocation command depends on the environment.  If you have only built ``bcc-cli``, without installing it, then you have to prepend ``cabal run -- ``
before ``bcc-cli``.  We henceforth assume that the necessary environment-specific adjustment has been made, so we only mention ``bcc-cli``.

The subcommands are subdivided in groups, and their full list can be seen in the output of ``bcc-cli --help``.

All subcommands have help available.  For example:

.. code-block:: console

   cabal run -- bcc-cli -- cole key migrate-delegate-key-from --help

   bcc-cli -- cole key migrate-delegate-key-from
   Usage: bcc-cli cole key migrate-delegate-key-from --from FILEPATH
                                                          --to FILEPATH
     Migrate a delegate key from an older version.


   Available options:
     --cole-legacy-formats   Cole/bcc-sl formats and compatibility
     --cole-formats          Cole era formats and compatibility
     --from FILEPATH          Signing key file to migrate.
     --to FILEPATH            Non-existent file to write the signing key to.
     -h,--help                Show this help text


Genesis operations
==================

Generation
----------

The Cole genesis generation operations will create a directory that contains:

* ``genesis.json``:
  The genesis JSON file itself.

* ``avvm-seed.*.seed``:
  Bcc Voucher Vending Machine seeds (secret). Affected by ``--avvm-entry-count`` and ``--avvm-entry-balance``.

* ``delegate-keys.*.key``:
  Delegate private keys. Affected by: ``--n-delegate-addresses``.

* ``delegation-cert.*.json``:
  Delegation certificates. Affected by: ``--n-delegate-addresses``.

* ``genesis-keys.*.key``:
  Genesis stake private keys. Affected by: ``--n-delegate-addresses``, ``--total-balance``.

* ``poor-keys.*.key``:
  Non-delegate private keys with genesis UTxO. Affected by: ``--n-poor-addresses``, ``--total-balance``.

More details on the Cole Genesis ``JSON`` file can be found in ``docs/reference/cole-genesis.md``

 Cole genesis delegation and related concepts are described in detail in:

  `<https://hydra.tbco.io/job/Bcc/bcc-ledger-specs/coleLedgerSpec/latest/download-by-type/doc-pdf/ledger-spec>`_

The canned ``scripts/benchmarking/genesis.sh`` example provides a nice set of defaults and
illustrates available options.

Key operations
==============

Note that key operations do not support password-protected keys.

Signing key generation & verification key extraction
----------------------------------------------------

Signing keys can be generated using the ``keygen`` subcommand.

Extracting a verification key out of the signing key is performed by the ``to-verification`` subcommand.

Delegate key migration
----------------------

In order to continue using a delegate key from the Cole Legacy era in the new implementation,
it needs to be migrated over, which is done by the ``migrate-delegate-key-from`` subcommand:

.. code-block:: console

  $ cabal v2-run -- bcc-cli cole key migrate-delegate-key-from
          --from key0.sk --to key0Converted.sk

Signing key queries
-------------------

One can gather information about a signing key's properties through the ``signing-key-public``
and ``signing-key-address`` subcommands (the latter requires the network magic):

.. code-block:: console

   $ cabal v2-run -- bcc-cli cole key signing-key-public --cole-formats --secret key0.sk

   public key hash: a2b1af0df8ca764876a45608fae36cf04400ed9f413de2e37d92ce04
   public key: sc4pa1pAriXO7IzMpByKo4cG90HCFD465Iad284uDYz06dHCqBwMHRukReQ90+TA/vQpj4L1YNaLHI7DS0Z2Vg==

   $ cabal v2-run -- bcc-cli signing-key-address --cole-formats --secret key0.pbft --testnet-magic 42

   2cWKMJemoBakxhXgZSsMteLP9TUvz7owHyEYbUDwKRLsw2UGDrG93gPqmpv1D9ohWNddx
   VerKey address with root e5a3807d99a1807c3f161a1558bcbc45de8392e049682df01809c488, attributes: AddrAttributes { derivation path: {} }

Transactions
============

Creation
--------

Transactions can be created via the  ``issue-genesis-utxo-expenditure`` & ``issue-utxo-expenditure`` commands.

The easiest way to create a transaction is via the ``scripts/benchmarking/issue-genesis-utxo-expenditure.sh`` script as follows:

``./scripts/benchmarking/issue-genesis-utxo-expenditure.sh transaction_file``

NB: This by default creates a transaction based on ``configuration/defaults/liveview/config-0.yaml``

If you do not have a ``genesis_file`` you can run ``scripts/benchmarking/genesis.sh`` which will create an example ``genesis_file`` for you.
The script ``scripts/benchmarking/issue-genesis-utxo-expenditure.sh`` has defaults for all the requirements of the ``issue-genesis-utxo-expenditure`` command.

Submission
----------

The ``submit-tx`` subcommand provides the option of submitting a pre-signed
transaction, in its raw wire format (see GenTx for Cole transactions).

The canned ``scripts/benchmarking/submit-tx.sh`` script will submit the supplied transaction to a testnet
launched by ``scripts/benchmarking/sophie-testnet-liveview.sh`` script.

Issuing UTxO expenditure (genesis and regular)
----------------------------------------------

To make a transaction spending UTxO, you can either use the:

  - ``issue-genesis-utxo-expenditure``, for genesis UTxO
  - ``issue-utxo-expenditure``, for normal UTxO

subcommands directly, or, again use canned scripts that will make transactions tailored
for the aforementioned testnet cluster:

  - ``scripts/benchmarking/issue-genesis-utxo-expenditure.sh``.
  - ``scripts/benchmarking/issue-utxo-expenditure.sh``.

The script requires the target file name to write the transaction to, input TxId
(for normal UTxO), and optionally allows specifying the source txin output index,
source and target signing keys and entropic value to send.

The target address defaults to the 1-st richman key (``configuration/delegate-keys.001.key``)
of the testnet, and entropic amount is almost the entirety of its funds.

Local node queries
==================

You can query the tip of your local node via the ``get-tip`` command as follows

1. Open `tmux`
2. Run ``cabal build bcc-node``
3. Run ``./scripts/lite/sophie-testnet.sh example``
4. Run ``export BCC_NODE_SOCKET_PATH=/bcc-node/example/socket/node-1-socket
4. ``cabal exec bcc-cli -- get-tip --testnet-magic 42``

You will see output from stdout in this format:

.. code-block:: console

   Current tip:
   Block hash: 4ab21a10e1b25e39
   Slot: 6
   Block number: 5

Update proposals
================

Update proposal creation
------------------------

A Cole update proposal can be created as follows:

.. code-block:: console

   bcc-cli -- cole governance
                  create-update-proposal
                    (--mainnet | --testnet-magic NATURAL)
                    --signing-key FILEPATH
                    --protocol-version-major WORD16
                    --protocol-version-sentry WORD16
                    --application-name STRING
                    --software-version-num WORD32
                    --system-tag STRING
                    --installer-hash HASH
                    --filepath FILEPATH
                  ..

The mandatory arguments are ``--mainnet | --testnet-magic``, ``signing-key``, ``protocol-version-major``, ``protocol-version-sentry``, ``application-name``, ``software-version-num``, ``system-tag``, ``installer-hash`` and ``filepath``.

The remaining arguments are optional parameters you want to update in your update proposal.

You can also check your proposal's validity using the `validate-cbor` command. See: `Validate CBOR files`_.

See the `Cole specification <https://hydra.tbco.io/job/Bcc/bcc-ledger-specs/coleLedgerSpec/latest/download-by-type/doc-pdf/ledger-spec>`_
for more details on update proposals.

Update proposal submission
--------------------------

You can submit your proposal using the ``submit-update-proposal`` command.

Example:

.. code-block:: console

   bcc-cli -- cole governance
               submit-update-proposal
               --config configuration/defaults/mainnet/configuration.yaml
               (--mainnet | --testnet-magic NATURAL)
               --filepath my-update-proposal

See the `Cole specification <https://hydra.tbco.io/job/Bcc/bcc-ledger-specs/coleLedgerSpec/latest/download-by-type/doc-pdf/ledger-spec>`_
for more deatils on update proposals.

Update proposal voting
======================

You can create and submit cole update proposal votes with the ``create-proposal-vote`` & ``submit-proposal-vote`` commands. The following are two example commands:


Cole vote creation:

.. code-block:: console

   cabal exec bcc-cli -- cole governance create-proposal-vote
                          (--mainnet | --testnet-magic NATURAL)
                          --signing-key configuration/defaults/liveview/genesis/delegate-keys.000.key
                          --proposal-filepath ProtocolUpdateProposalFile
                          --vote-yes
                          --output-filepath UpdateProposalVoteFile

Cole vote submission:

.. code-block:: console

   cabal exec bcc-cli -- cole governance submit-proposal-vote
                          (--mainnet | --testnet-magic NATURAL)
                          --filepath UpdateProposalVoteFile

Development
===========

GHCID
-----

run *ghcid* with: ``ghcid -c "cabal repl exe:bcc-node --reorder-goals"``

Haskell Language Server
-----------------------

When using Haskell Langague Server with Visual Studio Code, you may find that
`HLINT annotations are ignored<https://github.com/haskell/haskell-language-server/issues/638>`.

To work around this, you may run the script `./scripts/reconfigure-hlint.sh` to generate a `.hlint.yaml`
file with HLINT ignore rules derived from the source code.

Testing
========

``bcc-node`` is essentially a container which implements several components such networking, consensus, and storage. These components have individual test coverage. The node goes through integration and release testing by Devops/QA while automated CLI tests are ongoing alongside development.

Developers on ``bcc-node`` can `launch their own testnets <doc/getting-started/launching-a-testnet.md>`_ or `run the chairman tests <doc/getting-started/running-chairman-tests.md>`_ locally.

Chairman tests
--------------

Debugging
=========

Pretty printing CBOR encoded files
----------------------------------

It may be useful to print the on chain representations of blocks, delegation certificates, txs and update proposals. There are two commands that do this (for any cbor encoded file):

To pretty print as CBOR:
``cabal exec bcc-cli -- pretty-print-cbor --filepath CBOREncodedFile``

Validate CBOR files
-------------------

You can validate Cole era blocks, delegation certificates, txs and update proposals with the ``validate-cbor`` command.

``cabal exec bcc-cli -- validate-cbor --cole-block 21600 --filepath CBOREncodedColeBlockFile``


Native Tokens
=======================================

Native tokens is a new feature that enables the transacting of multi-assets on Bcc. Native tokens are now supported on mainnet and users can transact with bcc, and an unlimited number of user-defined (custom) tokens natively.

To help you get started we have compiled a handy list of resources:  #TODO

`Bcc Forum discussion <https://forum.bcc.org/c/developers/bcc-tokens/150>`_

`Documentation for native tokens <https://docs.bcc.org/native-tokens/learn>`_

You can also read more about `native tokens and how they compare to bcc and ERC20 <https://github.com/The-Blockchain-Company/bcc-ledger-specs/blob/master/doc/explanations/features.rst>`_. Browse native tokens created on the Bcc blockchain and see their transactions in an interactive dashboard that allows filtering and searching: nativetokens.da.iogservices.io.
