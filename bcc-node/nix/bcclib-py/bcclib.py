"""Bcc Python Library."""
import functools
import json
import subprocess
import os
from copy import copy
from pathlib import Path
from time import sleep
from contextlib import contextmanager

@contextmanager
def cd(path):
    if not os.path.exists(path):
        os.mkdir(path, 0o700)
    old_dir = os.getcwd()
    os.chdir(path)
    try:
        yield
    finally:
        os.chdir(old_dir)


class BccCLIError(Exception):
    pass

class BccWrongEraError(Exception):
    pass

class BccCluster:
    """Bcc Cluster Tools."""

    def __init__(self, network_magic, state_dir, num_delegs, protocol="bcc"):
        self.cli = BccCLIWrapper(network_magic, state_dir, protocol=protocol)
        if(protocol == "bcc" or protocol == "cole"):
            self.cole_delegate_keys = list(map(lambda x: self.cli.state_dir / "cole" / f"delegate-keys.{x:03}.key", list(range(0,num_delegs))))
        if(protocol == "bcc" or protocol == "sophie"):
            self.sophie_delegate_skeys = list(map(lambda x: self.cli.state_dir / "sophie" / "delegate-keys" / f"delegate{x}.skey", list(range(1,num_delegs+1))))
            self.sophie_genesis_vkeys = list(map(lambda x: self.cli.state_dir / "sophie" / "genesis-keys" / f"genesis{x}.vkey", list(range(1,num_delegs+1))))
            if os.path.exists( self.cli.state_dir / "sophie" / "genesis-utxo.skey"):
                self.genesis_utxo_vkey = self.cli.state_dir / "sophie" / "genesis-utxo.vkey"
                self.genesis_utxo_skey = self.cli.state_dir / "sophie" / "genesis-utxo.skey"
                self.genesis_utxo_addr = self.cli.get_genesis_addr(self.genesis_utxo_vkey)

    # We're not adding methods to CLIWrapper for these complex cole commands
    def hard_fork_cole(self, version):
        version_params = version.split(".")
        proposal_path = self.cli.state_dir / f"{version}.proposal"
        self.cli.cmd(["bcc-cli", "cole", "create-update-proposal", "--testnet-magic", str(self.cli.network_magic), "--signing-key", self.cole_delegate_keys[0], "--protocol-version-major", version_params[0], "--protocol-version-sentry", version_params[1], "--application-name", "bcc-sl", "--software-version-num", "1", "--system-tag", "linux", "--installer-hash", "0", "--filepath", proposal_path])
        self.cli.cmd(["bcc-cli", "cole", "submit-update-proposal", "--testnet-magic", str(self.cli.network_magic), "--filepath", proposal_path])
        for index,key in enumerate(self.cole_delegate_keys, start=1):
            proposal_vote_path = f"{proposal_path}-vote{index}"
            self.cli.cmd(["bcc-cli", "cole", "create-proposal-vote", "--proposal-filepath", proposal_path, "--testnet-magic", str(self.cli.network_magic), "--signing-key", key, "--vote-yes", "--output-filepath", proposal_vote_path])
            self.cli.cmd(["bcc-cli", "cole", "submit-proposal-vote", "--testnet-magic", str(self.cli.network_magic), "--filepath", proposal_vote_path])
        self.update_config_version(version_params)
        for i in range(len(self.cole_delegate_keys)):
            self.restart_node(f"bft{i+1}")
            sleep(1)
        # TODO: wait until new epoch where sophie starts
        if version == "2.0.0":
            self.cli.current_protocol = "sophie"

    # only works with sophie
    def sleep_until_next_epoch(self):
        tip = self.cli.get_tip()
        slot = tip["slotNo"]
        epoch_length = self.cli.epoch_length
        slots_remaining = epoch_length - (slot % epoch_length)
        sleep(self.cli.slot_length * slots_remaining)

    def restart_node(self, node):
        self.cli.cmd(["supervisorctl", "restart", node])

    def update_config_version(self, version_params, config_file="config.json"):
        config_file = self.cli.state_dir / config_file
        with open(config_file) as in_json:
            config = json.load(in_json)
        config["LastKnownBlockVersion-Major"] = int(version_params[0])
        config["LastKnownBlockVersion-Sentry"] = int(version_params[1])
        config["LastKnownBlockVersion-Alt"] = int(version_params[2])
        with open(config_file, "w") as out_json:
            json.dump(config, out_json)

    def send_tx_genesis(
        self, txouts=None, certificates=None, signing_keys=None, proposal_file=None,
    ):
        txouts = txouts or []
        certificates = certificates or []
        signing_keys = signing_keys or []

        utxo = self.cli.get_utxo(address=self.genesis_utxo_addr)
        total_input_amount = 0
        txins = []
        for k, v in utxo.items():
            total_input_amount += v["amount"]
            txin = k.split("#")
            txin = (txin[0], txin[1])
            txins.append(txin)

        signing_keys.append(str(self.genesis_utxo_skey))
        utxo = self.cli.get_utxo(address=self.genesis_utxo_addr)
        txins = []
        for k, v in utxo.items():
            txin = k.split("#")
            txin = (txin[0], txin[1], v["amount"])
            txins.append(txin)
        txout_total = sum(map(lambda x: x[1], txouts))

        # Build, Sign and Send TX to chain
        try:
            self.cli.build_tx(
                txins=txins,
                txouts=txouts + [(self.genesis_utxo_addr, 0)],
                certificates=certificates,
                proposal_file=proposal_file,
            )
            fee = self.cli.estimate_fee(len(txins), len(txouts), len(signing_keys))
            ttl = self.cli.get_tip()["slotNo"] + 5000
            self.cli.build_tx(
                txins=txins,
                txouts=txouts + [(self.genesis_utxo_addr, total_input_amount - fee - txout_total)],
                certificates=certificates,
                fee=fee,
                ttl=ttl,
                proposal_file=proposal_file,
            )
            self.cli.sign_tx(signing_keys=signing_keys)
            self.cli.submit_tx()
        except BccCLIError as err:
            raise BccCLIError(
                f"Sending a genesis transaction failed!\n"
                f"utxo: {utxo}\n"
                f"txins: {txins} txouts: {txouts} signing keys: {signing_keys}\n{err}"
            )

    def submit_update_proposal(self, proposal_opts):
        # TODO: assumption is update proposals submitted near beginning of epoch
        epoch = self.cli.get_tip()["slotNo"] // self.cli.epoch_length

        self.cli.create_update_proposal(proposal_opts, self.sophie_genesis_vkeys, epoch=epoch)
        self.send_tx_genesis(signing_keys=self.sophie_delegate_skeys, proposal_file="update.proposal")

    # TODO:
    def cole_generate_tx_slice(self, start, slice_size, tx_filename, snapshot, wallet, fee):
        s = slice(start, start+slice_size)
        records = snapshot[s]
        txouts = list(map( lambda x: getTxOut(x), records))
        txouts_total = sum(map(lambda x: x["value"], records))

        wallet["value"] = wallet["value"] - txouts_total - fee
        txout_args = self.cli.prepend_flag("--txout", txouts)
        self.cli.cmd(["bcc-cli", "issue-utxo-expenditure", "--tx", tx_filename, "--testnet-magic", str(self.networkMagic), "--txin", getTxIn(current_tx), "--wallet-key", str(wallet["key"]), "--txout", getTxOut(wallet) ] + txout_args)
        return wallet

class BccCLIWrapper:
    """Bcc CLI Wrapper."""

    def __init__(self, network_magic, state_dir, sophie_keys="sophie", cole_keys="cole", protocol="sophie"):
        self.network_magic = network_magic

        self.state_dir = Path(state_dir).expanduser().absolute()
        self.sophie_genesis_json = self.state_dir / sophie_keys / "genesis.json"
        self.cole_genesis_json = self.state_dir / cole_keys / "genesis.json"
        self.pparams_file = self.state_dir / "pparams.json"

        self.check_state_dir()


        self.pparams = None
        self.current_protocol = protocol
        if protocol == "sophie":
            with open(self.sophie_genesis_json) as in_json:
                self.sophie_genesis = json.load(in_json)
            self.refresh_pparams()
            self.slot_length = self.sophie_genesis["slotLength"]
            self.epoch_length = self.sophie_genesis["epochLength"]
        elif protocol == "cole":
            with open(self.cole_genesis_json) as in_json:
                self.cole_genesis = json.load(in_json)

        elif protocol == "bcc":
            # TODO: check if cole or sophie
            self.current_protocol = "cole"
            with open(self.sophie_genesis_json) as in_json:
                self.sophie_genesis = json.load(in_json)
            with open(self.cole_genesis_json) as in_json:
                self.cole_genesis = json.load(in_json)


    def check_state_dir(self):
        if not self.state_dir.exists():
            raise BccCLIError(f"The state dir `{self.state_dir}` doesn't exist.")

        for file_name in ([
            self.sophie_genesis_json
        ]):
            if not file_name.exists():
                raise BccCLIError(f"The file `{file_name}` doesn't exist.")

    @staticmethod
    def cmd(cli_args):
        p = subprocess.Popen(cli_args, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        stdout, stderr = p.communicate()
        if p.returncode != 0:
            raise BccCLIError(f"An error occurred running a CLI command `{p.args}`: {stderr}")
        return stdout

    @staticmethod
    def prepend_flag(flag, contents):
        return sum(([flag, x] for x in contents), [])

    def query_cli(self, cli_args):
        return self.cmd(
            [
                "bcc-cli",
                "sophie",
                "query",
                *cli_args,
                "--testnet-magic",
                str(self.network_magic),
            ]
        )


    def refresh_pparams(self):
        if self.current_protocol == "cole":
            raise BccWrongEraError("Attempted to run sophie command in cole era")
        self.query_cli(["protocol-parameters", "--out-file", str(self.pparams_file)])
        with open(self.pparams_file) as in_json:
            self.pparams = json.load(in_json)

    def estimate_fee(self, txins=1, txouts=1, witnesses=1, cole_witnesses=0, txbody_file="tx.body"):
        self.refresh_pparams()
        stdout = self.cmd(
            [
                "bcc-cli",
                "sophie",
                "transaction",
                "calculate-min-fee",
                "--testnet-magic",
                str(self.network_magic),
                "--protocol-params-file",
                str(self.pparams_file),
                "--tx-in-count",
                str(txins),
                "--tx-out-count",
                str(txouts),
                "--cole-witness-count",
                str(cole_witnesses),
                "--witness-count",
                str(witnesses),
                "--tx-body-file",
                str(txbody_file),
            ]
        )
        fee, __ = stdout.decode().split(" ")
        return int(fee)

    def build_tx(
        self,
        out_file="tx.body",
        txins=None,
        txouts=None,
        certificates=None,
        fee=0,
        ttl=None,
        proposal_file=None,
    ):
        if ttl == None:
            ttl = self.get_tip()["slotNo"] + 5000
        txins = txins or []
        txouts_copy = copy(txouts) if txouts else []
        certificates = certificates or []

        txins_combined = [f"{x[0]}#{x[1]}" for x in txins]
        txouts_combined = [f"{x[0]}+{x[1]}" for x in txouts_copy]

        txin_args = self.prepend_flag("--tx-in", txins_combined)
        txout_args = self.prepend_flag("--tx-out", txouts_combined)
        cert_args = self.prepend_flag("--certificate-file", certificates)

        build_args = [
            "bcc-cli",
            "sophie",
            "transaction",
            "build-raw",
            "--invalid-hereafter",
            str(ttl),
            "--fee",
            str(fee),
            "--out-file",
            str(out_file),
            *txin_args,
            *txout_args,
            *cert_args,
        ]

        if proposal_file:
            build_args.extend(["--update-proposal-file", proposal_file])

        self.cmd(build_args)

    def sign_tx(self, tx_body_file="tx.body", out_file="tx.signed", signing_keys=None):
        signing_keys = signing_keys or []
        key_args = self.prepend_flag("--signing-key-file", signing_keys)
        self.cmd(
            [
                "bcc-cli",
                "sophie",
                "transaction",
                "sign",
                "--tx-body-file",
                str(tx_body_file),
                "--out-file",
                str(out_file),
                "--testnet-magic",
                str(self.network_magic),
                *key_args,
            ]
        )

    def submit_tx(self, tx_file="tx.signed"):
        if self.current_protocol == "cole":
            raise BccWrongEraError("Attempted to run sophie command in cole era")
        self.cmd(
            [
                "bcc-cli",
                "sophie",
                "transaction",
                "submit",
                "--testnet-magic",
                str(self.network_magic),
                "--tx-file",
                str(tx_file),
            ]
        )

    def get_payment_address(self, payment_vkey, stake_vkey=None):
        if not payment_vkey:
            raise BccCLIError("Must set payment key.")

        cli_args = ["--payment-verification-key-file", str(payment_vkey)]
        if stake_vkey:
            cli_args.extend(["--stake-verification-key-file", str(stake_vkey)])

        return (
            self.cmd(
                [
                    "bcc-cli",
                    "sophie",
                    "address",
                    "build",
                    "--testnet-magic",
                    str(self.network_magic),
                    *cli_args,
                ]
            )
            .rstrip()
            .decode("ascii")
        )

    def get_genesis_addr(self, vkey_path):
        return (
            self.cmd(
                [
                    "bcc-cli",
                    "sophie",
                    "genesis",
                    "initial-addr",
                    "--testnet-magic",
                    str(self.network_magic),
                    "--verification-key-file",
                    str(vkey_path),
                ]
            )
            .rstrip()
            .decode("ascii")
        )

    def get_utxo(self, address):
        if self.current_protocol == "cole":
            raise BccWrongEraError("Attempted to run sophie command in cole era")
        self.query_cli(["utxo", "--address", address, "--out-file", "utxo.json"])
        with open("utxo.json") as in_json:
            utxo = json.load(in_json)
        return utxo

    def get_tip(self):
        return json.loads(self.query_cli(["tip"]))


    def create_update_proposal(self, proposal_opts, genesis_keys, epoch=1, file_name="update.proposal"):
        genesis_key_args = self.prepend_flag("--genesis-verification-key-file", genesis_keys)
        proposal_args = sum(list(map(lambda x: [ f"--{x[0]}", str(x[1]) ], proposal_opts)), [])
        self.cmd(
            [
                "bcc-cli",
                "sophie",
                "governance",
                "create-update-proposal",
                "--out-file",
                file_name,
                "--epoch",
                str(epoch),
                *proposal_args,
                *genesis_key_args
            ]
        )
    def create_utxo(self, name):
        self.cmd(["bcc-cli",
                  "sophie",
                  "address",
                  "key-gen",
                  "--verification-key-file",
                  f"{name}.vkey",
                  "--signing-key-file",
                  f"{name}.skey"
                ])

    def create_stake_address_and_cert(self, name):
        self.cmd(["bcc-cli",
                  "sophie",
                  "stake-address",
                  "key-gen",
                  "--verification-key-file",
                  f"{name}.vkey",
                  "--signing-key-file",
                  f"{name}.skey"
                ])
        self.cmd(["bcc-cli",
                  "sophie",
                  "stake-address",
                  "registration-certificate",
                  "--stake-verification-key-file",
                  f"{name}.vkey",
                  "--out-file",
                  f"{name}.cert"
                ])


    def create_cold_key(self, name):
        self.cmd(["bcc-cli",
                  "sophie",
                  "node",
                  "key-gen",
                  "--cold-verification-key-file",
                  f"{name}.vkey",
                  "--cold-signing-key-file",
                  f"{name}.skey",
                  "--operational-certificate-issue-counter-file",
                  f"{name}.counter"
                ])

    def create_vrf_key(self, name):
        self.cmd(["bcc-cli",
                  "sophie",
                  "node",
                  "key-gen-VRF",
                  "--verification-key-file",
                  f"{name}.vkey",
                  "--signing-key-file",
                  f"{name}.skey"
                ])

    def create_stake_pool(self, prefix, owner_vkey, pledge, cost, margin, relay_dns, relay_port, metadata_url, metadata_hash):
        self.cmd(["bcc-cli",
                  "sophie",
                  "stake-pool",
                  "registration-certificate",
                  "--cold-verification-key-file",
                  f"{prefix}-cold.vkey",
                  "--vrf-verification-key-file",
                  f"{prefix}-vrf.vkey",
                  "--pool-pledge",
                  str(pledge),
                  "--pool-cost",
                  str(cost),
                  "--pool-margin",
                  str(margin),
                  "--pool-reward-account-verification-key-file",
                  f"{prefix}-reward.vkey",
                  "--pool-owner-stake-verification-key-file",
                  owner_vkey,
                  "--single-host-pool-relay",
                  relay_dns,
                  "--pool-relay-port",
                  str(relay_port),
                  "--metadata-url",
                  metadata_url,
                  "--metadata-hash",
                  metadata_hash,
                  "--out-file",
                  f"{prefix}.cert",
                  "--testnet-magic",
                  str(self.network_magic)
                ])

    def create_metadata_file(self, name, file_name, description, ticker, homepage):
        contents = {
                "name": name,
                "description": description,
                "ticker": ticker,
                "homepage": homepage
        }
        with open(file_name, "w") as out_json:
            json.dump(contents, out_json)
        return self.cmd(["bcc-cli",
                  "sophie",
                  "stake-pool",
                  "metadata-hash",
                  "--pool-metadata-file",
                  file_name
                ]).rstrip()

    def create_delegation(self, stake, pool, file_name):
        self.cmd(["bcc-cli",
                  "sophie",
                  "stake-address",
                  "delegation-certificate",
                  "--stake-verification-key-file",
                  stake,
                  "--cold-verification-key-file",
                  pool,
                  "--out-file",
                  file_name
                ])

    # Creates owners, keys, pool registration and owner delegation, then submits all in a transaction
    def register_stake_pool(self, name, ticker, description, homepage, pledge, cost, margin, index, relay_dns, relay_port, metadata_url, prefix="node", payment_key="utxo", directory="pools"):
        self.refresh_pparams()
        prev_dir = os.getcwd()
        with cd(directory):
            # create owner keys and stake registration certificate
            p = f"{prefix}{index}"
            if os.path.exists(f"{p}.cert"):
                raise BccCLIError("Pool already exists! Aborting!")
            self.create_utxo(f"{p}-owner-utxo")
            self.create_stake_address_and_cert(f"{p}-owner-stake")
            self.create_stake_address_and_cert(f"{p}-reward")
            self.create_cold_key(f"{p}-cold")
            self.create_vrf_key(f"{p}-vrf")
            metadata_hash = self.create_metadata_file(name, f"{p}-metadata.json", description, ticker, homepage)
            self.create_stake_pool(p, f"{p}-owner-stake.vkey", pledge, cost, margin, relay_dns, relay_port, metadata_url, metadata_hash)
            self.create_delegation(f"{p}-owner-stake.vkey", f"{p}-cold.vkey", f"{p}-owner-delegation.cert")
            source_payment_address = self.get_payment_address(f"{prev_dir}/{payment_key}.vkey")
            source_payment_utxo = self.get_utxo(source_payment_address)
            # TODO: function for this???
            txins = []
            total_input_amount = 0
            for k, v in source_payment_utxo.items():
                total_input_amount += v["amount"]
                txin = k.split("#")
                txin = (txin[0], txin[1])
                txins.append(txin)

            owner_payment_address = self.get_payment_address(f"{p}-owner-utxo.vkey", f"{p}-owner-stake.vkey")

            txouts = [(source_payment_address, total_input_amount), (owner_payment_address, pledge)]
            deposits = self.pparams["poolDeposit"] + (2 * self.pparams["keyDeposit"])
            signing_keys = [ f"{prev_dir}/{payment_key}.skey", f"{p}-owner-stake.skey", f"{p}-cold.skey", f"{p}-reward.skey" ]
            certificates = [ f"{p}-owner-stake.cert", f"{p}-reward.cert", f"{p}.cert", f"{p}-owner-delegation.cert" ]
            self.build_tx(f"tx-register-{p}.txbody", txins, txouts, certificates)
            fee = self.estimate_fee(len(txins), len(txouts), len(signing_keys), txbody_file=f"tx-register-{p}.txbody")
            txouts = [(source_payment_address, total_input_amount - pledge - fee - deposits ), (owner_payment_address, pledge)]
            self.build_tx(f"tx-register-{p}.txbody", txins, txouts, certificates, fee)
            self.sign_tx(f"tx-register-{p}.txbody", f"tx-register-{p}.txsigned", signing_keys)
            self.submit_tx(f"tx-register-{p}.txsigned")

    def convert_itn_vkey(self, key):
        with open(f"{key}.pk", "w") as fname:
            fname.write(k)
        self.cmd(["bcc-cli", "stake-address", "convert-itn-key", "--itn-verification-key-file", f"{key}.pk", "--out-file", f"{key}.vkey"])

    def convert_itn_skey(self, key):
        with open(f"{key}.sk", "w") as fname:
            fname.write(k)
        self.cmd(["bcc-cli", "stake-address", "convert-itn-key", "--itn-signing-key-file", f"{key}.sk", "--out-file", f"{key}.skey"])

    # Cole specific commands
    def cole_txin_format(self, tx):
        return "(\"{}\",{})".format(tx[0],tx[1])

    def cole_txout_format(self, fund):
        return "(\"{}\",{})".format(fund["address"], fund["value"])


