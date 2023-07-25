# Zerepoch Scripts

## What is a Zerepoch spending script?

This is a type of Zerepoch script that is required to validate the spending of a tx output at its own script address. The tx output at the Zerepoch script address *must* be associated with a datum hash, otherwise, the tx output will be unspendable! The purpose of this datum hash is to encode the state of the contract. A demonstration of this can be seen in [lecture #7](https://youtu.be/oJupInqvJUI) of the Zerepoch Pioneers Program. The Zerepoch spending script expects a datum and a redeemer in order to successfully validate the spending of the tx output at its own script address; note that the redeemer is considered to be the user input. These are supplied in the transaction being submitted along with the Zerepoch script itself and specified transaction execution units.

The transaction execution units are an upper bound or budget of what will be spent to execute the Zerepoch script. If you don't specify a high enough value to cover script execution, your transaction will still be successful (provided it is a valid tx) but you will lose your collateral. The collateral is the transaction input(s) you specify to be consumed if your script fails to execute. There is a protocol parameter `collateralPercent` that determines what percentage of your inputs you must supply as collateral.

*Note that in order to use a tx input as collateral, it **cannot** reside at a script address; it must reside at a ‘normal’ payment address and it cannot contain any multi-assets.*

### An example of using a Zerepoch spending script

Below is an example that shows how to use a Zerepoch spending script. This is a step-by-step
process involving:

+ the creation of the `AlwaysSucceeds` Zerepoch txin script
+ sending BCC to the Zerepoch script address
+ spending BCC at the Zerepoch script address

In this example we will use the [AlwaysSucceeds](../../../zerepoch-example/zerepoch-example/src/Bcc/ZerepochExample/AlwaysSucceeds.hs) Zerepoch spending script. In order to execute a Zerepoch spending script, we require the following:

- Collateral tx input(s) - these are provided and are forfeited in the event the Zerepoch script fails to execute.
- A Zerepoch tx output with accompanying datum hash. This is the tx output that sits at the Zerepoch script address. It must have a datum hash, otherwise, it is unspendable.
- The Zerepoch script serialized in the text envelope format. `bcc-cli` expects Zerepoch scripts to be serialized in the text envelope format.

#### Creating the `AlwaysSucceeds` Zerepoch spending script

The zerepoch-example executable will automagically generate several Zerepoch scripts in the CLI-compatiable text envelope format.

Run the following commands:

```bash
cd zerepoch-example

cabal run exe:zerepoch-example
```

This will output `always-succeeds-txin.zerepoch` in the `generated-zerepoch-scripts` dir.

#### Setting up a local Aurum node cluster

There is a convenient script that will set up an Aurum cluster immediately on your local machine.

Run the following command:

```bash
cabal install bcc-cli
cabal install bcc-node
./scripts/cole-to-aurum/mkfiles.sh aurum
```

Follow the instructions displayed in the terminal to start your Aurum cluster.

#### Sending BCC to the script address

In order to require a Zerepoch script to validate the spending of a tx ouput, we must put the tx output at the script address of the said Zerepoch script. However, before we do that, we must create a datum hash:

```bash
> bcc-cli transaction hash-script-data --script-data-value 42
> 9e1199a988ba72ffd6e9c269cadb3b53b5f360ff99f112d9b2ee30c4d74ad88b
```
In this example, the script we are using always succeeds so we can use any datum hash. We calculate the script address as follows:

```bash
> bcc-cli address build --payment-script-file zerepoch-example/generated-zerepoch-scripts/always-succeeds-txin.zerepoch  --testnet-magic 42
> addr_test1wzeqkp6ne3xm6gz39l874va4ujgl4kr0e46pf3ey8xsu3jsgkpcj2
```

Now, we should create the tx that will send BCC to the script address of our `AlwaysSucceeds` script:

```bash
bcc-cli transaction build-raw \
  --aurum-era \
  --fee 0 \
  --tx-in $txin \
  --tx-out "addr_test1wzeqkp6ne3xm6gz39l874va4ujgl4kr0e46pf3ey8xsu3jsgkpcj2+$entropic" \
  --tx-out-datum-hash 9e1199a988ba72ffd6e9c269cadb3b53b5f360ff99f112d9b2ee30c4d74ad88b \
  --out-file create-datum-output.body

bcc-cli transaction sign \
  --tx-body-file create-datum-output.body \
  --testnet-magic 42 \
  --signing-key-file $UTXO_SKEY \
  --out-file create-datum-output.tx
```

#### Spending BCC at the script address

Now that there is BCC at our script address, we must construct the appropriate transaction in order to spend it.

`$zerepochutxotxin` - This is the tx input that sits at the Zerepoch script address (NB: It has a datum hash).
`$zerepochrequiredtime` and `$zerepochrequiredspace` - These make up the Zerepoch script execution budget and are part of the `$txfee`
`tx-in-redeemer-value` - We must also supply a redeemer value even though the Zerepoch script will succeed regardless of the redeemer.

```bash
bcc-cli transaction build-raw \
  --aurum-era \
  --fee "$txfee" \
  --tx-in $zerepochutxotxin \
  --tx-in-collateral $txinCollateral \
  --tx-out "$dummyaddress+$spendable" \
  --tx-in-script-file $zerepochscriptinuse \
  --tx-in-datum-value 42  \
  --protocol-params-file pparams.json\
  --tx-in-redeemer-value 42 \
  --tx-in-execution-units "($zerepochrequiredtime, $zerepochrequiredspace)" \
  --out-file test-aurum.body

bcc-cli transaction sign \
  --tx-body-file test-aurum.body \
  --testnet-magic 42 \
  --signing-key-file "${UTXO_SKEY}" \
  --out-file aurum.tx
```

If there is BCC at `$dummyaddress` then the Zerepoch script was successfully executed. Conversely, if the Zerepoch script failed, the collateral input would have been consumed.

You can use the [example-txin-locking-zerepoch-script.sh](../../../scripts/zerepoch/example-txin-locking-zerepoch-script.sh) in conjunction with [mkfiles.sh aurum](../../../scripts/cole-to-aurum/mkfiles.sh) script to automagically run the `AlwaysSucceeds` script.

