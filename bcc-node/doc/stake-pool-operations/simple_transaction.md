# Create a simple transaction

Creating a transaction requires various steps:

* Get the protocol parameters
* Calculate the fee
* Define the time-to-live (TTL) for the transaction
* Build the transaction
* Sign the transaction
* Submit the transaction

#### Get protocol parameters

Get the protocol parameters and save them to `protocol.json` with:

```
bcc-cli query protocol-parameters \
  --mainnet \
  --out-file protocol.json
```

#### Get the transaction hash and index of the **UTXO** to spend:

```
bcc-cli query utxo \
  --address $(cat payment.addr) \
  --mainnet
```

```
                            TxHash                                 TxIx        Amount
----------------------------------------------------------------------------------------
4e3a6e7fdcb0d0efa17bf79c13aed2b4cb9baf37fb1aa2e39553d5bd720c5c99     4         20000000 entropic
```

#### Draft the transaction

Create a draft for the transaction and save it in tx.draft

Note that for `--tx-in` we use the following syntax: `TxHash#TxIx` where `TxHash` is the transaction hash and `TxIx` is the index; for `--tx-out` we use: `TxOut+Entropic` where `TxOut` is the hex encoded address followed by the amount in `Entropic`. For the transaction draft --tx-out, --invalid-hereafter and --fee can be set to zero.

    bcc-cli transaction build-raw \
    --tx-in 4e3a6e7fdcb0d0efa17bf79c13aed2b4cb9baf37fb1aa2e39553d5bd720c5c99#4 \
    --tx-out $(cat payment2.addr)+0 \
    --tx-out $(cat payment.addr)+0 \
    --invalid-hereafter 0 \
    --fee 0 \
    --out-file tx.draft

#### Calculate the fee

A simple transaction needs one input, a valid UTXO from `payment.addr`, and two outputs:

* Output1: The address that receives the transaction.
* Output2: The address that receives the change of the transaction.

Note that to calculate the fee you need to include the draft transaction

    bcc-cli transaction calculate-min-fee \
    --tx-body-file tx.draft \
    --tx-in-count 1 \
    --tx-out-count 2 \
    --witness-count 1 \
    --cole-witness-count 0 \
    --mainnet \
    --protocol-params-file protocol.json

    > 167965

#### Calculate the change to send back to payment.addr,
all amounts must be in Entropic:

    expr <UTXO BALANCE> - <AMOUNT TO SEND> - <TRANSACTION FEE>

For example, if we send 10 BCC from a UTxO containing 20 BCC, the change to send back to `payment.addr` after paying the fee is: 9.832035 BCC

    expr 20000000 - 10000000 - 167965

    > 9832035

#### Determine the TTL (time to Live) for the transaction

To build the transaction we need to specify the **TTL (Time to live)**, this is the slot height limit for our transaction to be included in a block, if it is not in a block by that slot the transaction will be cancelled. So TTL = slot + N slots. Where N is the amount of slots you want to add to give the transaction a window to be included in a block.

Query the tip of the blockchain:

    bcc-cli query tip --mainnet

Look for the value of `slot`

    {
        "epoch": 259,
        "hash": "dbf5104ab91a7a0b405353ad31760b52b2703098ec17185bdd7ff1800bb61aca",
        "slot": 26633911,
        "block": 5580350
    }

Calculate your `invalid-hereafter`, for example:  26633911 + 200 slots = 26634111

#### Build the transaction

We write the transaction in a file, we will name it `tx.raw`.

    bcc-cli transaction build-raw \
    --tx-in 4e3a6e7fdcb0d0efa17bf79c13aed2b4cb9baf37fb1aa2e39553d5bd720c5c99#4 \
    --tx-out $(cat payment2.addr)+10000000 \
    --tx-out $(cat payment.addr)+9832035 \
    --invalid-hereafter 26634111 \
    --fee 167965 \
    --out-file tx.raw

#### Sign the transaction

Sign the transaction with the signing key **payment.skey** and save the signed transaction in **tx.signed**

    bcc-cli transaction sign \
    --tx-body-file tx.raw \
    --signing-key-file payment.skey \
    --mainnet \
    --out-file tx.signed

#### Submit the transaction

    bcc-cli transaction submit \
    --tx-file tx.signed \
    --mainnet

#### Check the balances

We must give it some time to get incorporated into the blockchain, but eventually, we will see the effect:

    bcc-cli query utxo \
        --address $(cat payment.addr) \
        --mainnet

    >                            TxHash                                 TxIx         Amount
    > ----------------------------------------------------------------------------------------
    > b64ae44e1195b04663ab863b62337e626c65b0c9855a9fbb9ef4458f81a6f5ee     1         9832035 entropic

    bcc-cli query utxo \
        --address $(cat payment2.addr) \
        --mainnet

    >                            TxHash                                 TxIx         Amount
    > ----------------------------------------------------------------------------------------
    > b64ae44e1195b04663ab863b62337e626c65b0c9855a9fbb9ef4458f81a6f5ee     0         10000000 entropic


**Note**`--mainnet` identifies the Bcc mainnet, for testnets use `--testnet-magic 1097911063` instead.
