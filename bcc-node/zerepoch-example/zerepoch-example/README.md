# zerepoch-example

This library demonstrates end to end examples of creating and executing Zerepoch scripts on chain.

This is done roughly in the following steps:

1. Write your Zerepoch **on chain** code.
2. Serialize your Zerepoch on chain code to the text envelope format (`bcc-cli` expects this format).
3. Create your transaction with the accompanying Zerepoch script(s).
4. Submit transaction to execute Zerepoch script.

## FAQ

### Where is the off chain code?

The off chain code is used for transaction construction. In this case we construct the transaction with `bcc-cli` and therefore we don't need to write any off chain code.

### Where can I learn about Zerepoch scripts in more detail?

Our education director, Lars Brünjes, has an excellent series of [tutorials](https://youtu.be/IEn6jUo-0vU) on youtube. We will not attempt to provide an indepth explanation of [Zerepoch](https://docs.bcc.org/projects/zerepoch/en/latest/) in this repository.

## Tutorials

We demonstrate the following scripts:

1. Always succeeds [Zerepoch script](src/Bcc/ZerepochExample/Untyped/AlwaysSucceeds.hs).
2. Simple minting Zerepoch script (In progress).




