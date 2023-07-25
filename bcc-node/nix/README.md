# Nix dependencies

The nix build use the new flake format to manage dependencies. A flake-compatible nix command is provided from within `nix-shell`. To add flake support to your native nix setup please see https://nixos.wiki/wiki/Flakes.

Bcc-node nix build depends primarily on [haskell.nix](https://github.com/The-Blockchain-Company/haskell.nix) and secondarily, for some utilities, on [tbco-nix](https://github.com/The-Blockchain-Company/tbco-nix/).

Both can be updated from within a bcc-node `nix-shell` with:

```
nix flake update --update-input haskellNix
nix flake update --update-input tbcoNix
```

Or from outside the `nix-shell` with the scripts:

```
./nix/update-haskellNix.sh
./nix/update-tbcoNix.sh
```
