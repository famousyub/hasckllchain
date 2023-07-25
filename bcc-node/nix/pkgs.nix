# our packages overlay
final: prev: with final;
  let
    compiler-nix-name = config.haskellNix.compiler or "ghc8105";
  in {
  bccNodeProject = import ./haskell.nix {
    inherit compiler-nix-name
      pkgs
      lib
      stdenv
      haskell-nix
      buildPackages
      gitrev
      ;
  };
  bccNodeHaskellPackages = bccNodeProject.hsPkgs;
  bccNodeProfiledHaskellPackages = (import ./haskell.nix {
    inherit compiler-nix-name
      pkgs
      lib
      stdenv
      haskell-nix
      buildPackages
      gitrev
      ;
    inherit (bccNodeProject) projectPackages;
    inherit (bccNodeProject.projectModule) src cabalProjectLocal;
    profiling = true;
  }).hsPkgs;
  bccNodeEventlogHaskellPackages = (import ./haskell.nix {
    inherit compiler-nix-name
      pkgs
      lib
      stdenv
      haskell-nix
      buildPackages
      gitrev
      ;
    inherit (bccNodeProject) projectPackages;
    inherit (bccNodeProject.projectModule) src cabalProjectLocal;
    eventlog = true;
  }).hsPkgs;
  bccNodeAssertedHaskellPackages = (import ./haskell.nix {
    inherit compiler-nix-name
      pkgs
      lib
      stdenv
      haskell-nix
      buildPackages
      gitrev
      ;
    inherit (bccNodeProject) projectPackages;
    inherit (bccNodeProject.projectModule) src cabalProjectLocal;
    assertedPackages = [
      "shardagnostic-consensus"
      "shardagnostic-consensus-bcc"
      "shardagnostic-consensus-cole"
      "shardagnostic-consensus-sophie"
      "shardagnostic-network"
      "network-mux"
    ];
  }).hsPkgs;

  #Grab the executable component of our package.
  inherit (bccNodeHaskellPackages.bcc-node.components.exes) bcc-node;
  inherit (bccNodeHaskellPackages.bcc-cli.components.exes) bcc-cli;
  inherit (bccNodeHaskellPackages.bcc-topology.components.exes) bcc-topology;
  inherit (bccNodeHaskellPackages.tx-generator.components.exes) tx-generator;
  inherit (bccNodeHaskellPackages.locli.components.exes) locli;
  inherit (bccNodeHaskellPackages.bech32.components.exes) bech32;
  inherit (bccNodeHaskellPackages.bcc-submit-api.components.exes) bcc-submit-api;
  bcc-node-profiled = bccNodeProfiledHaskellPackages.bcc-node.components.exes.bcc-node;
  bcc-node-eventlogged = bccNodeEventlogHaskellPackages.bcc-node.components.exes.bcc-node;
  bcc-node-asserted = bccNodeAssertedHaskellPackages.bcc-node.components.exes.bcc-node;
  tx-generator-profiled = bccNodeProfiledHaskellPackages.tx-generator.components.exes.tx-generator;
  locli-profiled = bccNodeProfiledHaskellPackages.locli.components.exes.locli;

  # expose the db-converter and bcc-ping from the shardagnostic-network we depend on
  inherit (bccNodeHaskellPackages.shardagnostic-consensus-cole.components.exes) db-converter;
  inherit (bccNodeHaskellPackages.network-mux.components.exes) bcc-ping;

  cabal = haskell-nix.tool compiler-nix-name "cabal" {
    version = "latest";
    inherit (bccNodeProject) index-state;
  };

  hlint = haskell-nix.tool compiler-nix-name "hlint" {
    version = "3.2.7";
    inherit (bccNodeProject) index-state;
  };

  haskellBuildUtils = prev.haskellBuildUtils.override {
    inherit compiler-nix-name;
    inherit (bccNodeProject) index-state;
  };

  bcclib-py = callPackage ./bcclib-py {};

  scripts = lib.recursiveUpdate (import ./scripts.nix { inherit pkgs; })
    (import ./scripts-submit-api.nix { inherit pkgs; });

  dockerImage = let
    defaultConfig = {
      stateDir = "/data";
      dbPrefix = "db";
      socketPath = "/ipc/node.socket";
    };
  in callPackage ./docker {
    exe = "bcc-node";
    scripts = import ./scripts.nix {
      inherit pkgs;
      customConfigs = [ defaultConfig customConfig ];
    };
    script = "node";
  };

  submitApiDockerImage = let
    defaultConfig = {
      socketPath = "/ipc/node.socket";
    };
  in callPackage ./docker {
    exe = "bcc-submit-api";
    scripts = import ./scripts-submit-api.nix {
      inherit pkgs;
      customConfigs = [ defaultConfig customConfig ];
    };
    script = "submit-api";
  };

  # NixOS tests run a node and submit-api and validate it listens
  nixosTests = import ./nixos/tests {
    inherit pkgs;
  };

  clusterTests = import ./supervisord-cluster/tests { inherit pkgs; };

  # Disable failing python uvloop tests
  python38 = prev.python38.override {
    packageOverrides = pythonFinal: pythonPrev: {
      uvloop = pythonPrev.uvloop.overrideAttrs (attrs: {
        disabledTestPaths = [ "tests/test_tcp.py" "tests/test_sourcecode.py" "tests/test_dns.py" ];
      });
    };
  };
}
