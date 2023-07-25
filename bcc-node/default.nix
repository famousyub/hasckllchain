{ system ? builtins.currentSystem
, crossSystem ? null
# allows to cutomize haskellNix (ghc and profiling, see ./nix/haskell.nix)
, config ? {}
# override scripts with custom configuration
, customConfig ? {}
# allows to override dependencies of the project without modifications,
# eg. to test build against local checkout of nixpkgs and tbco-nix:
# nix build -f default.nix bcc-node --arg sourcesOverride '{
#   tbco-nix = ../tbco-nix;
# }'
, sourcesOverride ? {}
# pinned version of nixpkgs augmented with overlays (tbco-nix and our packages).
, pkgs ? import ./nix/default.nix { inherit system crossSystem config customConfig sourcesOverride gitrev; }
# Git sha1 hash, to be passed when not building from a git work tree.
, gitrev ? null
}:
with pkgs; with commonLib;
let

  haskellPackages = recRecurseIntoAttrs
    # the Haskell.nix package set, reduced to local packages.
    (selectProjectPackages bccNodeHaskellPackages);


  shell = import ./shell.nix {
    inherit pkgs;
    withHoogle = true;
  };

  packages = {
    inherit haskellPackages shell
      bcc-node bcc-node-profiled bcc-node-eventlogged
      bcc-cli db-converter bcc-ping
      locli locli-profiled
      tx-generator tx-generator-profiled
      scripts environments dockerImage submitApiDockerImage bech32;

    devopsShell = shell.devops;

    nixosTests = recRecurseIntoAttrs nixosTests;

    # so that eval time gc roots are cached (nix-tools stuff)
    inherit (bccNodeProject) roots plan-nix;

    inherit (haskellPackages.bcc-node.identifier) version;

    exes = mapAttrsRecursiveCond (as: !(isDerivation as)) rewriteStatic (collectComponents' "exes" haskellPackages);

    # `tests` are the test suites which have been built.
    tests = collectComponents' "tests" haskellPackages;
    # `benchmarks` (only built, not run).
    benchmarks = collectComponents' "benchmarks" haskellPackages;

    checks = recurseIntoAttrs {
      # `checks.tests` collect results of executing the tests:
      tests = collectChecks haskellPackages;

      hlint = callPackage hlintCheck {
        inherit (bccNodeProject.projectModule) src;
      };
    };

};
in packages
