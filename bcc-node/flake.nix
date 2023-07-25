{
  description = "Bcc Node";

  inputs = {
    # IMPORTANT: report any change to nixpkgs channel in nix/default.nix:
    nixpkgs.follows = "haskellNix/nixpkgs-2105";
    haskellNix = {
      url = "github:The-Blockchain-Company/haskell.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    utils.url = "github:rmourey26/flake-utils";
    tbcoNix = {
      url = "github:The-Blockchain-Company/tbco-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # Custom user config (default: empty), eg.:
    # { outputs = {...}: {
    #   # Cutomize listeming port of node scripts:
    #   nixosModules.bcc-node = {
    #     services.bcc-node.port = 3002;
    #   };
    # };
    customConfig.url = "github:The-Blockchain-Company/empty-flake";
  };

  outputs = { self, nixpkgs, utils, haskellNix, tbcoNix, customConfig }:
    let
      inherit (nixpkgs) lib;
      inherit (lib) head systems mapAttrs recursiveUpdate mkDefault
        getAttrs optionalAttrs nameValuePair attrNames;
      inherit (utils.lib) eachSystem mkApp flattenTree;
      inherit (tbcoNix.lib) prefixNamesWith collectExes;

      supportedSystems = import ./supported-systems.nix;
      defaultSystem = head supportedSystems;

      overlays = [
        tbcoNix.overlays.haskell-nix-extra
        tbcoNix.overlays.crypto
        tbcoNix.overlays.bcc-lib
        tbcoNix.overlays.utils
        (final: prev: {
          customConfig = recursiveUpdate
            (import ./nix/custom-config.nix final.customConfig)
            customConfig.outputs;
          gitrev = self.rev or "dirty";
          commonLib = lib
            // tbcoNix.lib
            // final.bccLib
            // import ./nix/svclib.nix { inherit (final) pkgs; };
        })
        (import ./nix/pkgs.nix)
      ];

    in eachSystem supportedSystems (system:
      let
        pkgs = haskellNix.legacyPackages.${system}.appendOverlays overlays;

        inherit (pkgs.commonLib) eachEnv environments;

        devShell = import ./shell.nix { inherit pkgs; };

        flake = pkgs.bccNodeProject.flake {};

        staticFlake = pkgs.pkgsStatic.bccNodeProject.flake {};

        windowsFlake = pkgs.pkgsCross.${systems.examples.mingwW64}.bccNodeProject.flake {};

        scripts = flattenTree pkgs.scripts;

        checkNames = attrNames flake.checks;

        checks =
          # Linux only checks:
          optionalAttrs (system == "x86_64-linux") (
            prefixNamesWith "windows/" (removeAttrs
              (getAttrs checkNames windowsFlake.checks)
              ["bcc-node-chairman:test:chairman-tests"]
            )
            // (prefixNamesWith "nixosTests/" (mapAttrs (_: v: v.${system} or v) pkgs.nixosTests))
          )
          # checks run on default system only;
          // optionalAttrs (system == defaultSystem) {
            hlint = pkgs.callPackage pkgs.hlintCheck {
              inherit (pkgs.bccNodeProject.projectModule) src;
            };
          };

        exes = collectExes flake.packages;
        exeNames = attrNames exes;
        lazyCollectExe = p: getAttrs exeNames (collectExes p);

        packages = {
          inherit (devShell) devops;
          inherit (pkgs) bcc-node-profiled bcc-node-eventlogged bcc-node-asserted tx-generator-profiled locli-profiled;
        }
        // scripts
        // exes
        // (prefixNamesWith "static/"
              (mapAttrs pkgs.rewriteStatic (lazyCollectExe staticFlake.packages)))
        # Linux only packages:
        // optionalAttrs (system == "x86_64-linux") (
          prefixNamesWith "windows/" (lazyCollectExe windowsFlake.packages)
          // {
            "dockerImage/node" = pkgs.dockerImage;
            "dockerImage/submit-api" = pkgs.submitApiDockerImage;
          }
        )
        # Add checks to be able to build them individually
        // (prefixNamesWith "checks/" checks);

      in recursiveUpdate flake {

        inherit environments packages checks;

        legacyPackages = pkgs;

        # Built by `nix build .`
        defaultPackage = flake.packages."bcc-node:exe:bcc-node";

        # Run by `nix run .`
        defaultApp = flake.apps."bcc-node:exe:bcc-node";

        # This is used by `nix develop .` to open a devShell
        inherit devShell;

        apps = {
          repl = mkApp {
            drv = pkgs.writeShellScriptBin "repl" ''
              confnix=$(mktemp)
              echo "builtins.getFlake (toString $(git rev-parse --show-toplevel))" >$confnix
              trap "rm $confnix" EXIT
              nix repl $confnix
          '';
          };
          bcc-ping = { type = "app"; program = pkgs.bcc-ping.exePath; };
        }
        # nix run .#<exe>
        // (collectExes flake.apps);
      }
    ) // {
      overlay = import ./overlay.nix self;
      nixosModules = {
        bcc-node = { pkgs, lib, ... }: {
          imports = [ ./nix/nixos/bcc-node-service.nix ];
          services.bcc-node.bccNodePkgs = lib.mkDefault self.legacyPackages.${pkgs.system};
        };
        bcc-submit-api = { pkgs, lib, ... }: {
          imports = [ ./nix/nixos/bcc-submit-api-service.nix ];
          services.bcc-submit-api.bccNodePkgs = lib.mkDefault self.legacyPackages.${pkgs.system};
        };
      };
    };
}
