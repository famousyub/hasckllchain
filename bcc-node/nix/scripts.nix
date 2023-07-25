{ pkgs
, customConfigs ? [ pkgs.customConfig ]
}:
with pkgs.commonLib;
let
  mkScript = envConfig: let
    service = evalService {
      inherit pkgs customConfigs;
      serviceName = "bcc-node";
      modules = [
        ./nixos/bcc-node-service.nix
        ({config, ...}: {
          services.bcc-node = let cfg = config.services.bcc-node; in {
            hostAddr = mkDefault "0.0.0.0";
            environment = mkDefault envConfig.name;
            nodeConfig = cfg.environments.${cfg.environment}.nodeConfig;
            bccNodePkgs = mkDefault pkgs;
            stateDir = mkDefault "state-node-${cfg.environment}";
            runtimeDir = mkDefault null;
          } // optionalAttrs (envConfig ? topology) {
            topology = mkDefault envConfig.topology;
          };
        })
      ];
    };

  in pkgs.writeScriptBin "bcc-node-${service.environment}" ''
    #!${pkgs.runtimeShell}
    export PATH=$PATH:${makeBinPath [ pkgs.coreutils ]}
    set -euo pipefail
    mkdir -p "$(dirname "${service.socketPath}")"
    ${service.script} $@
  '';

  debugDeps = with pkgs; [
    coreutils
    findutils
    gnugrep
    gnused
    postgresql
    strace
    lsof
    dnsutils
    bashInteractive
    iproute
    curl
    netcat
    bat
    tree
  ];

in forEnvironments (environment: recurseIntoAttrs rec {
  node = mkScript environment;
  node-debug = pkgs.symlinkJoin {
    inherit (node) name;
    paths = [ node ] ++ debugDeps;
  };
})
