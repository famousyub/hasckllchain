{ config, lib, pkgs, ... }:

# notes:
# this service exposes an http port, and connects to a bcc-node over a UNIX socket
let
  cfg = config.services.bcc-submit-api;
  inherit (cfg.bccNodePkgs) commonLib;
  envConfig = cfg.environment;
in {
  options = {
    services.bcc-submit-api = {
      enable = lib.mkEnableOption "enable the bcc-submit-api api";
      script = lib.mkOption {
        internal = true;
        type = lib.types.package;
      };
      package = lib.mkOption {
        type = lib.types.package;
        default = cfg.bccNodePkgs.bccNodeHaskellPackages.bcc-submit-api.components.exes.bcc-submit-api;
      };
      port = lib.mkOption {
        type = lib.types.port;
        default = 8090;
      };
      listenAddress = lib.mkOption {
        type = lib.types.str;
        default = "127.0.0.1";
      };
      socketPath = lib.mkOption {
        type = lib.types.nullOr lib.types.path;
        default = null;
      };
      config = lib.mkOption {
        type = lib.types.nullOr lib.types.attrs;
        default = commonLib.defaultExplorerLogConfig;
      };
      network = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        description = "network name";
        default = null;
      };
      environment = lib.mkOption {
        type = lib.types.nullOr lib.types.attrs;
        default = commonLib.environments.${cfg.network};
      };
      bccNodePkgs = lib.mkOption {
        type = lib.types.attrs;
        default = import ../. {};
        defaultText = "bcc-node pkgs";
        description = ''
          The bcc-node packages and library that should be used.
          Main usage is sharing optimization:
          reduce eval time when service is instantiated multiple times.
        '';
      };
    };
  };
  config = let
    envNodeCfg = cfg.environment.nodeConfig;
    sophieGenesisParams = __fromJSON (__readFile envNodeCfg.SophieGenesisFile);
    envFlag = if cfg.network == "mainnet" then "--mainnet" else "--testnet-magic ${toString sophieGenesisParams.networkMagic}";
  in lib.mkIf cfg.enable {
    services.bcc-submit-api.script = pkgs.writeShellScript "bcc-submit-api" ''
      ${if (cfg.socketPath == null) then ''if [ -z "$BCC_NODE_SOCKET_PATH" ]
      then
        echo "You must set \$BCC_NODE_SOCKET_PATH"
        exit 1
      fi'' else "export \"BCC_NODE_SOCKET_PATH=${cfg.socketPath}\""}
      exec ${cfg.package}/bin/bcc-submit-api --socket-path "$BCC_NODE_SOCKET_PATH" ${envFlag} \
            --port ${toString cfg.port} \
            --listen-address ${cfg.listenAddress} \
            --config ${builtins.toFile "submit-api.json" (builtins.toJSON cfg.config)}
    '';
    systemd.services.bcc-submit-api = {
      serviceConfig = {
        ExecStart = config.services.bcc-submit-api.script;
        DynamicUser = true;
      };
      wantedBy = [ "multi-user.target" ];
      after = [ "bcc-node.service" ];
    };
  };
}
