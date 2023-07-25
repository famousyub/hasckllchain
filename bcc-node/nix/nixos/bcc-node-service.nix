{ config
, lib
, pkgs
, ... }:

with lib; with builtins;
let
  cfg = config.services.bcc-node;
  inherit (cfg.bccNodePkgs) commonLib bcc-node bcc-node-profiled bcc-node-eventlogged bcc-node-asserted;
  envConfig = cfg.environments.${cfg.environment};
  runtimeDir = if cfg.runtimeDir == null then cfg.stateDir else "/run/${cfg.runtimeDir}";
  mkScript = cfg: i: let
    instanceConfig =
      cfg.nodeConfig
      //
      (optionalAttrs (cfg.nodeConfig ? hasEKG)
        {
          hasEKG = cfg.nodeConfig.hasEKG + i;
        })
      //
      (optionalAttrs (cfg.nodeConfig ? hasPrometheus)
        {
          hasPrometheus = map (n: if isInt n then n + i else n) cfg.nodeConfig.hasPrometheus;
        })
      //
      (foldl' (x: y: x // y) {}
        (mapAttrsToList
          (era: epoch:
            { "Test${era}HardForkAtEpoch" = epoch;
            })
          cfg.forceHardForks));
    nodeConfigFile = if (cfg.nodeConfigFile != null) then cfg.nodeConfigFile
      else toFile "config-${toString cfg.nodeId}-${toString i}.json" (toJSON instanceConfig);
    realNodeConfigFile = nodeConfigFile;
    topology = if cfg.topology != null then cfg.topology else toFile "topology.yaml" (toJSON {
      Producers = cfg.producers ++ (cfg.instanceProducers i);
    });
    consensusParams = {
      RealPBFT = [
        "${lib.optionalString (cfg.signingKey != null)
          "--signing-key ${cfg.signingKey}"}"
        "${lib.optionalString (cfg.delegationCertificate != null)
          "--delegation-certificate ${cfg.delegationCertificate}"}"
      ];
      TOptimum = [
        "${lib.optionalString (cfg.vrfKey != null)
          "--sophie-vrf-key ${cfg.vrfKey}"}"
        "${lib.optionalString (cfg.kesKey != null)
          "--sophie-kes-key ${cfg.kesKey}"}"
        "${lib.optionalString (cfg.operationalCertificate != null)
          "--sophie-operational-certificate ${cfg.operationalCertificate}"}"
      ];
      Bcc = [
        "${lib.optionalString (cfg.signingKey != null)
          "--signing-key ${cfg.signingKey}"}"
        "${lib.optionalString (cfg.delegationCertificate != null)
          "--delegation-certificate ${cfg.delegationCertificate}"}"
        "${lib.optionalString (cfg.vrfKey != null)
          "--sophie-vrf-key ${cfg.vrfKey}"}"
        "${lib.optionalString (cfg.kesKey != null)
          "--sophie-kes-key ${cfg.kesKey}"}"
        "${lib.optionalString (cfg.operationalCertificate != null)
          "--sophie-operational-certificate ${cfg.operationalCertificate}"}"
      ];
    };
    instanceDbPath = "${cfg.databasePath}${optionalString (i > 0) "-${toString i}"}";
    cmd = builtins.filter (x: x != "") [
      "${cfg.executable} run"
      "--config ${realNodeConfigFile}"
      "--database-path ${instanceDbPath}"
      "--topology ${topology}"
    ] ++ (lib.optionals (!cfg.systemdSocketActivation) ([
      "--host-addr ${cfg.hostAddr}"
      "--port ${toString (cfg.port + i)}"
      "--socket-path ${cfg.socketPath}"
    ] ++ lib.optional (cfg.ipv6HostAddr != null)
      "--host-ipv6-addr ${cfg.ipv6HostAddr}"
    )) ++ consensusParams.${cfg.nodeConfig.Protocol} ++ cfg.extraArgs ++ cfg.rtsArgs;
    in ''
        echo "Starting: ${concatStringsSep "\"\n   echo \"" cmd}"
        echo "..or, once again, in a single line:"
        echo "${toString cmd}"
        ${lib.optionalString (i > 0) ''
        # If exist copy state from existing instance instead of syncing from scratch:
        if [ ! -d ${instanceDbPath} ] && [ -d ${cfg.databasePath} ]; then
          echo "Copying existing immutable db from ${cfg.databasePath}"
          ${pkgs.rsync}/bin/rsync --archive --ignore-errors --exclude 'clean' ${cfg.databasePath}/ ${instanceDbPath}/ || true
        fi
        ''}
        exec ${toString cmd}'';
in {
  options = {
    services.bcc-node = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Enable bcc-node, a node implementing shardagnostic protocols
          (the blockchain protocols running bcc).
        '';
      };
      instances = mkOption {
        type = types.int;
        default = 1;
        description = ''
          Number of instance of the service to run.
        '';
      };
      script = mkOption {
        type = types.str;
        default = mkScript cfg 0;
      };

      profiling = mkOption {
        type = types.enum ["none" "time" "space" "space-module" "space-closure" "space-type" "space-retainer" "space-bio"];
        default = "none";
      };

      eventlog = mkOption {
        type = types.bool;
        default = false;
      };

      asserts = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to use an executable with asserts enabled.
        '';
      };

      bccNodePkgs = mkOption {
        type = types.attrs;
        default = import ../. {};
        defaultText = "bcc-node pkgs";
        description = ''
          The bcc-node packages and library that should be used.
          Main usage is sharing optimization:
          reduce eval time when service is instantiated multiple times.
        '';
      };

      package = mkOption {
        type = types.package;
        default = if (cfg.profiling != "none")
          then bcc-node-profiled
          else if cfg.eventlog then bcc-node-eventlogged
          else if cfg.asserts then bcc-node-asserted
          else bcc-node;
        defaultText = "bcc-node";
        description = ''
          The bcc-node package that should be used
        '';
      };

      executable = mkOption {
        type = types.str;
        default = "${cfg.package}/bin/bcc-node";
        defaultText = "bcc-node";
        description = ''
          The bcc-node executable invocation to use
        '';
      };

      environments = mkOption {
        type = types.attrs;
        default = commonLib.environments;
        description = ''
          environment node will connect to
        '';
      };

      environment = mkOption {
        type = types.enum (builtins.attrNames cfg.environments);
        default = "testnet";
        description = ''
          environment node will connect to
        '';
      };

      # Cole signing/delegation

      signingKey = mkOption {
        type = types.nullOr (types.either types.str types.path);
        default = null;
        description = ''
          Signing key
        '';
      };

      delegationCertificate = mkOption {
        type = types.nullOr (types.either types.str types.path);
        default = null;
        description = ''
          Delegation certificate
        '';
      };

      # Sophie kes/vrf keys and operation cert

      kesKey = mkOption {
        type = types.nullOr (types.either types.str types.path);
        default = null;
        description = ''
          Signing key
        '';
      };
      vrfKey = mkOption {
        type = types.nullOr (types.either types.str types.path);
        default = null;
        description = ''
          Signing key
        '';
      };

      operationalCertificate = mkOption {
        type = types.nullOr (types.either types.str types.path);
        default = null;
        description = ''
          Operational certificate
        '';
      };

      hostAddr = mkOption {
        type = types.str;
        default = "127.0.0.1";
        description = ''
          The host address to bind to
        '';
      };

      ipv6HostAddr = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = ''
          The ipv6 host address to bind to. Set to null to disable.
        '';
      };

      stateDir = mkOption {
        type = types.str;
        default = "/var/lib/bcc-node";
        description = ''
          Directory to store blockchain data.
        '';
      };

      runtimeDir = mkOption {
        type = types.nullOr types.str;
        default = "bcc-node";
        description = ''
          Runtime directory relative to /run
        '';
      };

      databasePath = mkOption {
        type = types.str;
        default = "${cfg.stateDir}/${cfg.dbPrefix}";
        description = ''Node database path.'';
      };

      socketPath = mkOption {
        type = types.str;
        default = "${runtimeDir}/node.socket";
        description = ''Local communication socket path.'';
      };

      systemdSocketActivation = mkOption {
        type = types.bool;
        default = false;
        description = ''Use systemd socket activation'';
      };

      extraServiceConfig = mkOption {
        # activate type for nixos-21.03:
        type = types.unspecified # types.functionTo (types.listOf types.attrs);
          // {
            merge = loc: foldl' (res: def: i: recursiveUpdate (res i) (def.value i)) (i: {});
          };
        default = i: {};
        description = ''
          Extra systemd service config (apply to all instances).
        '';
      };

      extraSocketConfig = mkOption {
        # activate type for nixos-21.03:
        type = types.unspecified # types.functionTo (types.listOf types.attrs);
          // {
            merge = loc: foldl' (res: def: i: recursiveUpdate (res i) (def.value i)) (i: {});
          };
        default = i: {};
        description = ''
          Extra systemd socket config (apply to all instances).
        '';
      };

      dbPrefix = mkOption {
        type = types.str;
        default = "db-${cfg.environment}";
        description = ''
          Prefix of database directories inside `stateDir`.
          (eg. for "db", there will be db-0, etc.).
        '';
      };

      port = mkOption {
        type = types.either types.int types.str;
        default = 3001;
        description = ''
          The port number
        '';
      };

      shareIpv4port = mkOption {
        type = types.bool;
        default = cfg.systemdSocketActivation;
        description = ''
          Should instances on same machine share ipv4 port.
          Default: true if systemd activated socket. Otherwise always false.
          If false use port increments starting from `port`.
        '';
      };

      shareIpv6port = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Should instances on same machine share ipv6 port.
          Only works with systemd socket.
          Default: false.
          If false use port increments starting from `port`.
        '';
      };

      nodeId = mkOption {
        type = types.int;
        default = 0;
        description = ''
          The ID for this node
        '';
      };

      producers = mkOption {
        default = [{
          addr = envConfig.relaysNew;
          port = envConfig.edgePort;
          valency = 1;
        }];
        type = types.listOf types.attrs;
        description = ''Static routes to peers.'';
      };

      instanceProducers = mkOption {
        # activate type for nixos-21.03:
        # type = types.functionTo (types.listOf types.attrs);
        default = _: [];
        description = ''
          Static routes to peers, specific to a given instance (when multiple instances are used).
        '';
      };

      topology = mkOption {
        type = types.nullOr (types.either types.str types.path);
        default = null;
        description = ''
          Cluster topology. If not set `producers` array is used to generated topology file.
        '';
      };

      nodeConfig = mkOption {
        type = types.attrs // {
          merge = loc: foldl' (res: def: recursiveUpdate res def.value) {};
        };
        default = envConfig.nodeConfig;
        description = ''Internal representation of the config.'';
      };

      nodeConfigFile = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = ''Actual configuration file (shell expression).'';
      };

      forceHardForks = mkOption {
        type = types.attrs;
        default = {};
        description = ''
          A developer-oriented dictionary option to force hard forks for given eras at given epochs.  Maps capitalised era names (Sophie, Evie, Jen, etc.) to hard fork epoch number.
          '';
      };

      extraArgs = mkOption {
        type = types.listOf types.str;
        default = [];
        description = ''Extra CLI args for 'bcc-node'.'';
      };

      rtsArgs = mkOption {
        type = types.listOf types.str;
        default = [ "-N2" "-A16m" "-qg" "-qb" "--disable-delayed-os-memory-return" ];
        apply = args: if (args != [] || cfg.profilingArgs != []) then
          ["+RTS"] ++ cfg.profilingArgs ++ args ++ ["-RTS"]
          else [];
        description = ''Extra CLI args for 'bcc-node', to be surrounded by "+RTS"/"-RTS"'';
      };

      profilingArgs = mkOption {
        type = types.listOf types.str;
        default = let commonProfilingArgs = ["--machine-readable" "-tbcc-node.stats" "-pobcc-node"]
          ++ lib.optional (cfg.eventlog) "-l";
          in if cfg.profiling == "time" then ["-P"] ++ commonProfilingArgs
            else if cfg.profiling == "space" then ["-h"] ++ commonProfilingArgs
            else if cfg.profiling == "space-module" then ["-hm"] ++ commonProfilingArgs
            else if cfg.profiling == "space-closure" then ["-hd"] ++ commonProfilingArgs
            else if cfg.profiling == "space-type" then ["-hy"] ++ commonProfilingArgs
            else if cfg.profiling == "space-retainer" then ["-hr"] ++ commonProfilingArgs
            else if cfg.profiling == "space-bio" then ["-hb"] ++ commonProfilingArgs
            else [];
        description = ''RTS profiling options'';
      };
    };
  };

  config = mkIf cfg.enable ( let
    stateDirBase = "/var/lib/";
    runDirBase = "/run/";
    genInstanceConf = f: listToAttrs (if cfg.instances > 1
      then genList (i: let n = "bcc-node-${toString i}"; in nameValuePair n (f n i)) cfg.instances
      else [ (nameValuePair "bcc-node" (f "bcc-node" 0)) ]); in lib.mkMerge [
    {
      users.groups.bcc-node.gid = 10016;
      users.users.bcc-node = {
        description = "bcc-node node daemon user";
        uid = 10016;
        group = "bcc-node";
        isSystemUser = true;
      };

      ## TODO:  use http://hackage.haskell.org/package/systemd for:
      ##   1. only declaring success after we perform meaningful init (local state recovery)
      ##   2. heartbeat & watchdog functionality
      systemd.services = genInstanceConf (n: i: recursiveUpdate {
        description   = "bcc-node node ${toString i} service";
        after         = [ "network-online.target" ]
          ++ (optional cfg.systemdSocketActivation "${n}.socket")
          ++ (optional (cfg.instances > 1) "bcc-node.service");
        requires = optional cfg.systemdSocketActivation "${n}.socket"
          ++ (optional (cfg.instances > 1) "bcc-node.service");
        wants = [ "network-online.target" ];
        wantedBy = [ "multi-user.target" ];
        partOf = mkIf (cfg.instances > 1) ["bcc-node.service"];
        script = mkScript cfg i;
        serviceConfig = {
          User = "bcc-node";
          Group = "bcc-node";
          Restart = "always";
          RuntimeDirectory = lib.mkIf (!cfg.systemdSocketActivation)
            (lib.removePrefix runDirBase (if (i == 0) then runtimeDir else "${runtimeDir}-${toString i}"));
          WorkingDirectory = cfg.stateDir;
          # This assumes /var/lib/ is a prefix of cfg.stateDir.
          # This is checked as an assertion below.
          StateDirectory =  lib.removePrefix stateDirBase cfg.stateDir;
          NonBlocking = lib.mkIf cfg.systemdSocketActivation true;
          # time to sleep before restarting a service
          RestartSec = 1;
          KillSignal = "SIGINT";
        };
      } (cfg.extraServiceConfig i));

      systemd.sockets = genInstanceConf (n: i: lib.mkIf cfg.systemdSocketActivation (recursiveUpdate {
        description = "Socket of the ${n} service.";
        wantedBy = [ "sockets.target" ];
        partOf = [ "${n}.service" ];
        socketConfig = {
          ListenStream = [ "${cfg.hostAddr}:${toString (if cfg.shareIpv4port then cfg.port else cfg.port + i)}" ]
            ++ optional (cfg.ipv6HostAddr != null) "[${cfg.ipv6HostAddr}]:${toString (if cfg.shareIpv6port then cfg.port else cfg.port + i)}"
            ++ [(if (i == 0) then cfg.socketPath else "${runtimeDir}-${toString i}/node.socket")];
          RuntimeDirectory = lib.removePrefix runDirBase
            (if (i == 0) then runtimeDir else "${runtimeDir}-${toString i}");
          ReusePort = "yes";
          SocketMode = "0660";
          SocketUser = "bcc-node";
          SocketGroup = "bcc-node";
          FreeBind = "yes";
        };
      } (cfg.extraSocketConfig i)));
    }
    {
      # oneshot service start allows to easily control all instances at once.
      systemd.services.bcc-node = lib.mkIf (cfg.instances > 1) {
        description = "Control all ${toString cfg.instances} at once.";
        enable  = true;
        wants = genList (i: "bcc-node-${toString i}.service") cfg.instances;
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = "yes";
          User = "bcc-node";
          Group = "bcc-node";
          ExecStart = "${pkgs.coreutils}/bin/echo Starting ${toString cfg.instances} bcc-node instances";
          WorkingDirectory = cfg.stateDir;
          StateDirectory =  lib.removePrefix stateDirBase cfg.stateDir;
        };
      };
    }
    {
      assertions = [
        {
          assertion = lib.hasPrefix stateDirBase cfg.stateDir;
          message = "The option services.bcc-node.stateDir should have ${stateDirBase} as a prefix!";
        }
        {
          assertion = (cfg.kesKey == null) == (cfg.vrfKey == null) && (cfg.kesKey == null) == (cfg.operationalCertificate == null);
          message = "Sophie Era: all of three [operationalCertificate kesKey vrfKey] options must be defined (or none of them).";
        }
      ];
    }
  ]);
}
