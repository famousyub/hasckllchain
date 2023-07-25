{ pkgs
, runJq

## The backend is an attrset of AWS/supervisord-specific methods and parameters.
, backend

## Environmental settings:
##   - either affect semantics on all backends equally,
##   - or have no semantic effect
, environment

, profile
}:

with pkgs.lib;

let

  ## The AWS node is started with:
  ## bcc-node run
  ## --config /nix/store/nywkyj205skkqy27ip3p0678977kxq0b-config-1-0.json
  ## --database-path /var/lib/bcc-node/db-bench-dense-k51-10ep-2000kU-500kD-6600esec
  ## --topology /nix/store/sb8gn8wb4s8m7a4pmkb6hvlr4fhy5vn2-topology.yaml
  ## --sophie-vrf-key /var/lib/keys/bcc-node-vrf-signing
  ## --sophie-kes-key /var/lib/keys/bcc-node-kes-signing
  ## --sophie-operational-certificate /var/lib/keys/bcc-node-operational-cert
  ## +RTS -l-agu -t --machine-readable -RTS +RTS -N2 -A16m -qg -qb -M14336.000000M -RTS
  ##
  ## nodeSpecServiceConfig :: NodeSpec -> ServiceConfig
  ##
  nodeSpecServiceConfig =
    { name, i, kind, port, isProducer }@nodeSpec:

    backend.finaliseNodeService nodeSpec
    {
      inherit port;

      forceHardForks =
        {
          sophie = { Sophie = 0; };
          evie = { Sophie = 0; Evie = 0; };
          jen    = { Sophie = 0; Evie = 0; Jen = 0; };
          aurum  = { Sophie = 0; Evie = 0; Jen = 0; Aurum = 0; };
        }.${profile.value.era};

      nodeConfig =
       backend.finaliseNodeConfig nodeSpec
         ((removeAttrs
           (recursiveUpdate environment.bccLib.environments.testnet.nodeConfig
             {
               Protocol             = "Bcc";
               RequiresNetworkMagic = "RequiresMagic";

               TracingVerbosity     = "NormalVerbosity";
               minSeverity          = "Debug";

               TraceMempool         = true;
               TraceTxInbound       = true;

               defaultScribes = [
                 [ "StdoutSK" "stdout" ]
               ];
               setupScribes =
                 [{
                   scKind     = "StdoutSK";
                   scName     = "stdout";
                   scFormat   = "ScJson";
                 }];
               options = {
                 mapBackends = {
                   "bcc.node.resources" = [ "KatipBK" ];
                 };
               };

               LastKnownBlockVersion-Major = 0;
               LastKnownBlockVersion-Sentry = 0;
               LastKnownBlockVersion-Alt   = 0;
             })
           [ "AurumGenesisHash"
             "ColeGenesisHash"
             "SophieGenesisHash"
           ])
       //
       ({
         sophie =
           { TestSophieHardForkAtEpoch = 0;
           };
         evie =
           { TestSophieHardForkAtEpoch = 0;
             TestEvieHardForkAtEpoch = 0;
           };
         jen =
           { TestSophieHardForkAtEpoch = 0;
             TestEvieHardForkAtEpoch = 0;
             TestJenHardForkAtEpoch    = 0;
           };
         aurum =
           { TestSophieHardForkAtEpoch = 0;
             TestEvieHardForkAtEpoch = 0;
             TestJenHardForkAtEpoch    = 0;
             TestAurumHardForkAtEpoch  = 0;
             TestEnableDevelopmentHardForkEras     = true;
             TestEnableDevelopmentNetworkProtocols = true;
           };
       }).${profile.value.era});
    };

  ## Given an env config, evaluate it and produce the node service.
  ## Call the given function on this service.
  ##
  ## nodeServiceConfigService :: NodeServiceConfig -> NodeService
  ##
  nodeServiceConfigService =
    serviceConfig:
    let
    systemdCompat.options = {
      systemd.services = mkOption {};
      systemd.sockets = mkOption {};
      users = mkOption {};
      assertions = mkOption {};
    };
    eval = let
      extra = {
        services.bcc-node = {
          enable = true;
          bccNodePkgs = pkgs;
        } // serviceConfig;
      };
    in evalModules {
      prefix = [];
      modules = import ../../nixos/module-list.nix ++ [ systemdCompat extra ];
      args = { inherit pkgs; };
    };
    in eval.config.services.bcc-node;

  ##
  ## node-services :: Map NodeName (NodeSpec, ServiceConfig, Service, NodeConfig, Script)
  ##
  node-services = mapAttrs
    (_: { name, i, ... }@nodeSpec:
      let
        serviceConfig = nodeSpecServiceConfig    nodeSpec;
        service       = nodeServiceConfigService serviceConfig;
      in {
        nodeSpec = {
          value = nodeSpec;
          JSON  = runJq "node-spec-${name}.json"
                    ''--null-input --sort-keys
                      --argjson x '${__toJSON nodeSpec}'
                    '' "$x";
        };

        serviceConfig = {
          value = serviceConfig;
          JSON  = runJq "node-service-config-${name}.json"
                    ''--null-input --sort-keys
                      --argjson x '${__toJSON serviceConfig}'
                    '' "$x";
        };

        service = {
          value = service;
          JSON  = runJq "node-service-${name}.json"
                    ''--null-input --sort-keys
                      --argjson x '${__toJSON service}'
                    '' "$x";
        };

        nodeConfig = {
          value = service.nodeConfig;
          JSON  = runJq "node-config-${name}.json"
                    ''--null-input --sort-keys
                      --argjson x '${__toJSON service.nodeConfig}'
                    '' "$x";
        };

        topology = rec {
          JSON  = backend.topologyForNode { inherit profile nodeSpec; };
          value = __fromJSON (__readFile JSON);
        };

        startupScript =
          pkgs.writeScript "startup-${name}.sh"
            ''
            #!${pkgs.stdenv.shell}

            ${service.script}
            '';
      })
    profile.node-specs.value;
in
{
  inherit node-services;
}
