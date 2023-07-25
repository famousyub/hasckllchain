{ pkgs, ... }:
with pkgs;
{
  name = "bcc-node-edge-test";
  nodes = {
    machine = { config, ... }: {
      nixpkgs.pkgs = pkgs;
      imports = [
        ../.
      ];
      services.bcc-node = {
        enable = true;
        systemdSocketActivation = true;
        port = 3001;
        hostAddr = "127.0.0.1";
        environment = "mainnet";
        topology = commonLib.mkEdgeTopology {
          port = 3001;
          edgeNodes = [ "127.0.0.1" ];
        };
        bccNodePkgs = pkgs;
        nodeConfig = config.services.bcc-node.environments.${config.services.bcc-node.environment}.nodeConfig // {
          hasPrometheus = [ config.services.bcc-node.hostAddr 12798 ];
          # Use Journald output:
          setupScribes = [{
            scKind = "JournalSK";
            scName = "bcc";
            scFormat = "ScText";
          }];
          defaultScribes = [
            [
              "JournalSK"
              "bcc"
            ]
          ];
        };
      };
      systemd.services.bcc-node.serviceConfig.Restart = lib.mkForce "no";
      services.bcc-submit-api = {
        enable = true;
        port = 8101;
        network = "mainnet";
        socketPath = config.services.bcc-node.socketPath;
        bccNodePkgs = pkgs;
      };
      systemd.services.bcc-submit-api.serviceConfig.SupplementaryGroups = "bcc-node";
    };
  };
  testScript = ''
    start_all()
    machine.wait_for_unit("bcc-node.service")
    machine.succeed("stat /run/bcc-node")
    machine.succeed("stat /run/bcc-node/node.socket")
    machine.wait_for_open_port(12798)
    machine.wait_for_open_port(3001)
    machine.succeed("systemctl status bcc-node")
    machine.succeed(
        "${bcc-ping}/bin/bcc-ping -h 127.0.0.1 -c 1 -q --json | ${jq}/bin/jq -c"
    )
    machine.wait_for_open_port(8101)
    machine.succeed("systemctl status bcc-submit-api")
  '';

}
