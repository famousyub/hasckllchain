{ pkgs
, customConfigs ? [ pkgs.customConfig ]
}:
with pkgs.commonLib;
let
  mkScript = envConfig: let
    service = evalService {
      inherit pkgs customConfigs;
      serviceName = "bcc-submit-api";
      modules = [
        ./nixos/bcc-submit-api-service.nix
        {
          services.bcc-submit-api = {
            network = mkDefault envConfig.name;
            bccNodePkgs = mkDefault pkgs;
          };
        }
      ];
    };

  in pkgs.writeScript "bcc-submit-api-${service.network}" ''
    #!${pkgs.runtimeShell}
    set -euo pipefail
    ${service.script} $@
  '';

  scripts = forEnvironments (environment: recurseIntoAttrs {
    submit-api = mkScript environment;
  });
in scripts
