############################################################################
# Docker image builder
#
# To build and load into the Docker engine:
#
#   docker load -i $(nix-build -A dockerImage --no-out-link)
#
# To launch with pre-loaded configuration, using the NETWORK env.
# An example using a docker volume to persist state:
#
#   docker run -v /data -e NETWORK=mainnet tbco/bcc-node
#
# Provide a complete command otherwise:
#
#   docker run -v $PWD/configuration/defaults/cole-mainnet:/configuration \
#     tbco/bcc-node run \
#      --config /configuration/configuration.yaml \
#      --topology /configuration/topology.json \
#      --database-path /db
#
# Mount a volume into /ipc for establishing cross-container communication via node.socket
#
#   docker run -v node-ipc:/ipc tbco/bcc-node
#   docker run -v node-ipc:/ipc tbco/some-node-client
############################################################################

{ pkgs
, commonLib
, dockerTools

# The main contents of the image.
, bcc-cli
, bcc-node
, scripts

# Set gitrev to null, to ensure the version below is used
, gitrev ? null

# Other things to include in the image.
, bashInteractive
, buildPackages
, cacert
, coreutils
, curl
, glibcLocales
, iana-etc
, iproute
, iputils
, socat
, utillinux
, writeScriptBin
, runtimeShell
, lib
, exe
, script
, repoName ? "tbco/${exe}"
}:

let

  # Layer of tools which aren't going to change much between versions.
  baseImage = dockerTools.buildImage {
    name = "${repoName}-env";
    contents = [
      bcc-cli       # Provide bcc-cli capability
      bashInteractive   # Provide the BASH shell
      cacert            # X.509 certificates of public CA's
      coreutils         # Basic utilities expected in GNU OS's
      curl              # CLI tool for transferring files via URLs
      glibcLocales      # Locale information for the GNU C Library
      iana-etc          # IANA protocol and port number assignments
      iproute           # Utilities for controlling TCP/IP networking
      iputils           # Useful utilities for Linux networking
      socat             # Utility for bidirectional data transfer
      utillinux         # System utilities for Linux
    ];
    # set up /tmp (override with TMPDIR variable)
    extraCommands = ''
      mkdir -m 0777 tmp
    '';
  };
  # Image with all tbco-nix network configs or utilizes a configuration volume mount
  # To choose a network, use `-e NETWORK testnet`
  clusterStatements = lib.concatStringsSep "\n" (lib.mapAttrsToList (env: scripts: let
    scriptBin = scripts.${script};
    in ''
      elif [[ "$NETWORK" == "${env}" ]]; then
        exec ${scriptBin}/bin/${scriptBin.name} $@
    '') scripts);

  runNetwork = pkgs.writeShellScriptBin "run-network" ''
    if [[ -z "$NETWORK" ]]; then
      echo "[Error] Cannot obtain NETWORK env variable"
      exit 1
    ${clusterStatements}
    else
      echo "[Error] Managed configuration for network "$NETWORK" does not exist"
      exit 1
    fi
  '';

  # The Docker context with static content
  context = ./context;

  # Mainnet configuration used by the 'run' option
  mainnetConfigFile = builtins.toFile "mainnet-config.json"
    (builtins.toJSON commonLib.environments.mainnet.nodeConfig);
  mainnetTopologyFile = commonLib.mkEdgeTopology { edgeNodes = [ commonLib.environments.mainnet.relaysNew ]; valency = 2; };

in
  dockerTools.buildImage {
    name = "${repoName}";
    tag = "${gitrev}";
    fromImage = baseImage;
    created = "now";   # Set creation date to build time. Breaks reproducibility
    contents = [
    ];

    # May require system-features = kvm in /etc/nix/nix.conf
    # https://discourse.nixos.org/t/cannot-build-docker-image/7445
    # runAsRoot = '' ln -s ${bcc-node} bin/bcc-node '';

    extraCommands = ''
      mkdir -p opt/bcc/config
      mkdir -p opt/bcc/data
      mkdir -p opt/bcc/ipc
      mkdir -p opt/bcc/logs
      mkdir -p usr/local/bin
      ln -s ${mainnetConfigFile} opt/bcc/config/mainnet-config.json
      ln -s ${mainnetTopologyFile} opt/bcc/config/mainnet-topology.json
      cp ${runNetwork}/bin/* usr/local/bin
      cp ${context}/bin/* usr/local/bin
      ln -s ${bcc-node}/bin/bcc-node usr/local/bin/bcc-node
      ln -s ${bcc-cli}/bin/bcc-cli usr/local/bin/bcc-cli
    '';
    config = {
      EntryPoint = [ "entrypoint" ];
      StopSignal = "SIGINT";
    };
  }
