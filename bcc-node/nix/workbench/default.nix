{ lib
, stdenv
, pkgs
, git
, graphviz
, jq
, moreutils
, makeWrapper
, runCommand
, customConfig
, bcc-cli
, bcc-topology
, locli

, useCabalRun
}:

with lib; with customConfig.localCluster;

let
  nixWbMode =
    if useCabalRun
    then "cabal-exes+nix-wb"
    else "nix-exes+nix-wb";

  workbench' = tools:
    stdenv.mkDerivation {
      pname = "workbench";

      version = "0.1";

      src = ./.;

      buildInputs = [ jq makeWrapper ];

      buildPhase = ''
        patchShebangs .
      '';

      postFixup = ''
        wrapProgram "$out/bin/wb" --argv0 wb --add-flags "--set-mode ${nixWbMode}" \
        --prefix PATH ":" ${pkgs.lib.makeBinPath tools}
      '';

      installPhase = ''
        mkdir -p         $out/bin
        cp -a wb profiles *.sh *.jq $out/bin
      '';

      dontStrip = true;
    };

  workbench = workbench'
    [ git graphviz
      jq
      moreutils

      bcc-cli
      bcc-topology
      locli
    ];

  runWorkbench =
    name: command:
    runCommand name {} ''
      ${workbench}/bin/wb ${command} > $out
    '';

  runWorkbenchJqOnly =
    name: command:
    runCommand name {} ''
      ${workbench' [jq moreutils]}/bin/wb ${command} > $out
    '';

  runJq =
    name: args: query:
    runCommand name {} ''
      args=(${args})
      ${jq}/bin/jq '${query}' "''${args[@]}" > $out
    '';

  exeCabalOp = op: exe:
    toString [ "cabal" "-v0" op "--" "exe:${exe}"];

  checkoutWbMode =
    if useCabalRun
    then "cabal-exes+checkout-wb"
    else "nix-exes+checkout-wb";

  shellHook = ''
    export WORKBENCH_BACKEND=supervisor

      ${optionalString workbenchDevMode
    ''
    echo 'workbench:  dev mode enabled, calling wb directly from checkout (instead of using Nix store)' >&2

    WORKBENCH_BCC_NODE_REPO_ROOT=$(git rev-parse --show-toplevel)
    WORKBENCH_EXTRA_FLAGS=

    function wb() {
      $WORKBENCH_BCC_NODE_REPO_ROOT/nix/workbench/wb --set-mode ${checkoutWbMode} $WORKBENCH_EXTRA_FLAGS "$@"
    }

    export WORKBENCH_BCC_NODE_REPO_ROOT WORKBENCH_EXTRA_FLAGS
    export -f wb

    ''}

    ${optionalString useCabalRun
    ''
    echo 'workbench:  cabal-inside-nix-shell mode enabled, calling bcc-* via 'cabal run' (instead of using Nix store)' >&2

    function bcc-cli() {
      ${exeCabalOp "run" "bcc-cli"} "$@"
    }

    function bcc-node() {
      ${exeCabalOp "run" "bcc-node"} "$@"
    }

    function bcc-topology() {
      ${exeCabalOp "run" "bcc-topology"} "$@"
    }

    function locli() {
      ${exeCabalOp "run" "locli"} "$@"
    }

    function tx-generator() {
      ${exeCabalOp "run" "tx-generator"} "$@"
    }

    export -f bcc-cli bcc-node bcc-topology locli tx-generator

    ''}

    function workbench-prebuild-executables() {
      ${optionalString useCabalRun
        ''
      git log -n1 --alternate-refs --pretty=format:"%Cred%cr %Cblue%h %Cgreen%D %Cblue%s%Creset" --color | sed "s/^/$(git diff --exit-code --quiet && echo ' ' || echo '[31mlocal changes + ')/"
      echo
      echo -n "workbench:  prebuilding executables (because of useCabalRun): "
      for exe in tx-generator bcc-cli bcc-node bcc-topology locli
      do echo -n "$exe "
         ${exeCabalOp "build" "$exe"} 2>&1 >/dev/null | { grep -v 'Temporary modify'; true; } || return 1
      done
      echo
        ''}
      true
    }
    export -f workbench-prebuild-executables

    export BCC_NODE_SOCKET_PATH=run/current/node-0/node.socket
    '';

  generateProfiles =
    { pkgs

    ## The backend is an attrset of AWS/supervisord-specific methods and parameters.
    , backend

    ## Environment arguments:
    ##   - either affect semantics on all backends equally,
    ##   - or have no semantic effect
    , envArgs
    }:
    rec {
      profile-names-json =
        runWorkbenchJqOnly "profile-names.json" "profiles list";

      profile-names =
        __fromJSON (__readFile profile-names-json);

      environment =
        ## IMPORTANT:  keep in sync with envArgs in 'supervisord-cluster/default.nix/envArgs'.
        with envArgs; rec {
          inherit bccLib stateDir;

          JSON = runWorkbenchJqOnly "environment.json"
          ''env compute-config \
            --cache-dir "${cacheDir}" \
            --base-port ${toString basePort} \
            ${optionalString staggerPorts "--stagger-ports"} \
          '';
          value = __fromJSON (__readFile JSON);
        };

      mkProfile =
        profileName:
        pkgs.callPackage ./profiles
          { inherit
              pkgs
              runWorkbenchJqOnly runJq workbench
              backend
              environment
              profileName;
          };

      profiles = genAttrs profile-names mkProfile;

      profilesJSON =
        runWorkbenchJqOnly "all-profiles.json" "profiles generate-all";
    };

  initialiseProfileRunDirShellScript =
    profile: runDir:
      __concatStringsSep "\n"
      (flip mapAttrsToList profile.node-services
        (name: svc:
          ''
          cp -f ${svc.serviceConfig.JSON} ${runDir}/${name}/service-config.json
          cp -f ${svc.nodeConfig.JSON}    ${runDir}/${name}/config.json
          cp -f ${svc.topology.JSON}      ${runDir}/${name}/topology.json
          cp -f ${svc.startupScript}      ${runDir}/${name}/start.sh
          ''
        )
      ++
      [ (let svc = profile.generator-service;
         in
          ''
          cp -f ${svc.runScript.JSON}     ${runDir}/generator/run-script.json
          cp -f ${svc.serviceConfig.JSON} ${runDir}/generator/service-config.json
          cp -f ${svc.nodeConfig.JSON}    ${runDir}/generator/config.json
          cp -f ${svc.startupScript}      ${runDir}/generator/start.sh
          '')
      ]);
in
{
  inherit workbench runWorkbench runJq;

  inherit generateProfiles initialiseProfileRunDirShellScript shellHook;
}
