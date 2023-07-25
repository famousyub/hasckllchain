self: final: prev: with self.legacyPackages.${final.system}; {
  inherit bcc-node bcc-cli bcc-ping bech32 db-converter bccLib
    bcc-node-profiled bcc-node-eventlogged bcc-node-asserted
    tx-generator tx-generator-profiled locli-profiled;
}
