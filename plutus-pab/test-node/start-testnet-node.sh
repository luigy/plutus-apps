#!/usr/bin/env bash

/nix/store/2yh84sahysrknq3zc2n53kq5ywv6mhfp-cardano-node-exe-cardano-node-1.31.0/bin/cardano-node run \
    --config testnet/testnet-config.json \
    --topology testnet/testnet-topology.json \
    --database-path testnet/db \
    --socket-path testnet/node.sock \
    --port 3003
