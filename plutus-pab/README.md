# Plutus Application Backend

_PAB_ (Plutus Application Backend) is an Off-Chain application for managing the state of
Plutus contract instances.

## Table of Contents

- [Building](#building)
- [Running](#running)
- [PAB Components](#pab-components)

## Building

_PAB_ is a Cabal based Haskell project. Production builds are done with Nix using [haskell.nix](https://github.com/input-output-hk/haskell.nix)

### Cabal

```
$ cabal build
```

### Nix

```
$ nix-build ../default.nix -A plutus-apps.haskell.packages.plutus-pab
```

## PAB Components
PAB contains several commands and services, which are outlined below.

- [pab-local-cluster](#pab-local-cluster)
- [psgenerator](#psgenerator)
- [migrate](#migrate)
- [all-servers](#all-servers)
- [client-services](#client-services)
- [wallet-server](#wallet-server)
- [webserver](#webserver)
- [node-server](#node-server)
- [chain-index](#chain-index)
- [default-logging-config](#default-logging-config)


### pab-local-cluster

#### Description

Can be used to run end-to-end tests using a private local testnet.

#### Example

1. Build necessary components:

```
> cabal build plutus-pab-local-cluster cardano-node cardano-wallet
```

2. Get config data:

  Clone <https://github.com/input-output-hk/cardano-wallet/> to $DIR and set the
  `SHELLEY_TEST_DATA` environment variable:

  ```
  export SHELLEY_TEST_DATA=$DIR/lib/shelley/test/data/cardano-node-shelley
  ```

3. Run the local cluster:

```
> cabal exec plutus-pab-local-cluster
```

4. Wait until the message `Starting PAB backend server on port 9080` appears.

5. Run the integration test:

```
curl -H "Content-Type: application/json" -v \
       -X POST \
       -d '{"caID":{"tag":"IntegrationTest"},"caWallet":{"getWalletId":"2d4cc31a4b3116ab86bfe529d30d9c362acd0b44"}}' \
       localhost:9080/api/contract/activate
```

A couple of log messages appear, the last one should say something like "Tx
confirmed. Integration test complete."


### psgenerator

```
$ cabal run plutus-pab -- psgenerator
```

#### Description

Generates the purescript bridge code.

#### Source

- [src/Plutus/PAB/Run/PSGenerator.hs](https://github.com/input-output-hk/plutus-apps/blob/main/plutus-pab/src/Plutus/PAB/Run/PSGenerator.hs)

### migrate

```
$ cabal run plutus-pab -- migrate
```

#### Description

Migrates the database in `pab-core.db` to the current schema.  The database contains the state for the contract instances.

#### Source
[Plutus.PAB.App.migrate](https://github.com/input-output-hk/plutus-apps/blob/main/plutus-pab/src/Plutus/PAB/App.hs#L260)


### all-servers

```
$ cabal run plutus-pab -- all-servers
```

#### Description

Combines the execution of all core services and mocks in the appropriate order:

* mocks
    - mock node
    - mock wallet
* core services
    - PAB webserver
    - chain index

#### Dependencies

- plutus-pab.yaml
- sqlite database
- pab-client


#### Source

- [src/Plutus/PAB/Run/CommandParser.hs](https://github.com/input-output-hk/plutus-apps/blob/main/plutus-pab/src/Plutus/PAB/Run/CommandParser.hs#L155-L164)

### client-services

```
$ cabal run plutus-pab -- client-services
```

#### Description

Starts all mocks and core services *except for* the mock node service:

* mocks
    - mock wallet
* core services
    - PAB webserver
    - chain index

#### Dependencies

- plutus-pab.yaml
- sqlite database
- pab-client


#### Source

- [src/Plutus/PAB/Run/CommandParser.hs](https://github.com/input-output-hk/plutus-apps/blob/main/plutus-pab/src/Plutus/PAB/Run/CommandParser.hs#L166-L175)

### wallet-server

```
$ cabal run plutus-pab -- wallet-server
```

#### Description

Plutus specific wallet implementation for managing user funds on the blockchain. Clients to this service are:

- PAB: adding funds to transactions & signing transactions
- user: making payments

#### Dependencies

- plutus-pab.yaml
- mock node

#### Source

- [Cardano.Wallet.Mock.API](https://github.com/input-output-hk/plutus-apps/blob/main/plutus-pab/src/Cardano/Wallet/Mock/API.hs)
- [Cardano.Wallet.Mock.Server.main](https://github.com/input-output-hk/plutus-apps/blob/main/plutus-pab/src/Cardano/Wallet/Mock/Server.hs#L70)
- [Cardano.Wallet.Mock.Types.WalletConfig](https://github.com/input-output-hk/plutus-apps/blob/main/plutus-pab/src/Cardano/Wallet/Mock/Types.hs#L110)

### webserver

```
$ cabal run plutus-pab -- webserver
```

#### Description

Serves the PAB user interface

#### Dependencies

- plutus-pab.yaml
- sqlite database
- pab-client

#### Source

- [Plutus.PAB.Webserver.API.API](https://github.com/input-output-hk/plutus-apps/blob/main/plutus-pab/src/Plutus/PAB/Webserver/API.hs#L35)
- [Plutus.PAB.Webserver.Server](https://github.com/input-output-hk/plutus-apps/blob/main/plutus-pab/src/Plutus/PAB/Webserver/Server.hs)

### node-server

```
$ cabal run plutus-pab -- node-server
```

#### Description

Mock-implementation of a Goguen node. Clients to this service are:

- chain-index
- webserver
- mock wallet

#### Dependencies

- plutus-pab.yaml

#### Source

- [Cardano.Node.API.API](https://github.com/input-output-hk/plutus-apps/blob/main/plutus-pab/src/Cardano/Node/API.hs#L14)
- [Cardano.Node.Server.main](https://github.com/input-output-hk/plutus-apps/blob/main/plutus-pab/src/Cardano/Node/Server.hs#L56)

### chain-index

```
$ cabal run plutus-pab -- chain-index
```

#### Description

Provides a consistent view of the current state of the blockchain including a key-value store
for datum and script hashes. Clients to this service are:

- process-outboxes

#### Dependencies

- plutus-pab.yaml
- mock node

#### Source

- [Plutus.ChainIndex.Api.API](https://github.com/input-output-hk/plutus-apps/blob/main/plutus-chain-index/src/Plutus/ChainIndex/Api.hs#L106)
- [Cardano.ChainIndex.Server.main](https://github.com/input-output-hk/plutus-apps/blob/main/plutus-pab/src/Cardano/ChainIndex/Server.hs#L37)

### default-logging-config

```
$ cabal run plutus-pab -- default-logging-config
```

#### Description

Prints the default logging configuration to STDOUT

#### Source

- [app/CommandParser.hs](https://github.com/input-output-hk/plutus-apps/blob/main/plutus-pab/app/CommandParser.hs#L59)
- [app/Main.hs](https://github.com/input-output-hk/plutus-apps/blob/main/plutus-pab/app/Main.hs#L28)
