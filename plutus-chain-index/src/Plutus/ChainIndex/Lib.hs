{-# LANGUAGE OverloadedStrings #-}
{-| Using the chain index as a library.
-}
module Plutus.ChainIndex.Lib where

import Cardano.BM.Configuration.Model qualified as CM
import Cardano.BM.Setup (setupTrace_)
import Cardano.BM.Trace (Trace, logDebug)
import Control.Concurrent.STM qualified as STM
import Control.Monad.Freer.Extras.Beam (BeamLog (SqlLog))
import Database.Beam.Migrate.Simple (autoMigrate)
import Database.Beam.Sqlite qualified as Sqlite
import Database.Beam.Sqlite.Migrate qualified as Sqlite
import Database.SQLite.Simple qualified as Sqlite
import Plutus.ChainIndex (ChainIndexLog (BeamLogItem), RunRequirements (RunRequirements))
import Plutus.ChainIndex.Config qualified as Config
import Plutus.ChainIndex.DbSchema (checkedSqliteDb)
import Plutus.ChainIndex.Logging qualified as Logging

withRunRequirements :: CM.Configuration -> Config.ChainIndexConfig -> (RunRequirements -> IO ()) -> IO ()
withRunRequirements logConfig config cont = do
  Sqlite.withConnection (Config.cicDbPath config) $ \conn -> do

    (trace :: Trace IO ChainIndexLog, _) <- setupTrace_ logConfig "chain-index"

    -- Optimize Sqlite for write performance, halves the sync time.
    -- https://sqlite.org/wal.html
    Sqlite.execute_ conn "PRAGMA journal_mode=WAL"
    Sqlite.runBeamSqliteDebug (logDebug trace . (BeamLogItem . SqlLog)) conn $ do
      autoMigrate Sqlite.migrationBackend checkedSqliteDb

    -- Automatically delete the input when an output from a matching input/output pair is deleted.
    -- See reduceOldUtxoDb in Plutus.ChainIndex.Handlers
    Sqlite.execute_ conn "DROP TRIGGER IF EXISTS delete_matching_input"
    Sqlite.execute_ conn
      "CREATE TRIGGER delete_matching_input AFTER DELETE ON unspent_outputs \
      \BEGIN \
      \  DELETE FROM unmatched_inputs WHERE input_row_tip__row_slot = old.output_row_tip__row_slot \
      \                                 AND input_row_out_ref = old.output_row_out_ref; \
      \END"

    stateTVar <- STM.newTVarIO mempty
    cont $ RunRequirements trace stateTVar conn (Config.cicSecurityParam config)

withDefaultRunRequirements :: (RunRequirements -> IO ()) -> IO ()
withDefaultRunRequirements cont = do
    logConfig <- Logging.defaultConfig
    withRunRequirements logConfig Config.defaultConfig cont
