{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-| Main entry points to the chain index.
-}
module Plutus.ChainIndex.App(main, runMain) where

import Control.Exception (throwIO)
import Control.Lens (unto)
import Control.Monad.Freer (Eff, reinterpret, runM, send)
import Control.Monad.Freer.Extras (raiseEnd)
import Control.Monad.Freer.Extras.Beam (BeamEffect)
import Control.Monad.Freer.Extras.Log (LogLevel (..), LogMessage (..), LogMsg (..), handleLogWriter)
import Control.Monad.Freer.Writer (runWriter)
import Control.Tracer (nullTracer)
import Data.Aeson qualified as A
import Data.Foldable (for_, traverse_)
import Data.Function ((&))
import Data.Functor (void)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Sequence (Seq, (|>))
import Data.Yaml qualified as Y
import Options.Applicative (execParser)
import Prettyprinter (Pretty (..))

import Cardano.BM.Configuration.Model qualified as CM
import Cardano.BM.Trace (logError)

import Cardano.Api qualified as C
import Cardano.Protocol.Socket.Client (ChainSyncEvent (..), runChainSync)
import Cardano.Protocol.Socket.Type (epochSlots)
import Plutus.ChainIndex (ChainIndexLog (..), RunRequirements (..), handleChainIndexEffects)
import Plutus.ChainIndex.CommandLine (AppConfig (..), Command (..), applyOverrides, cmdWithHelpParser)
import Plutus.ChainIndex.Compatibility (fromCardanoBlock, fromCardanoPoint, tipFromCardanoBlock)
import Plutus.ChainIndex.Config qualified as Config
import Plutus.ChainIndex.Effects (ChainIndexControlEffect (..), ChainIndexQueryEffect (..), appendBlock, resumeSync,
                                  rollback)
import Plutus.ChainIndex.Handlers (getResumePoints)
import Plutus.ChainIndex.Lib (withRunRequirements)
import Plutus.ChainIndex.Logging qualified as Logging
import Plutus.ChainIndex.Server qualified as Server
import Plutus.ChainIndex.Types (BlockProcessOption (..), pointSlot)
import Plutus.Monitoring.Util (runLogEffects)


runChainIndex
  :: RunRequirements
  -> Eff '[ChainIndexQueryEffect, ChainIndexControlEffect, BeamEffect] a
  -> IO (Maybe a)
runChainIndex runReq effect = do
  (errOrResult, logMessages') <-
    runM
    $ runWriter @(Seq (LogMessage ChainIndexLog))
    $ reinterpret
        (handleLogWriter @ChainIndexLog
                          @(Seq (LogMessage ChainIndexLog)) $ unto pure)
    $ handleChainIndexEffects runReq
    $ raiseEnd effect
  (result, logMessages) <- case errOrResult of
      Left err ->
        pure (Nothing, logMessages' |> LogMessage Error (Err err))
      Right result -> do
        pure (Just result, logMessages')
  -- Log all previously captured messages
  traverse_ (send . LMessage) logMessages
    & runLogEffects (trace runReq)
  pure result

chainSyncHandler
  :: RunRequirements
  -> Config.ChainIndexConfig
  -> IORef (Integer, Integer)
  -> ChainSyncEvent
  -> IO ()
chainSyncHandler runReq config lastProgressRef
  (RollForward block@(C.BlockInMode (C.Block (C.BlockHeader blockSlot _ blockNo) _) _) _) = do
    showProgress config lastProgressRef blockSlot
    let ciBlock = fromCardanoBlock block
    case ciBlock of
      Left err    ->
        logError (trace runReq) (ConversionFailed err)
      Right txs -> void $ runChainIndex runReq $
        appendBlock (tipFromCardanoBlock block) txs (BlockProcessOption (blockNo >= Config.cicStoreFrom config))
chainSyncHandler runReq _ _
  (RollBackward point _) = do
    putStr "Rolling back to "
    print point
    -- Do we really want to pass the tip of the new blockchain to the
    -- rollback function (rather than the point where the chains diverge)?
    void $ runChainIndex runReq $ rollback (fromCardanoPoint point)
chainSyncHandler runReq _ _
  (Resume point) = do
    putStr "Resuming from "
    print point
    void $ runChainIndex runReq $ resumeSync $ fromCardanoPoint point

showResumePoints :: [C.ChainPoint] -> String
showResumePoints = \case
  []  -> "none"
  [x] -> showPoint x
  xs  -> showPoint (head xs) ++ ", " ++ showPoint (xs !! 1) ++ " .. " ++ showPoint (last xs)
  where
    showPoint = show . toInteger . pointSlot . fromCardanoPoint

getTipSlot :: Config.ChainIndexConfig -> IO Integer
getTipSlot config = do
  C.ChainTip (C.SlotNo slotNo) _ _ <- C.getLocalChainTip $ C.LocalNodeConnectInfo
    { C.localConsensusModeParams = C.CardanoModeParams epochSlots
    , C.localNodeNetworkId = Config.cicNetworkId config
    , C.localNodeSocketPath = Config.cicSocketPath config
    }
  pure $ fromIntegral slotNo

showProgress :: Config.ChainIndexConfig -> IORef (Integer, Integer) -> C.SlotNo -> IO ()
showProgress config lastProgressRef (C.SlotNo blockSlot) = do
  (lastProgress, tipSlot) <- readIORef lastProgressRef
  let pct = (100 * fromIntegral blockSlot) `div` tipSlot
  if pct > lastProgress then do
    putStrLn $ "Syncing (" ++ show pct ++ "%)"
    newTipSlot <- getTipSlot config
    writeIORef lastProgressRef (pct, newTipSlot)
  else pure ()

main :: IO ()
main = do
  -- Parse comand line arguments.
  cmdConfig@AppConfig{acLogConfigPath, acConfigPath, acMinLogLevel, acCommand, acCLIConfigOverrides} <- execParser cmdWithHelpParser

  case acCommand of
    DumpDefaultConfig path ->
      A.encodeFile path Config.defaultConfig

    DumpDefaultLoggingConfig path ->
      Logging.defaultConfig >>= CM.toRepresentation >>= Y.encodeFile path

    StartChainIndex{} -> do
      -- Initialise logging
      logConfig <- maybe Logging.defaultConfig Logging.loadConfig acLogConfigPath
      for_ acMinLogLevel $ \ll -> CM.setMinSeverity logConfig ll

      -- Reading configuration file
      config <- applyOverrides acCLIConfigOverrides <$> case acConfigPath of
        Nothing -> pure Config.defaultConfig
        Just p  -> A.eitherDecodeFileStrict p >>=
          either (throwIO . Config.DecodeConfigException) pure

      putStrLn "\nCommand line config:"
      print cmdConfig

      putStrLn "\nLogging config:"
      CM.toRepresentation logConfig >>= print

      putStrLn "\nChain Index config:"
      print (pretty config)

      runMain logConfig config

runMain :: CM.Configuration -> Config.ChainIndexConfig -> IO ()
runMain logConfig config = do
  withRunRequirements logConfig config $ \runReq -> do

    Just resumePoints <- runChainIndex runReq getResumePoints

    putStr "\nPossible resume slots: "
    putStrLn $ showResumePoints resumePoints

    -- The primary purpose of this query is to get the first response of the node for potential errors before opening the DB and starting the chain index.
    -- See #69.
    putStr "\nThe tip of the local node: "
    slotNo <- getTipSlot config
    print slotNo
    progressRef <- newIORef (0, slotNo)

    putStrLn $ "Connecting to the node using socket: " <> Config.cicSocketPath config
    void $ runChainSync (Config.cicSocketPath config)
                        nullTracer
                        (Config.cicSlotConfig config)
                        (Config.cicNetworkId  config)
                        resumePoints
                        (\evt _ -> chainSyncHandler runReq config progressRef evt)

    let port = show (Config.cicPort config)
    putStrLn $ "Starting webserver on port " <> port
    putStrLn $ "A Swagger UI for the endpoints are available at "
            <> "http://localhost:" <> port <> "/swagger/swagger-ui"
    Server.serveChainIndexQueryServer (Config.cicPort config) runReq

