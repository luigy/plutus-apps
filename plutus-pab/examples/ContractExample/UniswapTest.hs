{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}

module ContractExample.UniswapTest where

import Data.Aeson (FromJSON, ToJSON)
-- import Data.Map qualified as Map
-- import Data.Semigroup qualified as Semigroup
import Data.Void (Void)
import GHC.Generics (Generic)
-- import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Constraints
-- import Ledger.Scripts (Datum (..), Redeemer (..), unitRedeemer)
-- import Ledger.Tx (getCardanoTxId)
-- import Ledger.Typed.Scripts as Scripts
import Ledger.Value as Value
import Plutus.Contract
import Plutus.Contract as Contract hiding (throwError)
import Plutus.Contracts.Currency qualified as Currency
import Plutus.Contracts.PubKey qualified as PubKey
import Prelude as Haskell

data IError =
    PKError PubKey.PubKeyError
    | CError ContractError
    | CurrencyError Currency.CurrencyError
    deriving stock (Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

run :: Contract () Currency.CurrencySchema IError ()
run = runError run' >>= \case
    Left err -> logWarn @Haskell.String (show err)
    Right () -> pure ()

run' :: Contract () Currency.CurrencySchema IError ()
run' = do
    logInfo @Haskell.String "Starting uniswap test"
    _ <- mapError CurrencyError $ setupTokens [("Obsidian", 1000000), ("Limu", 1000000)]
    return ()
    -- pkh <- mapError CError ownPubKeyHash
    -- (txOutRef, ciTxOut, pkInst) <- mapError PKError (PubKey.pubKeyContract pkh (Ada.adaValueOf 10))
    -- logInfo @Haskell.String "pubKey contract complete:"
    -- logInfo txOutRef
    -- let lookups =
    --         Constraints.otherData (Datum $ getRedeemer unitRedeemer)
    --         <> Constraints.unspentOutputs (maybe mempty (Map.singleton txOutRef) ciTxOut)
    --         <> Constraints.otherScript  (Scripts.validatorScript pkInst)
    --     constraints =
    --         Constraints.mustSpendScriptOutput txOutRef unitRedeemer
    --         <> Constraints.mustBeSignedBy pkh
    -- result <- runError @_ @_ @ContractError $ submitTxConstraintsWith @Scripts.Any lookups constraints
    -- case result of
    --     Left err -> do
    --         logWarn @Haskell.String "An error occurred. Integration test failed."
    --         logWarn err
    --     Right redeemingTx -> do
    --         let txi = getCardanoTxId redeemingTx
    --         logInfo @Haskell.String $ "Waiting for tx " <> show txi <> " to complete"
    --         mapError CError $ awaitTxConfirmed txi
    --         logInfo @Haskell.String "Tx confirmed. Integration test complete."

-- | Create some sample tokens and distribute them to the current wallet
-- setupTokens :: [(TokenName, Integer)] -> Contract (Maybe (Semigroup.Last Currency.OneShotCurrency)) Currency.CurrencySchema Currency.CurrencyError ()
setupTokens :: [(TokenName, Integer)] -> Contract () Currency.CurrencySchema Currency.CurrencyError ()
setupTokens tokenNames = do
    ownPK <- Contract.ownPubKeyHash
    cur   <- Currency.mintContract ownPK tokenNames
    let cs = Currency.currencySymbol cur
        v  = mconcat [Value.singleton cs tn amount | (tn, _) <- tokenNames]

    -- forM_ wallets $ \w -> do
    --     let pkh = walletPubKeyHash w
    --     when (pkh /= ownPK) $ do
    let pkh = ownPK
    mkTxConstraints @Void mempty (Constraints.mustPayToPubKey pkh v)
      >>= submitTxConfirmed . Constraints.adjustUnbalancedTx

    -- tell $ Just $ Semigroup.Last cur

  where
    amount = 1000000

