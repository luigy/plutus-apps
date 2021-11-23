{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module ContractExample.UniswapTest where

-- import Control.Lens (review)
import Control.Monad (return, (>>=))
import Data.Aeson (FromJSON, ToJSON)
import Data.Map qualified as Map
import Data.Maybe (Maybe (..))
-- import Data.Semigroup qualified as Semigroup
import Data.Monoid (mconcat, mempty, (<>))
import Data.Text qualified as Text
import Data.Void (Void)
import GHC.Generics (Generic)
import Ledger qualified as Ledger
-- import Ledger.Ada qualified as Ada
import Ledger.Address qualified as Address
import Ledger.Constraints qualified as Constraints
-- import Plutus.V1.Ledger.Tx (Tx (..), TxIn (..), TxOut (..), TxOutRef (..), TxOutTx (..))
-- import Ledger.Scripts (Datum (..), Redeemer (..), unitRedeemer)
import Ledger.Tx (TxOutRef (..), getCardanoTxId)
import Ledger.Typed.Scripts as Scripts
import Ledger.Value as Value
import Plutus.Contract
import Plutus.Contract as Contract hiding (throwError)
import Plutus.Contracts.Currency qualified as Currency
import Plutus.Contracts.PubKey qualified as PubKey
import Plutus.Contracts.Uniswap qualified as Uniswap
import PlutusTx.AssocMap qualified
import Prelude (Either (..), Eq, Integer, String, pure, show, ($), (.))
import Prelude qualified as Haskell
import Text.Groom (groom)

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
    -- pkh <- mapError CError ownPubKeyHash
    _ <- setupTokens
      [ (uniswapTokenName, 1)
      , (coinATokenName, 1000000)
      , (coinBTokenName, 1000000)
      ]
    return ()
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

-- TODO: This should be exported from Uniswap contract
uniswapTokenName :: TokenName
uniswapTokenName = "Uniswap"

-- TODO: This should be exported from Uniswap contract
coinATokenName :: TokenName
coinATokenName = "Obsidian"

-- TODO: This should be exported from Uniswap contract
coinBTokenName :: TokenName
coinBTokenName = "Limu"

-- | Create some sample tokens and distribute them to the current wallet
-- setupTokens :: [(TokenName, Integer)] -> Contract (Maybe (Semigroup.Last Currency.OneShotCurrency)) Currency.CurrencySchema Currency.CurrencyError ()
setupTokens :: [(TokenName, Integer)] -> Contract () Currency.CurrencySchema IError ()
setupTokens tokenNames = do
    ownPK <- mapError @_ @_ CError $ Contract.ownPubKeyHash
    cur   <- mintContract ownPK tokenNames

    let cs = Currency.currencySymbol cur
        v  = mconcat [Value.singleton cs tn amount | (tn, _) <- tokenNames]

    -- forM_ wallets $ \w -> do
    --     let pkh = walletPubKeyHash w
    --     when (pkh /= ownPK) $ do
    let pkh = ownPK
    mapError @_ @_ CError $ mkTxConstraints @Void mempty (Constraints.mustPayToPubKey pkh v)
      >>= submitTxConfirmed . Constraints.adjustUnbalancedTx

    -- tell $ Just $ Semigroup.Last cur

  where
    amount = 1000000

startUniswap :: Contract () s Text.Text Uniswap.Uniswap
startUniswap = do
    -- Setup all tokens and start Uniswap
    Uniswap.start


-- This is meant to run from start to finish
fullSwapTest :: Contract () s IError ()
fullSwapTest = mapError @_ @_ (CError . OtherError) $ do
    -- TODO: Mint tokens

    -- Initialize uniswap
    uniswap <- startUniswap

    let cp = Haskell.undefined
    -- { cpCoinA   :: Coin A   -- ^ One 'Coin' of the liquidity pair.
    -- , cpCoinB   :: Coin B   -- ^ The other 'Coin'.
    -- , cpAmountA :: Amount A -- ^ Amount of liquidity for the first 'Coin'.
    -- , cpAmountB :: Amount B -- ^ Amount of liquidity for the second 'Coin'.
    -- }
    _ <- Uniswap.create uniswap cp

    -- { spCoinA   :: Coin A         -- ^ One 'Coin' of the liquidity pair.
    -- , spCoinB   :: Coin B         -- ^ The other 'Coin'.
    -- , spAmountA :: Amount A       -- ^ The amount the first 'Coin' that should be swapped.
    -- , spAmountB :: Amount B       -- ^ The amount of the second 'Coin' that should be swapped.
    -- }
    let sp = Uniswap.SwapParams Haskell.undefined Haskell.undefined Haskell.undefined Haskell.undefined
    _ <- Uniswap.swap uniswap sp

    -- Done
    return ()

mintContract
    :: Ledger.PubKeyHash
    -> [(TokenName, Integer)]
    -> Contract w s IError Currency.OneShotCurrency
mintContract pkh amounts = do
    -- (txOutRef, _ciTxOut, _pkInst) <- mapError PKError (PubKey.pubKeyContract pkh (Ada.adaValueOf 3))
    utxos <- mapError @_ @_ CError $ utxosAt (Address.pubKeyHashAddress pkh)
    txOutRef <- case Map.lookupMin utxos of
        Nothing -> throwError $ CError $ OtherError $ Text.pack $ "No UTxO available " <> show (Address.pubKeyHashAddress pkh)
        Just (txOutRef, _) -> return txOutRef
    let theCurrency = mkCurrency txOutRef amounts
        curVali     = Currency.curPolicy theCurrency
        lookups     = Constraints.mintingPolicy curVali
                        <> Constraints.unspentOutputs utxos
        mintTx      = Constraints.mustSpendPubKeyOutput txOutRef
                        <> Constraints.mustMintValue (Currency.mintedValue theCurrency)
    mapError @_ @_ CError $ do
      unbalancedTx <- mkTxConstraints @Scripts.Any lookups mintTx
      logInfo @String "UnbalancedTx: "
      logInfo @String $ groom unbalancedTx
      balancedTx <- balanceTx unbalancedTx
      logInfo @String "BalancedTx: "
      logInfo @String $ groom balancedTx
      tx <- submitBalancedTx balancedTx
      -- tx <- submitUnbalancedTx unbalancedTx
      -- tx <- submitTxConstraintsWith  lookups mintTx
      _ <- awaitTxConfirmed (getCardanoTxId tx)
      pure theCurrency

mkCurrency :: TxOutRef -> [(TokenName, Integer)] -> Currency.OneShotCurrency
mkCurrency (TxOutRef h i) amts =
    Currency.OneShotCurrency
        { Currency.curRefTransactionOutput = (h, i)
        , Currency.curAmounts              = PlutusTx.AssocMap.fromList amts
        }
