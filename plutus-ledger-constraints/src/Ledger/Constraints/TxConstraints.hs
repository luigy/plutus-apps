{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
-- | Constraints for transactions
module Ledger.Constraints.TxConstraints where

import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Map qualified as Map
import GHC.Generics (Generic)
import Prettyprinter (Pretty (pretty, prettyList), hang, viaShow, vsep, (<+>))

import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Prelude (Bool (False, True), Foldable (foldMap), Functor (fmap), Integer, JoinSemiLattice ((\/)),
                         Maybe (Just, Nothing), Monoid (mempty), Semigroup ((<>)), any, concatMap, foldl, mapMaybe, not,
                         null, ($), (.), (>>=), (||))

import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Interval qualified as I
import Plutus.V1.Ledger.Scripts (Datum (Datum), DatumHash, MintingPolicyHash, Redeemer, ValidatorHash, unitRedeemer)
import Plutus.V1.Ledger.Time (POSIXTimeRange)
import Plutus.V1.Ledger.Tx (TxOutRef)
import Plutus.V1.Ledger.Value (TokenName, Value, isZero)
import Plutus.V1.Ledger.Value qualified as Value

import Prelude qualified as Haskell

-- | Constraints on transactions that want to spend script outputs
data TxConstraint =
    MustIncludeDatum Datum
    | MustValidateIn POSIXTimeRange
    | MustBeSignedBy PubKeyHash
    | MustSpendAtLeast Value
    | MustProduceAtLeast Value
    | MustSpendPubKeyOutput TxOutRef
    | MustSpendScriptOutput TxOutRef Redeemer
    | MustMintValue MintingPolicyHash Redeemer TokenName Integer
    | MustPayToPubKey PubKeyHash (Maybe Datum) Value
    | MustPayToOtherScript ValidatorHash Datum Value
    | MustHashDatum DatumHash Datum
    | MustSatisfyAnyOf [TxConstraint]
    deriving stock (Haskell.Show, Generic, Haskell.Eq)
    deriving anyclass (ToJSON, FromJSON)

instance Pretty TxConstraint where
    pretty = \case
        MustIncludeDatum dv ->
            hang 2 $ vsep ["must include datum:", pretty dv]
        MustValidateIn range ->
            "must validate in:" <+> viaShow range
        MustBeSignedBy signatory ->
            "must be signed by:" <+> pretty signatory
        MustSpendAtLeast vl ->
            hang 2 $ vsep ["must spend at least:", pretty vl]
        MustProduceAtLeast vl ->
            hang 2 $ vsep ["must produce at least:", pretty vl]
        MustSpendPubKeyOutput ref ->
            hang 2 $ vsep ["must spend pubkey output:", pretty ref]
        MustSpendScriptOutput ref red ->
            hang 2 $ vsep ["must spend script output:", pretty ref, pretty red]
        MustMintValue mps red tn i ->
            hang 2 $ vsep ["must mint value:", pretty mps, pretty red, pretty tn <+> pretty i]
        MustPayToPubKey pk datum v ->
            hang 2 $ vsep ["must pay to pubkey:", pretty pk, pretty datum, pretty v]
        MustPayToOtherScript vlh dv vl ->
            hang 2 $ vsep ["must pay to script:", pretty vlh, pretty dv, pretty vl]
        MustHashDatum dvh dv ->
            hang 2 $ vsep ["must hash datum:", pretty dvh, pretty dv]
        MustSatisfyAnyOf xs ->
            hang 2 $ vsep ["must satisfy any of:", prettyList xs]

data InputConstraint a =
    InputConstraint
        { icRedeemer :: a
        , icTxOutRef :: TxOutRef
        } deriving stock (Haskell.Show, Generic, Haskell.Functor)

addTxIn :: TxOutRef -> i -> TxConstraints i o -> TxConstraints i o
addTxIn outRef red tc =
    let ic = InputConstraint{icRedeemer = red, icTxOutRef = outRef}
    in tc { txOwnInputs = ic : txOwnInputs tc }

instance (Pretty a) => Pretty (InputConstraint a) where
    pretty InputConstraint{icRedeemer, icTxOutRef} =
        vsep
            [ "Redeemer:" <+> pretty icRedeemer
            , "TxOutRef:" <+> pretty icTxOutRef
            ]

deriving anyclass instance (ToJSON a) => ToJSON (InputConstraint a)
deriving anyclass instance (FromJSON a) => FromJSON (InputConstraint a)
deriving stock instance (Haskell.Eq a) => Haskell.Eq (InputConstraint a)

data OutputConstraint a =
    OutputConstraint
        { ocDatum :: a
        , ocValue :: Value
        } deriving stock (Haskell.Show, Generic, Haskell.Functor)

instance (Pretty a) => Pretty (OutputConstraint a) where
    pretty OutputConstraint{ocDatum, ocValue} =
        vsep
            [ "Datum:" <+> pretty ocDatum
            , "Value:" <+> pretty ocValue
            ]

deriving anyclass instance (ToJSON a) => ToJSON (OutputConstraint a)
deriving anyclass instance (FromJSON a) => FromJSON (OutputConstraint a)
deriving stock instance (Haskell.Eq a) => Haskell.Eq (OutputConstraint a)

-- | Restrictions placed on the allocation of funds to outputs of transactions.
data TxConstraints i o =
    TxConstraints
        { txConstraints :: [TxConstraint]
        , txOwnInputs   :: [InputConstraint i]
        , txOwnOutputs  :: [OutputConstraint o]
        }
    deriving stock (Haskell.Show, Generic)

instance Bifunctor TxConstraints where
    bimap f g txc =
        txc
            { txOwnInputs = Haskell.fmap (Haskell.fmap f) (txOwnInputs txc)
            , txOwnOutputs = Haskell.fmap (Haskell.fmap g) (txOwnOutputs txc)
            }

type UntypedConstraints = TxConstraints PlutusTx.BuiltinData PlutusTx.BuiltinData

instance Semigroup (TxConstraints i o) where
    l <> r =
        TxConstraints
            { txConstraints = txConstraints l <> txConstraints r
            , txOwnInputs = txOwnInputs l <> txOwnInputs r
            , txOwnOutputs = txOwnOutputs l <> txOwnOutputs r
            }

instance Haskell.Semigroup (TxConstraints i o) where
    (<>) = (<>) -- uses PlutusTx.Semigroup instance

instance Monoid (TxConstraints i o) where
    mempty = TxConstraints [] [] []

instance Haskell.Monoid (TxConstraints i o) where
    mappend = (<>)
    mempty  = mempty

deriving anyclass instance (ToJSON i, ToJSON o) => ToJSON (TxConstraints i o)
deriving anyclass instance (FromJSON i, FromJSON o) => FromJSON (TxConstraints i o)
deriving stock instance (Haskell.Eq i, Haskell.Eq o) => Haskell.Eq (TxConstraints i o)

{-# INLINABLE singleton #-}
singleton :: TxConstraint -> TxConstraints i o
singleton a = mempty { txConstraints = [a] }

{-# INLINABLE mustValidateIn #-}
-- | @mustValidateIn r@ requires the transaction's time range to be contained
--   in @r@.
mustValidateIn :: forall i o. POSIXTimeRange -> TxConstraints i o
mustValidateIn = singleton . MustValidateIn

{-# INLINABLE mustBeSignedBy #-}
-- | Require the transaction to be signed by the public key.
mustBeSignedBy :: forall i o. PubKeyHash -> TxConstraints i o
mustBeSignedBy = singleton . MustBeSignedBy

{-# INLINABLE mustIncludeDatum #-}
-- | Require the transaction to include a datum.
mustIncludeDatum :: forall i o. Datum -> TxConstraints i o
mustIncludeDatum = singleton . MustIncludeDatum

{-# INLINABLE mustPayToTheScript #-}
-- | Lock the value with a script
mustPayToTheScript :: forall i o. PlutusTx.ToData o => o -> Value -> TxConstraints i o
mustPayToTheScript dt vl =
    TxConstraints
        { txConstraints = [MustIncludeDatum (Datum $ PlutusTx.toBuiltinData dt)]
        , txOwnInputs = []
        , txOwnOutputs = [OutputConstraint dt vl]
        }

{-# INLINABLE mustPayToPubKey #-}
-- | Lock the value with a public key
mustPayToPubKey :: forall i o. PubKeyHash -> Value -> TxConstraints i o
mustPayToPubKey pk = singleton . MustPayToPubKey pk Nothing

{-# INLINABLE mustPayWithDatumToPubKey #-}
-- | Lock the value and datum with a public key
mustPayWithDatumToPubKey :: forall i o. PubKeyHash -> Datum -> Value -> TxConstraints i o
mustPayWithDatumToPubKey pk datum = singleton . MustPayToPubKey pk (Just datum)

{-# INLINABLE mustPayToOtherScript #-}
-- | Lock the value with a public key
mustPayToOtherScript :: forall i o. ValidatorHash -> Datum -> Value -> TxConstraints i o
mustPayToOtherScript vh dv vl =
    singleton (MustPayToOtherScript vh dv vl)
    <> singleton (MustIncludeDatum dv)

{-# INLINABLE mustMintValue #-}
-- | Create the given value
mustMintValue :: forall i o. Value -> TxConstraints i o
mustMintValue = mustMintValueWithRedeemer unitRedeemer

{-# INLINABLE mustMintValueWithRedeemer #-}
-- | Create the given value
mustMintValueWithRedeemer :: forall i o. Redeemer -> Value -> TxConstraints i o
mustMintValueWithRedeemer red = foldMap valueConstraint . (AssocMap.toList . Value.getValue) where
    valueConstraint (currencySymbol, mp) =
        let hs = Value.currencyMPSHash currencySymbol in
        foldMap (Haskell.uncurry (mustMintCurrencyWithRedeemer hs red)) (AssocMap.toList mp)

{-# INLINABLE mustMintCurrency #-}
-- | Create the given amount of the currency
mustMintCurrency :: forall i o. MintingPolicyHash -> TokenName -> Integer -> TxConstraints i o
mustMintCurrency mps = mustMintCurrencyWithRedeemer mps unitRedeemer

{-# INLINABLE mustMintCurrencyWithRedeemer #-}
-- | Create the given amount of the currency
mustMintCurrencyWithRedeemer :: forall i o. MintingPolicyHash -> Redeemer -> TokenName -> Integer -> TxConstraints i o
mustMintCurrencyWithRedeemer mps red tn = singleton . MustMintValue mps red tn

{-# INLINABLE mustSpendAtLeast #-}
-- | Requirement to spend inputs with at least the given value
mustSpendAtLeast :: forall i o. Value -> TxConstraints i o
mustSpendAtLeast = singleton . MustSpendAtLeast

{-# INLINABLE mustProduceAtLeast #-}
-- | Requirement to produce outputs with at least the given value
mustProduceAtLeast :: forall i o. Value -> TxConstraints i o
mustProduceAtLeast = singleton . MustProduceAtLeast

{-# INLINABLE mustSpendPubKeyOutput #-}
mustSpendPubKeyOutput :: forall i o. TxOutRef -> TxConstraints i o
mustSpendPubKeyOutput = singleton . MustSpendPubKeyOutput

{-# INLINABLE mustSpendScriptOutput #-}
mustSpendScriptOutput :: forall i o. TxOutRef -> Redeemer -> TxConstraints i o
mustSpendScriptOutput txOutref = singleton . MustSpendScriptOutput txOutref

{-# INLINABLE mustHashDatum #-}
mustHashDatum :: DatumHash -> Datum -> TxConstraints i o
mustHashDatum dvh = singleton . MustHashDatum dvh

{-# INLINABLE mustSatisfyAnyOf #-}
mustSatisfyAnyOf :: forall i o. [TxConstraints i o] -> TxConstraints i o
mustSatisfyAnyOf = singleton . MustSatisfyAnyOf . concatMap txConstraints

{-# INLINABLE isSatisfiable #-}
-- | Are the constraints satisfiable?
isSatisfiable :: forall i o. TxConstraints i o -> Bool
isSatisfiable TxConstraints{txConstraints} =
    let intervals = mapMaybe (\case { MustValidateIn i -> Just i; _ -> Nothing }) txConstraints
        itvl = foldl I.intersection I.always intervals
    in not (I.isEmpty itvl)

{-# INLINABLE pubKeyPayments #-}
pubKeyPayments :: forall i o. TxConstraints i o -> [(PubKeyHash, Value)]
pubKeyPayments TxConstraints{txConstraints} =
    Map.toList
    $ Map.fromListWith (<>)
      (txConstraints >>= \case { MustPayToPubKey pk _ vl -> [(pk, vl)]; _ -> [] })

-- | The minimum 'Value' that satisfies all 'MustSpendAtLeast' constraints
{-# INLINABLE mustSpendAtLeastTotal #-}
mustSpendAtLeastTotal :: forall i o. TxConstraints i o -> Value
mustSpendAtLeastTotal = foldl (\/) mempty . fmap f . txConstraints where
    f (MustSpendAtLeast v) = v
    f _                    = mempty

-- | The minimum 'Value' that satisfies all 'MustProduceAtLeast' constraints
{-# INLINABLE mustProduceAtLeastTotal #-}
mustProduceAtLeastTotal :: forall i o. TxConstraints i o -> Value
mustProduceAtLeastTotal = foldl (\/) mempty . fmap f . txConstraints where
    f (MustProduceAtLeast v) = v
    f _                      = mempty

{-# INLINABLE requiredSignatories #-}
requiredSignatories :: forall i o. TxConstraints i o -> [PubKeyHash]
requiredSignatories = foldMap f . txConstraints where
    f (MustBeSignedBy pk) = [pk]
    f _                   = []

{-# INLINABLE requiredMonetaryPolicies #-}
requiredMonetaryPolicies :: forall i o. TxConstraints i o -> [MintingPolicyHash]
requiredMonetaryPolicies = foldMap f . txConstraints where
    f (MustMintValue mps _ _ _) = [mps]
    f _                         = []

{-# INLINABLE requiredDatums #-}
requiredDatums :: forall i o. TxConstraints i o -> [Datum]
requiredDatums = foldMap f . txConstraints where
    f (MustIncludeDatum dv) = [dv]
    f _                     = []

{-# INLINABLE modifiesUtxoSet #-}
-- | Check whether every transaction that satisfies the constraints has to
--   modify the UTXO set.
modifiesUtxoSet :: forall i o. TxConstraints i o -> Bool
modifiesUtxoSet TxConstraints{txConstraints, txOwnOutputs, txOwnInputs} =
    let requiresInputOutput = \case
            MustSpendAtLeast{}          -> True
            MustProduceAtLeast{}        -> True
            MustSpendPubKeyOutput{}     -> True
            MustSpendScriptOutput{}     -> True
            MustMintValue{}             -> True
            MustPayToPubKey _ _ vl      -> not (isZero vl)
            MustPayToOtherScript _ _ vl -> not (isZero vl)
            MustSatisfyAnyOf xs         -> any requiresInputOutput xs
            _                           -> False
    in any requiresInputOutput txConstraints
        || not (null txOwnOutputs)
        || not (null txOwnInputs)
