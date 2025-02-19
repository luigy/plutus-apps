{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE MonoLocalBinds     #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
module Ledger.Constraints.OnChain where

import PlutusTx (ToData (toBuiltinData))
import PlutusTx.Prelude (AdditiveSemigroup ((+)), Bool (False, True), Eq ((==)), Functor (fmap), Maybe (Just),
                         Ord ((<=), (>=)), all, any, elem, isJust, isNothing, maybe, snd, traceIfFalse, ($), (&&), (.))

import Ledger qualified
import Ledger.Constraints.TxConstraints (InputConstraint (InputConstraint, icTxOutRef),
                                         OutputConstraint (OutputConstraint, ocDatum, ocValue),
                                         TxConstraint (MustBeSignedBy, MustHashDatum, MustIncludeDatum, MustMintValue, MustPayToOtherScript, MustPayToPubKey, MustProduceAtLeast, MustSatisfyAnyOf, MustSpendAtLeast, MustSpendPubKeyOutput, MustSpendScriptOutput, MustValidateIn),
                                         TxConstraints (TxConstraints, txConstraints, txOwnInputs, txOwnOutputs))
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Ada qualified as Ada
import Plutus.V1.Ledger.Address qualified as Address
import Plutus.V1.Ledger.Contexts (ScriptContext (ScriptContext, scriptContextTxInfo),
                                  TxInInfo (TxInInfo, txInInfoOutRef, txInInfoResolved),
                                  TxInfo (txInfoData, txInfoInputs, txInfoMint, txInfoValidRange),
                                  TxOut (TxOut, txOutAddress, txOutDatumHash, txOutValue))
import Plutus.V1.Ledger.Contexts qualified as V
import Plutus.V1.Ledger.Interval (contains)
import Plutus.V1.Ledger.Value (leq)

{-# INLINABLE checkOwnInputConstraint #-}
checkOwnInputConstraint :: ScriptContext -> InputConstraint a -> Bool
checkOwnInputConstraint ScriptContext{scriptContextTxInfo} InputConstraint{icTxOutRef} =
    let checkInput TxInInfo{txInInfoOutRef} =
            txInInfoOutRef == icTxOutRef -- TODO: We should also check the redeemer but we can't right now because it's hashed
    in traceIfFalse "L0" -- "Input constraint"
    $ any checkInput (txInfoInputs scriptContextTxInfo)

{-# INLINABLE checkOwnOutputConstraint #-}
checkOwnOutputConstraint
    :: ToData o
    => ScriptContext
    -> OutputConstraint o
    -> Bool
checkOwnOutputConstraint ctx@ScriptContext{scriptContextTxInfo} OutputConstraint{ocDatum, ocValue} =
    let hsh = V.findDatumHash (Ledger.Datum $ toBuiltinData ocDatum) scriptContextTxInfo
        checkOutput TxOut{txOutValue, txOutDatumHash=Just svh} =
               Ada.fromValue txOutValue >= Ada.fromValue ocValue
            && Ada.fromValue txOutValue <= Ada.fromValue ocValue + Ledger.minAdaTxOut
            && Value.noAdaValue txOutValue == Value.noAdaValue ocValue
            && hsh == Just svh
        checkOutput _       = False
    in traceIfFalse "L1" -- "Output constraint"
    $ any checkOutput (V.getContinuingOutputs ctx)

{-# INLINABLE checkTxConstraint #-}
checkTxConstraint :: ScriptContext -> TxConstraint -> Bool
checkTxConstraint ctx@ScriptContext{scriptContextTxInfo} = \case
    MustIncludeDatum dv ->
        traceIfFalse "L2" -- "Missing datum"
        $ dv `elem` fmap snd (txInfoData scriptContextTxInfo)
    MustValidateIn interval ->
        traceIfFalse "L3" -- "Wrong validation interval"
        $ interval `contains` txInfoValidRange scriptContextTxInfo
    MustBeSignedBy pubKey ->
        traceIfFalse "L4" -- "Missing signature"
        $ scriptContextTxInfo `V.txSignedBy` pubKey
    MustSpendAtLeast vl ->
        traceIfFalse "L5" -- "Spent value not OK"
        $ vl `leq` V.valueSpent scriptContextTxInfo
    MustProduceAtLeast vl ->
        traceIfFalse "L6" -- "Produced value not OK"
        $ vl `leq` V.valueProduced scriptContextTxInfo
    MustSpendPubKeyOutput txOutRef ->
        traceIfFalse "L7" -- "Public key output not spent"
        $ maybe False (isNothing . txOutDatumHash . txInInfoResolved) (V.findTxInByTxOutRef txOutRef scriptContextTxInfo)
    MustSpendScriptOutput txOutRef _ ->
        traceIfFalse "L8" -- "Script output not spent"
        -- Unfortunately we can't check the redeemer, because TxInfo only
        -- gives us the redeemer's hash, but 'MustSpendScriptOutput' gives
        -- us the full redeemer
        $ isJust (V.findTxInByTxOutRef txOutRef scriptContextTxInfo)
    MustMintValue mps _ tn v ->
        traceIfFalse "L9" -- "Value minted not OK"
        $ Value.valueOf (txInfoMint scriptContextTxInfo) (Value.mpsSymbol mps) tn == v
    MustPayToPubKey pk mdv vl ->
        let outs = V.txInfoOutputs scriptContextTxInfo
            hsh dv = V.findDatumHash dv scriptContextTxInfo
            checkOutput (Just dv) TxOut{txOutDatumHash=Just svh} = hsh dv == Just svh
            -- return 'True' by default meaning we fail only when the provided datum is not found
            checkOutput _ _                                      = True
        in
        traceIfFalse "La" -- "MustPayToPubKey"
        $ vl `leq` V.valuePaidTo scriptContextTxInfo pk && any (checkOutput mdv) outs
    MustPayToOtherScript vlh dv vl ->
        let outs = V.txInfoOutputs scriptContextTxInfo
            hsh = V.findDatumHash dv scriptContextTxInfo
            addr = Address.scriptHashAddress vlh
            checkOutput TxOut{txOutAddress, txOutValue, txOutDatumHash=Just svh} =
                   Ada.fromValue txOutValue >= Ada.fromValue vl
                && Ada.fromValue txOutValue <= Ada.fromValue vl + Ledger.minAdaTxOut
                && Value.noAdaValue txOutValue == Value.noAdaValue vl
                && hsh == Just svh
                && txOutAddress == addr
            checkOutput _ = False
        in
        traceIfFalse "Lb" -- "MustPayToOtherScript"
        $ any checkOutput outs
    MustHashDatum dvh dv ->
        traceIfFalse "Lc" -- "MustHashDatum"
        $ V.findDatum dvh scriptContextTxInfo == Just dv
    MustSatisfyAnyOf xs ->
        traceIfFalse "Ld" -- "MustSatisfyAnyOf"
        $ any (checkTxConstraint ctx) xs

{-# INLINABLE checkScriptContext #-}
-- | Does the 'ScriptContext' satisfy the constraints?
checkScriptContext :: forall i o. ToData o => TxConstraints i o -> ScriptContext -> Bool
checkScriptContext TxConstraints{txConstraints, txOwnInputs, txOwnOutputs} ptx =
    traceIfFalse "Ld" -- "checkScriptContext failed"
    $ all (checkTxConstraint ptx) txConstraints
    && all (checkOwnInputConstraint ptx) txOwnInputs
    && all (checkOwnOutputConstraint ptx) txOwnOutputs
