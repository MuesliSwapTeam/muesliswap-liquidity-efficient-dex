{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}

module MuesliSwapPools.BatchOrder.OnChain (mkBatchOrderScript) where

import Plutus.Script.Utils.Typed as Scripts
import qualified Plutus.Script.Utils.V2.Scripts as ScriptsV2
import MuesliSwapPools.BatchOrder.Types (OrderDatum(..), OrderRedeemer(..))
import MuesliSwapPools.ConstantProductPool.OnChain (mkPoolScript)
import qualified Plutus.V2.Ledger.Api as V2
import Plutus.V2.Ledger.Contexts (txSignedBy)
import Plutus.V1.Ledger.Address (toValidatorHash)
import PlutusTx
import PlutusTx.Prelude


mkBatchOrderScript :: V2.Script
mkBatchOrderScript = V2.unValidatorScript batcherScript

batcherScript :: V2.Validator
batcherScript = V2.mkValidatorScript $
  $$(PlutusTx.compile [|| wrap ||])
    `PlutusTx.applyCode` PlutusTx.liftCode poolValHash
    where
      wrap = Scripts.mkUntypedValidator . mkBatchOrderValidator

      poolValHash :: V2.ValidatorHash
      !poolValHash = ScriptsV2.validatorHash $ V2.Validator mkPoolScript

{-# INLINEABLE mkBatchOrderValidator #-}
mkBatchOrderValidator :: V2.ValidatorHash -> OrderDatum -> OrderRedeemer -> V2.ScriptContext -> Bool
mkBatchOrderValidator poolScriptHash datum redeemer ctx = case redeemer of
    ApplyOrder -> hasOnePoolInput
    CancelOrder -> validSig
  where
    txInfo :: V2.TxInfo
    txInfo = V2.scriptContextTxInfo ctx

    addrToPkh :: V2.Address -> V2.PubKeyHash
    addrToPkh (V2.Address (V2.PubKeyCredential k) _) = k
    addrToPkh _ = error ()

    validSig :: Bool
    validSig = txSignedBy txInfo $ addrToPkh $ odSender datum

    isPoolAddress :: V2.ValidatorHash -> V2.TxOut -> Bool
    isPoolAddress vh o = case toValidatorHash $ V2.txOutAddress o of
        Just vh' -> vh == vh'
        _ -> False
    
    hasOnePoolInput :: Bool
    hasOnePoolInput = case filter (isPoolAddress poolScriptHash) [V2.txInInfoResolved i | i <- V2.txInfoInputs txInfo] of
        [_] -> True
        _ -> False