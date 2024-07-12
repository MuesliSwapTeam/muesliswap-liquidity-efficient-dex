{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}

module MuesliSwapPools.ConstantProductPool.OnChain (mkPoolScript) where

import Plutus.V2.Ledger.Contexts
  ( 
    ScriptContext (scriptContextTxInfo),
    TxInInfo (txInInfoOutRef, txInInfoResolved),
    TxInfo
      ( txInfoInputs,
        txInfoMint,
        txInfoOutputs,
        txInfoSignatories,
        txInfoValidRange
      ),
    TxOut (..),
  )
import MuesliSwapPools.BatchOrder.Types
  ( OrderDatum (..),
    OrderStep (Deposit, Withdraw),
  )
import MuesliSwapPools.ConstantProductFactory.OnChain
  ( mkBatcherLicenseSymbol,
    mkFactoryCoin,
    mkSwapperLicenseSymbol,
  )
import MuesliSwapPools.ConstantProductLiquidity.OnChain
  ( mkLiquiditySymbol,
  )
import MuesliSwapPools.ConstantProductPool.Types
  ( PoolDatum (..),
    PoolParams (..),
    PoolRedeemer (ApplyPool, DirectSwap)
  )
import MuesliSwapPools.ConstantProductPool.Utils
  ( checkConcentratedSwap,
    calculateDepositAmount,
    calculateWithdrawAmount,
    hasNoInputFromBatcher,
    hasSwapperLicense,
    findOwnInputV2,
    validOutDatum,
    fromJust
  )
import MuesliSwapPools.ConstantProductPoolNFT.OnChain (mkNFTSymbol)
import MuesliSwapPools.ConstantProductPoolNFT.Utils
  ( poolNFTOf,
  )
import MuesliSwapPools.Types.Coin
  ( adaCoin,
    assetClassValue,
    assetClassValueOf,
    tokenNameOf,
  )
import MuesliSwapPools.Utils.OnChainUtils
  ( assertPoolValue,
    bsToInteger,
    mustFindScriptDatum,
    licenseDeadline,
  )
import Plutus.V1.Ledger.Api (POSIXTime (POSIXTime))
import Plutus.V1.Ledger.Address (toPubKeyHash, addressCredential)
import Plutus.V1.Ledger.Interval (after)
import qualified Plutus.V2.Ledger.Api as V2
import Plutus.Script.Utils.Typed as Scripts
import qualified PlutusTx
import PlutusTx.Prelude


mkPoolScript :: V2.Script
mkPoolScript = V2.unValidatorScript poolScript

poolScript :: V2.Validator
poolScript = V2.mkValidatorScript $
  $$(PlutusTx.compile [|| wrap ||])
    `PlutusTx.applyCode` PlutusTx.liftCode poolParams
    where
      wrap = Scripts.mkUntypedValidator . mkPoolValidator
      !poolParams = PoolParams
          { ppNftSymbol = mkNFTSymbol,
            ppLiquiditySymbol = mkLiquiditySymbol,
            ppFactoryCoin = mkFactoryCoin,
            ppBatcherLicenseSymbol = mkBatcherLicenseSymbol,
            ppSwapperLicenseSymbol = mkSwapperLicenseSymbol
          }

{-# INLINEABLE mkPoolValidator #-}
mkPoolValidator :: PoolParams -> PoolDatum -> PoolRedeemer -> ScriptContext -> Bool
mkPoolValidator pp datum redeemer ctx = case redeemer of
  ApplyPool batcherAddress licenseIndex ->
    validateApplyPool pp datum ctx batcherAddress licenseIndex
  DirectSwap licenseIndex ->
    validateDirectSwap pp datum ctx licenseIndex

-- | The 'validateDirectSwap' function validates that ...
--
-- 1.  The pool value must contains necessary token inside
-- 2.  Pool's NFT token must be the same between input and output
-- 3.  Factory NFT token in input have quantity = 1
-- 4.  Factory NFT token in output have quantity = 1
-- 5.  Validate that no asset will be minted in this action
-- 6.  Concentrated constant product formula must be satisfied
-- 7.  Pool datum must remain unchanged
-- 8.  Swapper license token must be present and not be expired
-- 9.  No inputs from batcher contract must be spent in this transaction
validateDirectSwap ::
  -- | Params of PoolValidator
  PoolParams ->
  -- | The Datum specifying which coins are in the pool
  PoolDatum ->
  -- | ScriptContext
  ScriptContext ->
  -- | licenseIndex
  Integer ->
  Bool
validateDirectSwap
  PoolParams
    { ppNftSymbol,
      ppLiquiditySymbol,
      ppSwapperLicenseSymbol,
      ppFactoryCoin
    }
  datum@PoolDatum
    { pdCoinA = coinA,
      pdCoinB = coinB,
      pdTotalLiquidity = totalLiquidityIn,
      pdSwapFee = swapFee,
      pdPriceASqrt = priceASqrt,
      pdPriceBSqrt = priceBSqrt,
      pdPrecompFrac = precompFrac,
      pdApproxSqrtPrec = approxSqrtPrec
    }
  ctx
  licenseIndex =
    let info :: TxInfo
        info = scriptContextTxInfo ctx

        txInputs :: [TxInInfo]
        !txInputs = txInfoInputs info

        txOutputs :: [TxOut]
        !txOutputs = txInfoOutputs info

        ownInput :: TxInInfo
        !ownInput = case findOwnInputV2 ctx of
          Just txInInfo -> txInInfo
          _ -> error ()

        !ownAddress = txOutAddress $ txInInfoResolved ownInput
        !inVal = txOutValue $ txInInfoResolved ownInput
        !nftCoinInInput = poolNFTOf inVal ppNftSymbol

        ownOutput :: TxOut
        !ownOutput = case [ o | o <- txOutputs,
                            txOutAddress o == ownAddress,
                            assetClassValueOf (txOutValue o) nftCoinInInput == 1
                          ] of
          [o] -> o
          _ -> error ()

        !outVal = txOutValue ownOutput

        outputPoolDatum = mustFindScriptDatum @PoolDatum ownOutput info
        validTimeRange = txInfoValidRange info
        validPoolValue = assertPoolValue coinA coinB lpCoin outVal
        nftCoinInOutput = poolNFTOf outVal ppNftSymbol
        factoryCoinIn = assetClassValueOf inVal ppFactoryCoin
        lpCoin = (ppLiquiditySymbol, tokenNameOf nftCoinInInput)
        factoryCoinOut = assetClassValueOf outVal ppFactoryCoin
        amountAIn = assetClassValueOf inVal coinA
        amountBIn = assetClassValueOf inVal coinB
        amountAOut = assetClassValueOf outVal coinA
        amountBOut = assetClassValueOf outVal coinB

        licenseInput :: TxInInfo
        licenseInput = txInputs !! licenseIndex

        isConcentratedSwap = if amountAOut > amountAIn && amountBOut > amountBIn then True else
          checkConcentratedSwap amountAIn amountBIn amountAOut amountBOut swapFee priceASqrt priceBSqrt precompFrac approxSqrtPrec
        
     in validPoolValue -- 1.
          && nftCoinInInput == nftCoinInOutput -- 2.
          && factoryCoinIn == 1 -- 3.
          && factoryCoinOut == 1 -- 4.
          && txInfoMint info == mempty -- 5.
          && isConcentratedSwap -- 6.
          && datum == outputPoolDatum -- 7.
          && after (POSIXTime $ licenseDeadline licenseInput ppSwapperLicenseSymbol) validTimeRange -- 8.
          && hasNoInputFromBatcher txInputs info -- 9.

-- | The 'validateApplyPool' function validates the orders will be correctly applied to the pool
--   by valid batcher
--
-- 1. Validate that the pool value must contains necessary token inside
-- 2. A valid batcher must sign the transaction
-- 3. The license of batcher is not expired
-- 4. Pool's NFT token must be the same between input and output
-- 5. Factory NFT token in input have quantity = 1
-- 6. Factory NFT token in output have quantity = 1
-- 7. The delta liquidity after applying through all orders must be the same
--    with minted amount of LP token
-- 8. Validate that pool value in the output must be the same with the calculated amount
--    after applying through all orders
-- 9. CoinA, CoinB, SwapFee, priceASqrt, priceBSqrt fields of datum remain unchanged
validateApplyPool ::
  -- | Params of PoolValidator
  PoolParams ->
  -- | The Datum specifying which coins are in the pool
  PoolDatum ->
  -- | ScriptContext
  ScriptContext ->
  -- | Address of valid batcher which will submit this batch transaction
  V2.Address ->
  -- | Index of license's UTxO in the transaction inputs
  Integer ->
  Bool
validateApplyPool
  PoolParams
    { ppNftSymbol,
      ppLiquiditySymbol,
      ppBatcherLicenseSymbol,
      ppFactoryCoin
    }
  datum@PoolDatum
    { pdCoinA = coinA,
      pdCoinB = coinB,
      pdTotalLiquidity = totalLiquidityIn
    }
  ctx
  batcherAddress
  licenseIndex =
    let info :: TxInfo
        info = scriptContextTxInfo ctx

        signatories :: [V2.PubKeyHash]
        signatories = txInfoSignatories info

        txInputs :: [TxInInfo]
        !txInputs = txInfoInputs info

        txOutputs :: [TxOut]
        !txOutputs = txInfoOutputs info

        ownInput :: TxInInfo
        !ownInput = case findOwnInputV2 ctx of
          Just txInInfo -> txInInfo
          _ -> error ()

        !ownAddress = txOutAddress $ txInInfoResolved ownInput

        ownOutput :: TxOut
        !ownOutput = case [o | o <- txOutputs, ownAddress == txOutAddress o] of
          [o] -> o
          _ -> error ()

        outputPoolDatum = mustFindScriptDatum @PoolDatum ownOutput info

        !inVal = txOutValue $ txInInfoResolved ownInput
        !outVal = txOutValue ownOutput

        mintValue = txInfoMint info
        validTimeRange = txInfoValidRange info
        validPoolValue = assertPoolValue coinA coinB lpCoin outVal
        nftCoinInInput = poolNFTOf inVal ppNftSymbol
        nftCoinInOutput = poolNFTOf outVal ppNftSymbol
        lpCoin = (ppLiquiditySymbol, tokenNameOf nftCoinInInput)
        totalLiquidityOut = pdTotalLiquidity outputPoolDatum
        factoryCoinIn = assetClassValueOf inVal ppFactoryCoin
        factoryCoinOut = assetClassValueOf outVal ppFactoryCoin
        amountAIn = assetClassValueOf inVal coinA
        amountBIn = assetClassValueOf inVal coinB
        amountAOut = assetClassValueOf outVal coinA
        amountBOut = assetClassValueOf outVal coinB
        totalDeltaLiquidity = totalLiquidityOut - totalLiquidityIn

        applySuccess =
          applyOrders
            userInputs
            userOutputs
            (amountAIn, amountBIn, totalLiquidityIn)

        batcherPubKeyHash = case toPubKeyHash batcherAddress of
          Just pkh -> pkh
          Nothing -> error ()

        licenseInput :: TxInInfo
        licenseInput = txInputs !! licenseIndex

        userInputs :: [TxInInfo]
        userInputs = [i | i <- txInputs, isUserInput i]

        userOutputs :: [TxOut]
        userOutputs = [o | o <- txOutputs, isUserOutput o]

        isUserInput :: TxInInfo -> Bool
        isUserInput txIn =
          txInInfoOutRef txIn /= txInInfoOutRef ownInput
            && txOutAddress (txInInfoResolved txIn) /= batcherAddress

        isUserOutput :: TxOut -> Bool
        isUserOutput txOut =
          let addr = txOutAddress txOut
           in txOutAddress txOut /= txOutAddress ownOutput && addr /= batcherAddress

        applyStep' = \iv od st o -> applyStep iv od st o datum lpCoin

        applyOrders ::
          [TxInInfo] ->
          [TxOut] ->
          (Integer, Integer, Integer) ->
          Bool
        applyOrders [] [] (i1, i2, i3) =
          i1 == amountAOut
            && i2 == amountBOut
            && i3 == totalLiquidityOut
        applyOrders [] _ _ = error ()
        applyOrders _ [] _ = error ()
        applyOrders (i : ins) (o : outs) state =
          let txOut = txInInfoResolved i
              inputValue = txOutValue txOut
              orderDatum = mustFindScriptDatum txOut info
              newState = applyStep' inputValue orderDatum state o
           in applyOrders ins outs newState

        untamperedDatum = datum { pdTotalLiquidity = totalLiquidityOut } == outputPoolDatum

     in validPoolValue -- 1.
          && batcherPubKeyHash `elem` signatories -- 2.
          && after (POSIXTime $ licenseDeadline licenseInput ppBatcherLicenseSymbol) validTimeRange -- 3.
          && nftCoinInInput == nftCoinInOutput -- 4.
          && factoryCoinIn == 1 -- 5.
          && factoryCoinOut == 1 -- 6.
          && assetClassValueOf mintValue lpCoin == totalDeltaLiquidity -- 7.
          && applySuccess -- 8.
          && untamperedDatum -- 9.

-- | The 'applyStep' function validates the order and updates the pool state.
applyStep ::
  -- | Input value
  V2.Value ->
  -- | Order
  OrderDatum ->
  -- | The state of the pool
  (Integer, Integer, Integer) ->
  -- | Transaction output
  TxOut ->
  -- | Information about the pool
  PoolDatum ->
  -- | Liquidity provider coin (LP coin)
  (V2.CurrencySymbol, V2.TokenName) ->
  (Integer, Integer, Integer)
applyStep
  !val
  od@OrderDatum
    { odBatcherFee,
      odOutputADA,
      odStep,
      odPoolNftTokenName
    }
  state
  txo
  pd
  lpCoin =
    if odBatcherFee <= 0 || odOutputADA <= 0 || tokenNameOf lpCoin /= odPoolNftTokenName
      then error ()
    else case odStep of
        Deposit minimumLP ->
          validateDeposit val od state txo pd lpCoin minimumLP
        Withdraw minimumCoinA minimumCoinB ->
          validateWithdraw val od state txo pd lpCoin minimumCoinA minimumCoinB

-- |
--  The 'validateDeposit' function validates that the LP coins are given to the
--  liquidity provider in exchange for their A&B coins.
--  = Logic outline
--    * Calculate the LP coin amount to give the user in exchange for their
--      deposited A&B coin amounts
--        * If the rate deltaA / amountA != deltaB / amountB, account for the
--          change and refund the change balance to the receiver
--        * If there's enough LP coins in the pool to reward the deposit
--           * Validate whether the value of is sent to the correct
--             user (receiver)
--        * If there's NOT enough then throwing error
validateDeposit ::
  -- | The input Value with A&B coins
  V2.Value ->
  -- | The Datum with the sender, receiver and fee information
  OrderDatum ->
  -- | The state of the pool
  (Integer, Integer, Integer) ->
  -- | Transaction output for sending out the LP coins
  TxOut ->
  -- | The Datum specifying which coins are in the pool
  PoolDatum ->
  -- | Liquidity provider coin (LP coin)
  (V2.CurrencySymbol, V2.TokenName) ->
  -- | Desired minimum amount of LP coin to get
  Integer ->
  (Integer, Integer, Integer)
validateDeposit
  val
  OrderDatum
    { odOutputADA,
      odBatcherFee,
      odReceiver,
      odReceiverDatumHash
    }
  ( !amountA,
    !amountB,
    !totalLiquidity
    )
  TxOut
    { txOutValue = txOutVal,
      txOutAddress,
      txOutDatum
    }
  PoolDatum
    { pdCoinA = coinA,
      pdCoinB = coinB,
      pdPriceASqrt = priceASqrt,
      pdPriceBSqrt = priceBSqrt,
      pdPrecompFrac = precompFrac,
      pdApproxSqrtPrec = approxSqrtPrec
    }
  lpCoin
  minimumLP =
    let coinAAmount = assetClassValueOf val coinA
        coinBAmount = assetClassValueOf val coinB
        !deltaA =
          if coinA == adaCoin
            then coinAAmount - (odBatcherFee + odOutputADA)
            else coinAAmount
        !deltaB =
          if coinB == adaCoin
            then coinBAmount - (odBatcherFee + odOutputADA)
            else coinBAmount
        -- If the rate deltaA / amountA != deltaB / amountB (this case happens
        -- when having swapping actions before this depositing action because of
        -- this rate is changed), we should recalculate the real deltaA & deltaB
        -- amount and refund the change balance to the receiver (the deltaA' &
        -- deltaB')
        (!deltaA', !deltaB', !deltaLiquidity) =
          case calculateDepositAmount deltaA deltaB amountA amountB totalLiquidity priceASqrt priceBSqrt precompFrac approxSqrtPrec of
            Just da -> da
            _ -> error ()
        lpAmountOut = assetClassValueOf txOutVal lpCoin
        adaAmountOut = assetClassValueOf txOutVal adaCoin
        coinAAmountOut = assetClassValueOf txOutVal coinA
        coinBAmountOut = assetClassValueOf txOutVal coinB
        validAmount
          | deltaB > deltaB' =
            if coinB == adaCoin
              then (adaAmountOut == deltaB - deltaB' + odOutputADA) && (lpAmountOut == deltaLiquidity)
              else
                (adaAmountOut == odOutputADA)
                  && (lpAmountOut == deltaLiquidity)
                  && (deltaB - deltaB' == coinBAmountOut)
          | deltaA > deltaA' =
            if coinA == adaCoin
              then (adaAmountOut == deltaA - deltaA' + odOutputADA) && (lpAmountOut == deltaLiquidity)
              else
                (adaAmountOut == odOutputADA)
                  && (lpAmountOut == deltaLiquidity)
                  && (deltaA - deltaA' == coinAAmountOut)
          | otherwise = (lpAmountOut == deltaLiquidity) && (adaAmountOut == odOutputADA)
        validReceiver = (txOutAddress == odReceiver) && validOutDatum txOutDatum odReceiverDatumHash
        newAmountA = amountA + deltaA'
        newAmountB = amountB + deltaB'
     in if (deltaLiquidity >= minimumLP) && validAmount && validReceiver
          then
            ( newAmountA,
              newAmountB,
              totalLiquidity + deltaLiquidity
            )
          else error ()

-- |
--  The 'validateWithdraw' function validates that A&B coins are given back to
--  the liquidity provider in exchange for their LP coins.
--  = Logic outline
--    * Calculate the A&B coin amounts to withdraw based on the given LP amount
--        * If there's enough A&B coins in the pool to withdraw
--            * Validate whether the correct amounts return to the correct user (receiver)
--        * If there's NOT enough then throwing error
validateWithdraw ::
  -- | The input Value with LP coins
  V2.Value ->
  -- | The Datum with the sender and receiver information
  OrderDatum ->
  -- | The state of the pool
  (Integer, Integer, Integer) ->
  -- | Transaction output for sending out the withdrawn coins
  TxOut ->
  -- | The Datum specifying which coins are in the pool
  PoolDatum ->
  -- | Liquidity provider coin (LP coin)
  (V2.CurrencySymbol, V2.TokenName) ->
  -- | Minimum amount of coin A to withdraw
  Integer ->
  -- | Minimum amount of coin B to withdraw
  Integer ->
  (Integer, Integer, Integer)
validateWithdraw
  val
  OrderDatum
    { odOutputADA,
      odReceiver,
      odReceiverDatumHash
    }
  ( !amountA,
    !amountB,
    !totalLiquidity
  )
  TxOut
    { txOutValue = txOutVal,
      txOutAddress,
      txOutDatum
    }
  PoolDatum
    { pdCoinA = coinA,
      pdCoinB = coinB,
      pdPriceASqrt = priceASqrt,
      pdPriceBSqrt = priceBSqrt,
      pdPrecompFrac = precompFrac,
      pdApproxSqrtPrec = approxSqrtPrec
    }
  lpCoin
  minimumCoinA
  minimumCoinB =
    let !deltaLiquidity = assetClassValueOf val lpCoin
        (!deltaA, !deltaB) = fromJust $ calculateWithdrawAmount amountA amountB deltaLiquidity totalLiquidity priceASqrt priceBSqrt precompFrac approxSqrtPrec
        coinAAmountOut = assetClassValueOf txOutVal coinA
        coinBAmountOut = assetClassValueOf txOutVal coinB
        adaAmountOut = assetClassValueOf txOutVal adaCoin
        validAmount
          | coinA == adaCoin =
            coinAAmountOut == deltaA + odOutputADA
              && coinBAmountOut == deltaB
          | coinB == adaCoin =
            coinBAmountOut == deltaB + odOutputADA
              && coinAAmountOut == deltaA
          | otherwise =
            coinAAmountOut == deltaA
              && coinBAmountOut == deltaB
              && adaAmountOut == odOutputADA
        validReceiver = txOutAddress == odReceiver && validOutDatum txOutDatum odReceiverDatumHash
        newAmountA = amountA - deltaA
        newAmountB = amountB - deltaB
        poolHasSufficientABCoins = deltaA >= minimumCoinA && deltaB >= minimumCoinB
     in if poolHasSufficientABCoins && validAmount && validReceiver
          then
            ( newAmountA,
              newAmountB,
              totalLiquidity - deltaLiquidity
            )
          else error ()