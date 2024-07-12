{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}

module MuesliSwapPools.ConstantProductPool.Utils
  ( calculateInitialLiquidity,
    minimumLiquidity,
    calculateDepositAmount,
    hasNoInputFromBatcher,
    hasSwapperLicense,
    findOwnInputV2,
    validOutDatum,
    calculateWithdrawAmount,
    checkConcentratedSwap,
    fromJust
  )
where

import qualified Plutus.V2.Ledger.Api as V2
import Plutus.V2.Ledger.Contexts (TxInfo, TxInInfo, txOutDatum, txInInfoResolved)
import MuesliSwapPools.BatchOrder.Types
  ( OrderDatum,
    odScriptVersion,
    scriptVersion,
  )
import qualified PlutusTx
import PlutusTx.Prelude
import qualified PlutusTx.AssocMap as Map
import PlutusTx.Ratio (properFraction, numerator, denominator, ratio, recip)


{-# INLINEABLE minimumLiquidity #-}
minimumLiquidity :: Integer
minimumLiquidity = 1000

{-# INLINEABLE calSqrt #-}
calSqrt :: Integer -> Integer
calSqrt x
  | x < 0 = error ()
  | x == 0 = 0
  | x == 1 = 1
  | x == 2 = 1
  | otherwise = go x (x `divide` 2 + 1)
  where
    go :: Integer -> Integer -> Integer
    go i1 i2 =
      if i2 < i1
        then go i2 ((x `divide` i2 + i2) `divide` 2)
        else i1

{-# INLINEABLE fromJust #-}
fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing = error ()

{-# INLINEABLE calPow #-}
calPow :: Integer -> Integer -> Integer
calPow a b
  | b <= 0 = 1
  | otherwise = a * calPow a (b - 1)

{-# INLINEABLE calApproxSqrt #-}
calApproxSqrt :: Rational -> Integer -> Rational
calApproxSqrt x prec = fromJust $ (calSqrt ((numerator x) * f)) `ratio` (calSqrt ((denominator x) * f))
  where
    f = calPow 10 $ max 0 $ prec - min (numLen $ numerator x) (numLen $ denominator x)
    numLen :: Integer -> Integer
    numLen i
      | i == 0 = 0
      | otherwise = 1 + (numLen $ i `divide` 10)

{-# INLINEABLE calFloor #-}
calFloor :: Rational -> Integer
calFloor x = if x >= (fromInteger 0) then fst (properFraction x) else error ()

{-# INLINEABLE calCeil #-}
calCeil :: Rational -> Integer
calCeil x = if x >= (fromInteger 0) then
  (if s > (fromInteger 0) then n + 1 else n)
  else error ()
  where (n, s) = properFraction x

{-# INLINEABLE calculateInitialLiquidity #-}
calculateInitialLiquidity :: Integer -> Integer -> Rational -> Rational -> Rational -> Integer -> Integer
calculateInitialLiquidity amountA amountB pASqrt pBSqrt precompFrac prec =
  let
    amA = fromInteger amountA
    amB = fromInteger amountB
    providedL = calculateLiquidity amA amB pASqrt pBSqrt precompFrac prec
  in if precompFrac == (fromInteger 1) - (pASqrt * (recip pBSqrt))
    && (fromInteger 0) < pASqrt
    && pASqrt < pBSqrt then
      calFloor providedL
    else
      error ()

{-# INLINEABLE calculateDepositAmount #-}
calculateDepositAmount :: Integer -> Integer -> Integer -> Integer -> Integer -> Rational -> Rational -> Rational -> Integer -> Maybe (Integer, Integer, Integer)
calculateDepositAmount deltaA deltaB amountA amountB lpBefore pASqrt pBSqrt precompFrac prec =
  let
    zero = fromInteger 0
    amA = fromInteger amountA
    amB = fromInteger amountB
    dA = fromInteger deltaA
    dB = fromInteger deltaB
    curL = calculateLiquidity amA amB pASqrt pBSqrt precompFrac prec
    pSqrt = calculatePriceSqrt amB curL pASqrt

    dLiqA = dA * pSqrt * pBSqrt * (recip (pBSqrt - pSqrt))
    dLiqB = dB * (recip (pSqrt - pASqrt))
    (dA', dB', dLiq)
      | dLiqA < dLiqB = (dA, dLiqA * (pSqrt - pASqrt), dLiqA)
      | dLiqA > dLiqB = (dLiqB * (recip pSqrt - recip pBSqrt), dB, dLiqB)
      | otherwise = (dA, dB, dLiqA)

    dLp = if dLiq >= zero then
      (lpBefore * (numerator dLiq) * (denominator curL)) `divide` ((denominator dLiq) * (numerator curL))
      else error ()

  in if dA' >= zero && dB' >= zero
    then Just (calCeil dA', calCeil dB', dLp)
  else Nothing

{-# INLINEABLE calculateWithdrawAmount #-}
calculateWithdrawAmount :: Integer -> Integer -> Integer -> Integer -> Rational -> Rational -> Rational -> Integer -> Maybe (Integer, Integer)
calculateWithdrawAmount amountA amountB deltaLp lpBefore pASqrt pBSqrt precompFrac prec =
  let
    zero = fromInteger 0
    amA = fromInteger amountA
    amB = fromInteger amountB
    curL = calculateLiquidity amA amB pASqrt pBSqrt precompFrac prec
    pSqrt = calculatePriceSqrt amB curL pASqrt

    dLiq = fromJust $ ((numerator curL) * deltaLp) `ratio` (lpBefore * (denominator curL))
    dA = dLiq * (recip pSqrt - recip pBSqrt)
    dB = dLiq * (pSqrt - pASqrt)

  in if dA >= zero && dB >= zero
     then Just (calFloor dA, calFloor dB)
     else Nothing

{-# INLINEABLE calculatePriceSqrt #-}
calculatePriceSqrt :: Rational -> Rational -> Rational -> Rational
calculatePriceSqrt amountB curL pASqrt = amountB * (recip curL) + pASqrt

{-# INLINEABLE calculateLiquidity #-}
calculateLiquidity :: Rational -> Rational -> Rational -> Rational -> Rational -> Integer -> Rational
calculateLiquidity amountA amountB pASqrt pBSqrt precompFrac prec =
  let
    b = amountB * (recip pBSqrt) + amountA * pASqrt
  in (b + (calApproxSqrt (b * b + (fromInteger 4) * amountA * amountB * precompFrac) prec)) * recip ((fromInteger 2) * precompFrac)

{-# INLINEABLE checkConcentratedSwap #-}
checkConcentratedSwap :: Integer -> Integer -> Integer -> Integer -> Integer -> Rational -> Rational -> Rational -> Integer -> Bool
checkConcentratedSwap oldA oldB newA newB fee pASqrt pBSqrt precompFrac prec =
  let
    oA = fromInteger oldA
    oB = fromInteger oldB
    nA = fromInteger newA
    nB = fromInteger newB
    c = fromJust $ fee `ratio` 10000
    inA = fromInteger $ max 0 (newA - oldA)
    inB = fromInteger $ max 0 (newB - oldB)

    beforeL = calculateLiquidity oA oB pASqrt pBSqrt precompFrac prec
    intL = calculateLiquidity (nA - c * inA) (nB - c * inB) pASqrt pBSqrt precompFrac prec
  in intL >= beforeL

{-# INLINEABLE hasNoInputFromBatcher #-}
hasNoInputFromBatcher :: [TxInInfo] -> TxInfo -> Bool
hasNoInputFromBatcher txIns txInfo =
  let isBatcherInput txIn = case txOutDatum $ txInInfoResolved txIn of
        V2.OutputDatum (V2.Datum d) -> case PlutusTx.fromBuiltinData d of
          Just b -> odScriptVersion b == scriptVersion
          _ -> False
        _ -> False
   in case [i | i <- txIns, isBatcherInput i] of
        [] -> True
        _ -> False

{-# INLINEABLE hasSwapperLicense #-}
hasSwapperLicense :: V2.Value -> V2.CurrencySymbol -> Bool
hasSwapperLicense (V2.Value v) licenseSymbol = case Map.lookup licenseSymbol v of
  Nothing -> False
  Just _ -> True

findOwnInputV2 :: V2.ScriptContext -> Maybe V2.TxInInfo
findOwnInputV2 V2.ScriptContext{V2.scriptContextTxInfo=V2.TxInfo{V2.txInfoInputs},
             V2.scriptContextPurpose=V2.Spending txOutRef} = go txInfoInputs
    where
        go [] = Nothing
        go (i@V2.TxInInfo{V2.txInInfoOutRef} : rest) = if txInInfoOutRef == txOutRef
                                                 then Just i
                                                 else go rest
findOwnInputV2 _ = Nothing

{-# INLINEABLE validOutDatum #-}
validOutDatum :: V2.OutputDatum -> Maybe V2.DatumHash -> Bool
validOutDatum _ Nothing = True
validOutDatum (V2.OutputDatumHash dh) (Just dh') = dh == dh'
validOutDatum _ _ = False