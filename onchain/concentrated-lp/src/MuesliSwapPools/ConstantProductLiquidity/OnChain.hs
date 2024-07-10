{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}

module MuesliSwapPools.ConstantProductLiquidity.OnChain
  ( mkLiquidityScript,
    mkLiquiditySymbol,
    mkLiquidityPolicy,
  )
where

import MuesliSwapPools.ConstantProductPoolNFT.OnChain
import Plutus.V2.Ledger.Contexts (ownCurrencySymbol)
import MuesliSwapPools.Utils.OnChainUtils (hasOutDatum)
import qualified Plutus.V2.Ledger.Api as V2
import qualified Plutus.Script.Utils.V2.Scripts as ScriptsV2
import qualified PlutusTx
import qualified PlutusTx.AssocMap as Map
import PlutusTx.Prelude


{-# INLINEABLE mkLiquidityPolicy #-}
mkLiquidityPolicy :: V2.MintingPolicy
mkLiquidityPolicy =
  V2.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| mkLiquidityValidator ||])
      `PlutusTx.applyCode` PlutusTx.liftCode mkNFTSymbol

{-# INLINEABLE mkLiquidityScript #-}
mkLiquidityScript :: V2.Script
mkLiquidityScript = V2.unMintingPolicyScript mkLiquidityPolicy

{-# INLINEABLE mkLiquiditySymbol #-}
mkLiquiditySymbol :: V2.CurrencySymbol
mkLiquiditySymbol = ScriptsV2.scriptCurrencySymbol mkLiquidityPolicy

-- | The 'mkLiquidityValidator' function validates the LP token is minted correctly
--
-- 1.   Validate that LP TokenName == NFT TokenName
{-# INLINEABLE mkLiquidityValidator #-}
mkLiquidityValidator :: V2.CurrencySymbol -> BuiltinData -> BuiltinData -> ()
mkLiquidityValidator nftSymbol _ rawContext =
  let context = PlutusTx.unsafeFromBuiltinData rawContext
      info = V2.scriptContextTxInfo context
      ownSymbol = ownCurrencySymbol context

      txOutputs :: [V2.TxOut]
      !txOutputs = V2.txInfoOutputs info

      mintValue :: V2.Value
      !mintValue = V2.txInfoMint info

      nftTokenName :: V2.TokenName
      nftTokenName = case [fromJust lu | lu <- lookups, isJust lu] of
        [i] -> case [m | m@(_, am) <- Map.toList i, am == 1] of
          [(tn, _)] -> tn
          _ -> error ()
        _ -> error ()
        where
          outputsWithDatum = [o | o <- txOutputs, hasOutDatum o]
          lookups = [Map.lookup nftSymbol (V2.getValue $ V2.txOutValue o) | o <- outputsWithDatum]

          fromJust (Just x) = x
          fromJust Nothing  = error ()

      lpTokenName :: V2.TokenName
      lpTokenName = case Map.lookup ownSymbol (V2.getValue mintValue) of
        Just i -> case Map.toList i of
          [(tn, _)] -> tn
          _ -> error ()
        _ -> error ()
   in check $ nftTokenName == lpTokenName -- 1.