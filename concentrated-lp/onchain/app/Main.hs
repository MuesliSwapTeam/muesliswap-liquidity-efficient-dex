module Main where

import Cardano.Api
import Cardano.Api.Shelley (PlutusScript (..))
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as BSS
import MuesliSwapPools.BatchOrder.OnChain (mkBatchOrderScript)
import MuesliSwapPools.ConstantProductFactory.OnChain (mkFactoryScript)
import MuesliSwapPools.ConstantProductLiquidity.OnChain (mkLiquidityScript)
import MuesliSwapPools.ConstantProductPool.OnChain (mkPoolScript)
import MuesliSwapPools.ConstantProductPoolNFT.OnChain (mkNFTScript, mkNFTSymbol)


main :: IO ()
main = do
  writePlutusScript' "plutus/nft_minting_policy.plutus" mkNFTScript
  writePlutusScript' "plutus/lp_minting_policy.plutus" mkLiquidityScript
  writePlutusScript' "plutus/factory_minting_policy.plutus" mkFactoryScript
  writePlutusScript' "plutus/pool_script.plutus" mkPoolScript
  writePlutusScript' "plutus/batch_order_script.plutus" mkBatchOrderScript

writePlutusScript' filename scrpt =
  do
    let scriptSBS = BSS.toShort . BSL.toStrict . serialise $ scrpt
    let scriptSerial = PlutusScriptSerialised scriptSBS :: PlutusScript PlutusScriptV2
    result <- writeFileTextEnvelope filename Nothing scriptSerial
    case result of
      Left err -> print $ displayError err
      Right () -> return ()