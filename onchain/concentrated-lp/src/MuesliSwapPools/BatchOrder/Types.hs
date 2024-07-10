{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module MuesliSwapPools.BatchOrder.Types
  ( OrderStep (..),
    OrderDatum (..),
    scriptVersion,
    OrderRedeemer (..),
  )
where

import Ledger.Address (Address)
import qualified Plutus.V2.Ledger.Api as V2
import qualified PlutusTx
import PlutusTx.Prelude (Integer, Maybe, BuiltinByteString)
import qualified Prelude as Haskell

data OrderStep
  = Deposit
      { dMinimumLP :: Integer
      }
  | Withdraw
      { wMinimumCoinA :: Integer,
        wMinimumCoinB :: Integer
      }
  | OneSideDeposit
  deriving stock (Haskell.Show)

PlutusTx.makeIsDataIndexed ''OrderStep [('Deposit, 0), ('Withdraw, 1), ('OneSideDeposit, 2)]

data OrderDatum = OrderDatum
  { odSender :: Address,
    odReceiver :: Address,
    odReceiverDatumHash :: Maybe V2.DatumHash,
    odStep :: OrderStep,
    odBatcherFee :: Integer,
    odOutputADA :: Integer,
    odPoolNftTokenName :: V2.TokenName,
    odScriptVersion :: BuiltinByteString
  }
  deriving stock (Haskell.Show)

PlutusTx.makeIsDataIndexed ''OrderDatum [('OrderDatum, 0)]

{-# INLINEABLE scriptVersion #-}
scriptVersion :: BuiltinByteString
scriptVersion = "MuesliSwap_cLP"

data OrderRedeemer = ApplyOrder | CancelOrder

PlutusTx.makeIsDataIndexed
  ''OrderRedeemer
  [ ('ApplyOrder, 0),
    ('CancelOrder, 1)
  ]