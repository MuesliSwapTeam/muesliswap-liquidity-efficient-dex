# Process requests to deposit or withdraw tokens

import math
import random
import argparse

from src.batch_lib import (
    generate_argparser,
    ObserveOrderData,
    schedule_observer,
)
from ..util import *
from .. import lp_lib

_LOGGER = logging.getLogger(__name__)

BATCHER_SKEY = KEYS_DIR.joinpath("payment_cpool.skey")


REDEEMER = Bech32Addr(
    "addr1q9dzylk2cdp73k83zemqj4cccxynyc9jvka9dyr7xratyzs0wjj5rgd9ns668x8qjp0a8fuz9j5v6cjadl9vc70hh36s4yr4hw"
)


def process_batches(pool, orders, max_batch=1):
    redeem: Dict[
        Tuple[ShelleyAddress, lp_lib.BatchingOrder], Dict[Token, int]
    ] = defaultdict(lambda: defaultdict(int))
    mint_lp = 0
    batched = []
    for o in orders:
        try:
            if len(batched) >= max_batch:
                break
            if o.sender in redeem or o.receiver in redeem:
                continue
            # try to find orders that can be satisfied by this pool
            if isinstance(o, lp_lib.Deposit):
                # make sure that this pool matches
                dep_tokenA, dep_tokenB = (
                    (o.tokenA, o.tokenB)
                    if o.tokenA.token == pool.tokenA.token
                    and o.tokenB.token == pool.tokenB.token
                    else (o.tokenB, o.tokenA)
                )
                if not (
                    pool.tokenA.token == dep_tokenA.token
                    and pool.tokenB.token == dep_tokenB.token
                ):
                    continue
                if (
                    o.utxo.owner
                    in [lp_lib.BATCHING_CONTRACT_PV2, lp_lib.BATCHING_CONTRACT_CLP]
                    and pool.lp_asset.token.name != o.pool_id.name
                ):
                    continue
                if o.minimum_lp_coins < 0:
                    continue
                # compute resulting LP tokens
                lq_befor = pool.lp_asset.amount
                dep_tokenAp, dep_tokenBp, lq_diff = lp_lib.calculate_deposit_amount_concentrated(
                    dep_tokenA,
                    dep_tokenB,
                    pool.tokenA,
                    pool.tokenB,
                    lq_befor,
                    pool.priceA_sqrt,
                    pool.priceB_sqrt,
                    pool.precomp_frac,
                    pool.approx_sqrt_prec,
                )
                new_tokenA_amount = pool.tokenA.amount + dep_tokenAp.amount
                new_tokenB_amount = pool.tokenB.amount + dep_tokenBp.amount
                if lq_diff < o.minimum_lp_coins:
                    continue
                if pool.profit_sharing_dest is None:
                    profit_sharing_lq = 0
                    new_root_k_last = pool.root_k_last
                else:
                    profit_sharing_lq = lp_lib.calculate_profit_sharing(
                        pool.root_k_last,
                        pool.tokenA,
                        pool.tokenB,
                        pool.lp_asset.amount,
                        pool.profit_sharing_fee,
                    )
                    new_root_k_last = math.sqrt(new_tokenA_amount * new_tokenB_amount)
                # otherwise store how much of each token both the pool and the order placer will redeem
                # the batcher fee will be returned as part of the change is ignored here
                # the deposit redeemer retreives lp tokens
                redeem[(o.receiver, o)][LOVELACE] += o.deposit
                redeem[(o.receiver, o)][pool.lp_asset.token] += lq_diff
                # and the change in token A&B
                if dep_tokenAp != dep_tokenA:
                    redeem[(o.receiver, o)][dep_tokenAp.token] += (
                        dep_tokenA.amount - dep_tokenAp.amount
                    )
                if dep_tokenBp != dep_tokenB:
                    redeem[(o.receiver, o)][dep_tokenBp.token] += (
                        dep_tokenB.amount - dep_tokenBp.amount
                    )
                # the new pool has an updated amount of token A&B and lq asset
                mint_lp += lq_diff + profit_sharing_lq
                pool_args = {
                    "tokenA": Asset(new_tokenA_amount, pool.tokenA.token),
                    "tokenB": Asset(new_tokenB_amount, pool.tokenB.token),
                    "lp_asset": Asset(
                        pool.lp_asset.amount + lq_diff + profit_sharing_lq,
                        pool.lp_asset.token,
                    ),
                    "utxo": pool.utxo,
                    "provider": pool.provider,
                    "pool_id": pool.pool_id,
                    "timestamp": pool.timestamp,
                    "fee": pool.fee,
                    "profit_sharing_fee": pool.profit_sharing_fee,
                    "batcher_fee": pool.batcher_fee,
                    "deposit": pool.deposit,
                    "deposit_fee": pool.deposit_fee,
                    "profit_sharing_dest": pool.profit_sharing_dest,
                    "profit_sharing_amt": Asset(
                        pool.profit_sharing_amt.amount + profit_sharing_lq,
                        pool.profit_sharing_amt.token,
                    )
                    if pool.profit_sharing_amt is not None
                    else None,
                    "root_k_last": new_root_k_last,
                }
                pool_args["priceA_sqrt"] = pool.priceA_sqrt
                pool_args["priceB_sqrt"] = pool.priceB_sqrt
                pool_args["precomp_frac"] = pool.precomp_frac
                pool_args["approx_sqrt_prec"] = pool.approx_sqrt_prec
                pool = lp_lib.ConcentratedPool(**pool_args)

            if isinstance(o, lp_lib.Withdraw):
                # make sure that this pool matches
                if pool.lp_asset.token != o.lp_coins.token:
                    continue
                if o.min_tokenA.amount < 0 or o.min_tokenB.amount < 0:
                    # things break regarding the datum hash
                    continue
                wit_tokenA, wit_tokenB = (
                    (o.min_tokenA, o.min_tokenB)
                    if o.min_tokenA.token == pool.tokenA.token
                    and o.min_tokenB.token == pool.tokenB.token
                    else (o.min_tokenB, o.min_tokenA)
                )
                if not (
                    pool.tokenA.token == wit_tokenA.token
                    and pool.tokenB.token == wit_tokenB.token
                ):
                    continue
                # check how many tokens of each kind the pool can provide
                delta_lq = o.lp_coins.amount
                (
                    delta_A_asset,
                    delta_B_asset,
                ) = lp_lib.calculate_withdraw_amount_concentrated(
                    pool.tokenA,
                    pool.tokenB,
                    delta_lq,
                    pool.lp_asset.amount,
                    pool.priceA_sqrt,
                    pool.priceB_sqrt,
                    pool.precomp_frac,
                    pool.approx_sqrt_prec,
                )
                delta_A, delta_B = delta_A_asset.amount, delta_B_asset.amount
                new_poolA = pool.tokenA.amount - delta_A
                new_poolB = pool.tokenB.amount - delta_B
                assert new_poolA > 0 and new_poolB > 0
                if delta_A < wit_tokenA.amount or delta_B < wit_tokenB.amount:
                    continue
                # make sure that at least 2 ADA remain in the pool after withdrawing!
                if pool.tokenA.token == LOVELACE and new_poolA < 2000000:
                    continue
                if pool.tokenB.token == LOVELACE and new_poolB < 2000000:
                    continue
                # from this point on we assume that this withdraw will go through
                if pool.profit_sharing_dest is None:
                    profit_sharing_lq = 0
                    new_root_k_last = pool.root_k_last
                else:
                    profit_sharing_lq = lp_lib.calculate_profit_sharing(
                        pool.root_k_last,
                        pool.tokenA,
                        pool.tokenB,
                        pool.lp_asset.amount,
                        pool.profit_sharing_fee,
                    )
                    new_root_k_last = math.sqrt(new_poolA * new_poolB)
                # compute how much the withdrawer redeems and the new pool state
                redeem[(o.receiver, o)][wit_tokenA.token] += delta_A
                redeem[(o.receiver, o)][wit_tokenB.token] += delta_B
                redeem[(o.receiver, o)][LOVELACE] += o.deposit
                mint_lp -= o.lp_coins.amount
                # the new pool has an updated amount of token A&B and lq asset
                pool_args = {
                    "tokenA": Asset(new_poolA, pool.tokenA.token),
                    "tokenB": Asset(new_poolB, pool.tokenB.token),
                    "lp_asset": Asset(
                        pool.lp_asset.amount - o.lp_coins.amount, pool.lp_asset.token
                    ),
                    "provider": pool.provider,
                    "utxo": pool.utxo,
                    "pool_id": pool.pool_id,
                    "timestamp": pool.timestamp,
                    "fee": pool.fee,
                    "profit_sharing_fee": pool.profit_sharing_fee,
                    "batcher_fee": pool.batcher_fee,
                    "deposit": pool.deposit,
                    "deposit_fee": pool.deposit_fee,
                    "profit_sharing_dest": pool.profit_sharing_dest,
                    "root_k_last": new_root_k_last,
                    "profit_sharing_amt": Asset(
                        pool.profit_sharing_amt.amount + profit_sharing_lq,
                        pool.profit_sharing_amt.token,
                    )
                    if pool.profit_sharing_amt is not None
                    else None,
                }
                pool_args["priceA_sqrt"] = pool.priceA_sqrt
                pool_args["priceB_sqrt"] = pool.priceB_sqrt
                pool_args["precomp_frac"] = pool.precomp_frac
                pool_args["approx_sqrt_prec"] = pool.approx_sqrt_prec
                pool = lp_lib.ConcentratedPool(**pool_args)

            batched.append(o)
        except AssertionError:
            continue
    return batched, redeem, pool, mint_lp


class BatchingObserver(ObserveOrderData):
    def __init__(
        self,
        cli_args: argparse.Namespace,
    ):
        super(BatchingObserver, self).__init__(
            cli_args,
            [lp_lib.BATCHING_LICENSE_NFT_POLICYID_CLP],
            REDEEMER,
        )

    @property
    def batching_licenses(self):
        return self.licenses[lp_lib.BATCHING_LICENSE_NFT_POLICYID_CLP]

    def trigger_batcher(self):
        all_pools = self.pools
        all_pools = (
            [],
            [],
            [p for p in all_pools if p.utxo.owner in lp_lib.POOL_CONTRACT_CLP],
        )
        all_orders = [
            sorted(x, key=lambda o: (o.utxo.tx_hash, o.utxo.index))
            for x in lp_lib.fetch_batching_orders(all_pools)
        ]
        collaterals = self.collaterals
        batching_licenses = self.batching_licenses
        self.refetch = True
        # walk through all orders and try to process them
        for pools, orders in zip(all_pools, all_orders):
            for pool in pools:
                max_batched = 1
                if len(collaterals) == 0 or len(batching_licenses) == 0:
                    _LOGGER.error("No collaterals or licenses anymore at address!")
                    break
                try:
                    batched, redeem, new_pool, mint_lp = process_batches(
                        pool, orders, max_batched
                    )
                    if len(batched):
                        _LOGGER.info("Found batch, submitting")
                        cs = [collaterals[0]] * (
                            len(batched) + 2
                        )  # random.sample(collaterals, len(batched) + 2)
                        l = random.choice(batching_licenses)
                        lp_lib.build_batch_tx(
                            pool,
                            new_pool,
                            batched,
                            redeem,
                            mint_lp,
                            self.wallet_address,
                            self.wallet_skeyfile,
                            cs,
                            l,
                        )
                        for c in set(cs):
                            collaterals.remove(c)
                        batching_licenses.remove(l)
                        for b in set(batched):
                            orders.remove(b)
                except Exception as e:
                    _LOGGER.error(
                        f"Unexpected error processing pool {pool}", exc_info=e
                    )


if __name__ == "__main__":
    a = generate_argparser()
    args = a.parse_args()

    event = BatchingObserver(args)
    observer = schedule_observer(event)
