from __future__ import annotations

from collections import defaultdict
import random
from cardano_clusterlib import clusterlib

from typing import Optional

from math import floor
import argparse
import logging


import src.util
from src.util import NETWORK
from cardano_python_utils.classes import (
    Bech32Addr,
    Token,
    LOVELACE,
    Asset,
    ShelleyAddress,
)
from cardano_python_utils.pools.muesliv2_lib import (
    ORDERBOOK_CONTRACT_V2,
    ORDERBOOK_CONTRACT_V3,
    ORDERBOOK_CONTRACT_V4,
)
from src import mm_lib, swap_lib
from src.batch_lib import (
    ObserveOrderData,
    generate_argparser,
    schedule_observer,
)
from src.mm_lib import Order
from secret import MAINNET
from .. import lp_lib
from ..order_lib import collect_pairs, remap
from fractions import Fraction

_LOGGER = logging.getLogger(__name__)

WALLET_ADDRESS = (
    Bech32Addr(
        "addr1q9yrhxezs9jpv9huxd6sk27pecrak8863gqfkan7m24678dcvtjm8enawhyjjkcf6eves2cwz4c8y9tvhjuzpvmu4rwssukzmr"
    )
    if MAINNET
    else "addr_test1vpyrhxezs9jpv9huxd6sk27pecrak8863gqfkan7m24678gn68sz7"
)
WALLET_SKEYFILE = src.util.KEYS_DIR.joinpath("payment_purr.skey")
REDEEMER = Bech32Addr(
    "addr1qxl8rt6xpzy44pz59kq769k0us5gy53fzl7f2gpj5cee2pu9kxyn2mxfgcycd7tqr6uu7yeyzss7g8d83tkllg4pyacqgk38pk"
)

# this should be ~ once matcher fee from the FE
# but optimally also pay for the contract costs
MIN_MATCHING_GAIN_PER_PARTICIPANT = 460000
MIN_MATCHING_GAIN_INITIAL = 2 * MIN_MATCHING_GAIN_PER_PARTICIPANT
MIN_SWAP_SIZE = 1500000

MAX_GAIN = 1_000_000


def swap(buy, sell, wallet_address, pool, max_matched):
    # sort by closer to pool price
    placed = [o for o in buy + sell if o.sell.amount != 0 and o.buy.amount != 0]
    placed = sorted(
        placed,
        key=lambda x: abs(
            (
                Fraction(x.buy.amount, x.sell.amount)
                if pool.tokenA.token == x.buy.token
                else Fraction(x.sell.amount, x.buy.amount)
            )
            - Fraction(pool.virt_tokenA.amount, pool.virt_tokenB.amount)
        ),
        reverse=True,
    )

    redeem = defaultdict(lambda: defaultdict(int))
    # remaining order
    new_order = None
    # list of matched orders (txs)
    # with annotation of the ratio of matching
    matched = defaultdict(Fraction)

    # we can only do one partial match per tx
    while len(placed) > 0 and len(matched) < max_matched and new_order == None:
        # _LOGGER.debug(f"Popping first two orders for a match")
        # take first order
        f = placed.pop()
        try:
            # try both directions in case one fails
            if f.sell.token == LOVELACE:
                pool_in, new_pool_real_state = pool.pool_in(f.buy)
                mm_redeem = Asset(f.sell.amount - pool_in.amount, f.sell.token)
            # if f.buy.token == LOVELACE or True:
            else:
                # if other tokens are traded we earn at least the fee
                pool_out, new_pool_real_state = pool.pool_out(f.sell)
                mm_redeem = Asset(pool_out.amount - f.buy.amount, f.buy.token)
            # sanity check
        except Exception as e:
            _LOGGER.debug(f"Error when computing pool state {e}")
            continue

        if (
            mm_redeem.amount >= 0
            and all(s >= 0 for s in new_pool_real_state)
            and (
                new_pool_real_state[0] >= 2000000
                if pool.tokenA.token == LOVELACE
                else True
            )
            and (
                new_pool_real_state[1] >= 2000000
                if pool.tokenB.token == LOVELACE
                else True
            )
        ):
            # establish match

            if mm_redeem.token == LOVELACE:
                mm_return = mm_redeem.amount - min(MAX_GAIN, mm_redeem.amount)
                mm_redeem = Asset(mm_redeem.amount - mm_return, mm_redeem.token)
                if mm_return > 0:
                    redeem[(f.creator, f)][LOVELACE] += mm_return

            redeem[(wallet_address, None)][mm_redeem.token] += mm_redeem.amount
            r = Fraction(1)
            matched[f] = r
            redeem[(f.creator, f)][f.buy.token] += f.buy.amount * r
            for a_token, a_amount in f.attached.items():
                redeem[(f.creator, f)][a_token] += a_amount
            paid_fee = floor(r * f.fee)
            if f.buy.token != LOVELACE and f.sell.token != LOVELACE:
                redeem[(f.creator, f)][LOVELACE] -= paid_fee
            placed = [o for o in placed if o.creator != f.creator]
            # and calculate new pool
            tokenA_after, tokenB_after = new_pool_real_state
            pool = pool.copy_with_state(
                Asset(tokenA_after, pool.tokenA.token),
                Asset(tokenB_after, pool.tokenB.token),
            )

    return redeem, matched, new_order, pool


class SwapBatchObserver(ObserveOrderData):
    def __init__(self, cli_args, exclude):
        super(SwapBatchObserver, self).__init__(
            cli_args,
            [mm_lib.LICENSE_NFT_POLICYID, swap_lib.SWAPPING_LICENSE_NFT_POLICYID_CLP],
            REDEEMER,
        )
        self.exclude = exclude

    @property
    def swapping_license(self):
        return self.licenses[swap_lib.SWAPPING_LICENSE_NFT_POLICYID_CLP]

    @property
    def matcher_licenses(self):
        return self.licenses[mm_lib.LICENSE_NFT_POLICYID]

    def trigger_batcher(self):
        buy, sell = self.orders
        pools = [p for p in self.pools if p.utxo.owner in lp_lib.POOL_CONTRACT_CLP]
        for allowed_contracts in [
            {
                ORDERBOOK_CONTRACT_V2[NETWORK],
                ORDERBOOK_CONTRACT_V3[NETWORK],
                ORDERBOOK_CONTRACT_V4[NETWORK],
            },
            {ORDERBOOK_CONTRACT_V3[NETWORK], ORDERBOOK_CONTRACT_V4[NETWORK]},
        ]:
            filtered_buy, filtered_sell = (
                (o for o in buy if o.utxo.owner in allowed_contracts),
                (o for o in sell if o.utxo.owner in allowed_contracts),
            )
            buy_dd, sell_dd = collect_pairs(filtered_buy, filtered_sell, self.exclude)
            for pool in pools:
                matched = True
                retries = 3
                while matched and retries > 0:
                    if not self.collaterals or any(
                        not l for l in self.licenses.values()
                    ):
                        _LOGGER.warning("No collaterals or swapping licenses found!")
                        self.refetch = True
                        return
                    retries -= 1
                    remove_matched = False
                    matched = False
                    max_matched = 1
                    redeem, matched, new_order, new_pool = swap(
                        buy_dd[pool.tokenB.token][pool.tokenA.token]
                        + sell_dd[pool.tokenA.token][pool.tokenB.token],
                        sell_dd[pool.tokenB.token][pool.tokenA.token]
                        + buy_dd[pool.tokenA.token][pool.tokenB.token],
                        self.wallet_address,
                        pool,
                        max_matched,
                    )
                    if not (redeem and matched):
                        break
                    _LOGGER.info(
                        f"found match, submitting {pool.tokenA.token.to_hex()} - {pool.tokenB.token.to_hex()}"
                    )
                    cs = (
                        random.sample(self.collaterals, len(matched) + 1)
                        if len(self.collaterals) > len(matched)
                        else self.collaterals[:1] * (len(matched) + 1)
                    )
                    b = random.choice(self.swapping_license)
                    if b in self.matcher_licenses:
                        m = b
                    else:
                        m = random.choice(
                            list(
                                set(self.matcher_licenses) - set(self.swapping_license)
                            )
                        )
                    try:
                        swap_lib.build_swap_tx(
                            pool,
                            new_pool,
                            new_order,
                            matched,
                            redeem,
                            self.wallet_address,
                            self.wallet_skeyfile,
                            cs,
                            b,
                            m,
                        )
                        self.refetch = True
                        remove_matched = True

                    except clusterlib.CLIError as e:
                        _LOGGER.info(f"Failed to build transaction")
                        if "not present in the UTxO" in str(
                            e
                        ) or "BadInputsUTxO" in str(e):
                            _LOGGER.error(f"utxo was spent in the meantime")
                            remove_matched = True
                            self.refetch = True
                            break
                    except Exception as e:
                        _LOGGER.error(
                            f"Unexpected error when matching {pool}", exc_info=e
                        )
                        self.refetch = True
                    if remove_matched:
                        for o in set(matched):
                            if o in buy:
                                buy.remove(o)
                            if o in sell:
                                sell.remove(o)
                        for c in set(cs):
                            self.collaterals.remove(c)
                        self.swapping_license.remove(b)
                        self.matcher_licenses.remove(m)


if __name__ == "__main__":
    a = generate_argparser()
    a.add_argument(
        "--exclude",
        help="List of tuples of tokens that should not be matched. Use option once for every pair.",
        nargs=2,
        action="append",
        metavar=("buy_token", "sell_token"),
        default=[],
    )
    args = a.parse_args()

    event = SwapBatchObserver(
        args,
        set((Token.from_string(p[0]), Token.from_string(p[1])) for p in args.exclude),
    )
    schedule_observer(event)
