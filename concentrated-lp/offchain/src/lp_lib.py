from __future__ import annotations

from collections import defaultdict
from hashlib import sha256

from typing import Dict, Tuple

import enum
import fractions
import math

from fractions import Fraction

import dataclasses
import decimal

from cardano_python_utils.pools.lp_lib import parse_wallet_address, parse_token
from src.util import datums_be, BLOCKFROST, CARDANO_DIR, METADATA_DIR
from .cli_lib import *

_LOGGER = logging.getLogger(__name__)

#####################################################################################
# Classes
#####################################################################################

# PLUTUS V1
POOL_CONTRACT_PV1 = [
    "addr1z85t4tvj3rwf40wqnx6x72kqq6c6stra7jvkupnlqrqyarthhd58w0qrqpyv4dc2c2mk98sduawl7l4gjuc9rafyv98sgylfw3"
    if MAINNET
    else Bech32Addr("addr_test1wr5t4tvj3rwf40wqnx6x72kqq6c6stra7jvkupnlqrqyarg3n2fvk")
]
BATCHING_CONTRACT_PV1 = (
    "addr1wydncknydgqcur8m6s8m49633j8f2hjcd8c2l48cc45yj0s4ta38n"
    if MAINNET
    else Bech32Addr("addr_test1wqdncknydgqcur8m6s8m49633j8f2hjcd8c2l48cc45yj0swrfdgk")
)
# PLUTUS V2
# TODO add stake hash
POOL_CONTRACT_PV2 = (
    [
        "addr1z9cy2gmar6cpn8yymll93lnd7lw96f27kn2p3eq5d4tjr7rshnr04ple6jjfc0cvcmcpcxmsh576v7j2mjk8tw890vespzvgwd",
        "addr1z9cy2gmar6cpn8yymll93lnd7lw96f27kn2p3eq5d4tjr7xnh3gfhnqcwez2pzmr4tryugrr0uahuk49xqw7dc645chscql0d7",
    ]
    if MAINNET
    else ["addr_test1wz8405hch8x9q5y86gnqzl7xd93a7m6pp006c7677uykvfcclvwrj"]
)
BATCHING_CONTRACT_PV2 = (
    "addr1w9e7m6yn74r7m0f9mf548ldr8j4v6q05gprey2lhch8tj5gsvyte9"
    if MAINNET
    else Bech32Addr("addr_test1wryck3ywna60vxkw4l8053tfw67na38eyjr5dz8f9mygfhqe58zwn")
)

# concentrated LP
POOL_CONTRACT_CLP = (
    [
        "addr1z9qndmhduxjfqvz9rm36p8vsp9vm4l40mx6ndevngkk8srm28uczn6ce6zd5nx2dgr2sza96juq73qz4uhsdxaq74ghs3mz5fw"
    ]
    if MAINNET
    else ["addr_test1wzk7c66pzxu8dlv7xmspquxfh542l52x96hx96f8h4692dq2tvxua"]
)
BATCHING_CONTRACT_CLP = (
    "addr1w87gl00kfuj7qnk8spf25x5e0wfcvasgj5tq3lt5egh6swc4aa5lh"
    if MAINNET
    else Bech32Addr("addr_test1wq0vjevww9n9macx0hjx85sadv4gg2h64mjxem4fd8ylgucw5ktrj")
)


MuesliSwapLpTokenPolicyId: Dict[Bech32Addr, PolicyId] = {}
MuesliSwapLpTokenPolicyId.update(
    (p, PolicyId("8c9a2d459d2d8dc7c11192f971ab647fac65833121b7e8181e583c64"))
    for p in POOL_CONTRACT_PV1
)
MuesliSwapLpTokenPolicyId.update(
    (p, PolicyId("af3d70acf4bd5b3abb319a7d75c89fb3e56eafcdd46b2e9b57a2557f"))
    for p in POOL_CONTRACT_PV2
)
MuesliSwapLpTokenPolicyId.update(
    (p, PolicyId("6e3af9667763e915c9b3a901d3092625d515c2ad6d575eac92582aa8"))
    for p in POOL_CONTRACT_CLP
)

# debug
# MuesliSwapLpTokenPolicyId.update((p, PolicyId("7d25698d628a5252a9a912c21ec54c57d605f551378ce268c33d988e")) for p in POOL_CONTRACT_PV2)

ACCOMPANY_LOVELACE = 2000000
DEPOSIT_LOVELACE = 2000000
BATCHER_FEE = Asset(2000000, LOVELACE)


@dataclasses.dataclass(frozen=True)
class LpProvider:
    name: str


@dataclasses.dataclass()
class Pool:
    provider: LpProvider
    tokenA: Asset
    tokenB: Asset
    fee: decimal.Decimal
    # Fee for the matchmaker
    batcher_fee: Asset
    # Deposit, in lovelace
    deposit: int
    deposit_fee: int
    utxo: TxO
    pool_id: Token
    # last time the pool was queried
    timestamp: datetime.datetime
    profit_sharing_fee: Optional[Fraction] = None
    # amount of lp tokens accompanying the pool
    profit_sharing_amt: Optional[Asset] = None
    # total amount of lp tokens in existence
    lp_asset: Optional[Asset] = None
    profit_sharing_dest: Optional[ShelleyAddress] = None
    root_k_last: int = 0

    @property
    def L(self):
        return self.tokenA.amount * self.tokenB.amount

    def pool_ineq(self, new_tokenA_amount, new_tokenB_amount):
        if not (new_tokenA_amount >= 0 and new_tokenB_amount >= 0):
            return False
        in_a, in_b = max(new_tokenA_amount - self.tokenA.amount, 0), max(
            new_tokenB_amount - self.tokenB.amount, 0
        )
        fee_numerator, fee_denominator = self.fee.as_integer_ratio()
        fee_denominator *= 100  # stored as percent
        return ((new_tokenA_amount * fee_denominator) - (in_a * fee_numerator)) * (
            (new_tokenB_amount * fee_denominator) - (in_b * fee_numerator)
        ) >= (
            fee_denominator * fee_denominator * self.tokenA.amount * self.tokenB.amount
        )

    def pool_out(self, asset_in: Asset):
        """
        binary seach for largest amount_out s.t. pool inequality still satisfied
        there are more efficient ways, see "pool_in" / "pooL_out" - but this is a generic operation
        """
        # either input tokenA or tokenB
        assert (
            asset_in.token == self.tokenA.token or asset_in.token == self.tokenB.token
        )
        token_out = (
            self.tokenA.token
            if asset_in.token == self.tokenB.token
            else self.tokenB.token
        )
        token_in = asset_in.token
        amount_in = asset_in.amount
        # the upper bound is giving away everything that is in the pool
        lo, hi = 0, (
            self.tokenA.amount
            if asset_in.token == self.tokenB.token
            else self.tokenB.amount
        )
        while lo < hi:
            mid = (lo + hi) // 2
            if token_in == self.tokenA.token:
                new_state = (self.tokenA.amount + amount_in, self.tokenB.amount - mid)
            else:
                new_state = (self.tokenA.amount - mid, self.tokenB.amount + amount_in)
            if self.pool_ineq(*new_state):
                lo = mid + 1
            else:
                hi = mid
        amount_out = lo - 1
        if token_in == self.tokenA.token:
            new_state = (
                self.tokenA.amount + amount_in,
                self.tokenB.amount - amount_out,
            )
        else:
            new_state = (
                self.tokenA.amount - amount_out,
                self.tokenB.amount + amount_in,
            )
        return Asset(amount_out, token_out), new_state

    def pool_in(self, asset_out: Asset):
        """
        binary seach for smallest amount_in s.t. pool inequality still satisfied
        there are more efficient ways, see "pool_in" / "pooL_out" - but this is a generic operation
        """
        # either input tokenA or tokenB
        assert (
            asset_out.token == self.tokenA.token or asset_out.token == self.tokenB.token
        )
        # binary seach for largest tokenB_out s.t. pool inequality still satisfied
        token_out = asset_out.token
        token_in = (
            self.tokenA.token
            if asset_out.token == self.tokenB.token
            else self.tokenB.token
        )
        # TODO there are more efficient ways, see "pool_in" / "pooL_out"
        amount_out = asset_out.amount
        pool_owns = (
            self.tokenA.amount
            if asset_out.token == self.tokenA.token
            else self.tokenB.amount
        )
        assert (
            amount_out < pool_owns
        ), "Can not draw more from the pool than the maximum"
        hi = pool_owns
        # exponential growth
        while True:
            if token_in == self.tokenA.token:
                new_state = (self.tokenA.amount + hi, self.tokenB.amount - amount_out)
            else:
                new_state = (self.tokenA.amount - amount_out, self.tokenB.amount + hi)
            if self.pool_ineq(*new_state):
                break
            hi *= 2
        lo = 0
        # binary search
        while lo < hi:
            mid = (lo + hi) // 2
            if token_in == self.tokenA.token:
                new_state = (self.tokenA.amount + mid, self.tokenB.amount - amount_out)
            else:
                new_state = (self.tokenA.amount - amount_out, self.tokenB.amount + mid)
            if self.pool_ineq(*new_state):
                hi = mid
            else:
                lo = mid + 1
        amount_in = lo
        if token_in == self.tokenA.token:
            new_state = (
                self.tokenA.amount + amount_in,
                self.tokenB.amount - amount_out,
            )
        else:
            new_state = (
                self.tokenA.amount - amount_out,
                self.tokenB.amount + amount_in,
            )
        return Asset(amount_in, token_in), new_state

    def copy_with_state(self, tokenA: Asset, tokenB: Asset):
        return Pool(
            provider=self.provider,
            tokenA=tokenA,
            tokenB=tokenB,
            fee=self.fee,
            profit_sharing_fee=self.profit_sharing_fee,
            batcher_fee=self.batcher_fee,
            deposit=self.deposit,
            deposit_fee=self.deposit_fee,
            utxo=self.utxo,
            pool_id=self.pool_id,
            timestamp=self.timestamp,
            profit_sharing_amt=self.profit_sharing_amt,
            lp_asset=self.lp_asset,
            profit_sharing_dest=self.profit_sharing_dest,
            root_k_last=self.root_k_last,
        )


@dataclasses.dataclass()
class ConcentratedPool(Pool):
    priceA_sqrt: Fraction = None
    priceB_sqrt: Fraction = None
    approx_sqrt_prec: int = 10
    precomp_frac: Fraction = None

    @property
    def L(self):
        return cal_liquidity_concentrated(
            self.tokenA.amount,
            self.tokenB.amount,
            self.priceA_sqrt,
            self.priceB_sqrt,
            self.precomp_frac,
            self.approx_sqrt_prec,
        )

    @property
    def virt_tokenA(self):
        return Asset(
            self.tokenA.amount + (self.L / self.priceB_sqrt), self.tokenA.token
        )

    @property
    def virt_tokenB(self):
        return Asset(
            self.tokenB.amount + (self.L * self.priceA_sqrt), self.tokenB.token
        )

    def pool_ineq(self, new_tokenA_amount: int, new_tokenB_amount: int) -> bool:
        if not (new_tokenA_amount >= 0 and new_tokenB_amount >= 0):
            return False
        L_before = self.L
        in_a, in_b = max(new_tokenA_amount - self.tokenA.amount, 0), max(
            new_tokenB_amount - self.tokenB.amount, 0
        )
        L_int = cal_liquidity_concentrated(
            (
                new_tokenA_amount
                - (in_a * (Fraction(*self.fee.as_integer_ratio())) / 100)
            ),
            (
                new_tokenB_amount
                - (in_b * (Fraction(*self.fee.as_integer_ratio())) / 100)
            ),
            self.priceA_sqrt,
            self.priceB_sqrt,
            self.precomp_frac,
            self.approx_sqrt_prec,
        )
        return L_int >= L_before

    @property
    def sqrt_P(self):
        return cal_price_sqrt_concentrated(self.tokenB.amount, self.L, self.priceA_sqrt)

    def copy_with_state(self, tokenA: Asset, tokenB: Asset):
        return ConcentratedPool(
            provider=self.provider,
            tokenA=tokenA,
            tokenB=tokenB,
            fee=self.fee,
            profit_sharing_fee=self.profit_sharing_fee,
            batcher_fee=self.batcher_fee,
            deposit=self.deposit,
            deposit_fee=self.deposit_fee,
            utxo=self.utxo,
            pool_id=self.pool_id,
            timestamp=self.timestamp,
            profit_sharing_amt=self.profit_sharing_amt,
            lp_asset=self.lp_asset,
            profit_sharing_dest=self.profit_sharing_dest,
            root_k_last=self.root_k_last,
            priceA_sqrt=self.priceA_sqrt,
            priceB_sqrt=self.priceB_sqrt,
            precomp_frac=self.precomp_frac,
            approx_sqrt_prec=self.approx_sqrt_prec,
        )


###################################################################################
# Abstract handling of open Batching orders
###################################################################################


def cal_sqrt_floor(x: int):
    if x < 0:
        raise RuntimeError(f"Negative sqrt: {x}")
    elif x == 0:
        return 0
    elif x == 1:
        return 1
    elif x == 2:
        return 1
    else:

        def go(i1: int, i2: int):
            if i2 < i1:
                return go(i2, ((x // i2 + i2) // 2))
            else:
                return i1

        return go(x, (x // 2 + 1))


def pool_creation_id(ref_hash: str, ref_idx: int):
    return HexTokenName(
        sha256(bytes.fromhex(ref_hash + str(ref_idx).encode("utf-8").hex())).hexdigest()
    )


def cal_sqrt_ceil(x: int):
    sqrt = cal_sqrt_floor(x)
    return sqrt + 1 if sqrt * sqrt < x else sqrt


def cal_pow(a, b):
    if b <= 0:
        return 1
    return a * cal_pow(a, b - 1)


def num_len(x: int) -> int:
    if x == 0:
        return 0
    else:
        return 1 + num_len(x // 10)


def cal_approx_sqrt(x: Fraction, prec: int) -> Fraction:
    f = cal_pow(10, max(0, prec - min(num_len(x.numerator), num_len(x.denominator))))
    return Fraction(cal_sqrt_floor(x.numerator * f), cal_sqrt_ceil(x.denominator * f))


def _quot(a, b):
    return a // b if a * b > 0 else (a + (-a % b)) // b


def _rem(x, y):
    return x - _quot(x, y) * y


def cal_ceil(x: Fraction) -> int:
    n, s = (
        _quot(x.numerator, x.denominator),
        fractions.Fraction(_rem(x.numerator, x.denominator), x.denominator),
    )
    if x >= 0:
        if s > 0:
            return n + 1
        else:
            return n
    raise RuntimeError(f"Invalid x {x}")


def cal_floor(x: Fraction) -> int:
    return x // 1


def cal_liquidity_concentrated(
    x: int,
    y: int,
    pASqrt: Fraction,
    pBSqrt: Fraction,
    precomputed_fraction: Fraction,
    prec: int,
) -> Fraction:
    assert x >= 0 and y >= 0
    assert 0 <= pASqrt < pBSqrt
    A = precomputed_fraction
    B = Fraction(y) / pBSqrt + Fraction(x) * pASqrt
    return (B + cal_approx_sqrt(Fraction(B**2) + Fraction(4 * x * y) * A, prec)) / (
        2 * A
    )


def cal_price_sqrt_concentrated(y: int, L: Fraction, pASqrt: Fraction) -> Fraction:
    return Fraction(y) / L + pASqrt


def calculate_deposit_amount_concentrated(
    dep_tokenA: Asset,
    dep_tokenB: Asset,
    pool_tokenA: Asset,
    pool_tokenB: Asset,
    total_liquidity: int,
    priceASqrt: fractions.Fraction,
    priceBSqrt: fractions.Fraction,
    precomputed_fraction: fractions.Fraction,
    prec: int,
):
    curL = cal_liquidity_concentrated(
        pool_tokenA.amount,
        pool_tokenB.amount,
        priceASqrt,
        priceBSqrt,
        precomputed_fraction,
        prec,
    )
    priceSqrt = cal_price_sqrt_concentrated(pool_tokenB.amount, curL, priceASqrt)

    if priceSqrt < priceASqrt:
        delta_Ap = dep_tokenA.amount
        delta_Bp = 0
        delta_L = delta_Ap / ((1 / priceASqrt) - (1 / priceBSqrt))

    elif priceASqrt <= priceSqrt <= priceBSqrt:
        delta_liquidityA = dep_tokenA.amount * (
            (priceSqrt * priceBSqrt) / (priceBSqrt - priceSqrt)
        )
        delta_liquidityB = dep_tokenB.amount / (priceSqrt - priceASqrt)
        if delta_liquidityA < delta_liquidityB:
            delta_Ap = dep_tokenA.amount
            delta_Bp = delta_liquidityA * (priceSqrt - priceASqrt)
            delta_L = delta_liquidityA
        elif delta_liquidityA > delta_liquidityB:
            delta_Ap = delta_liquidityB * ((1 / priceSqrt) - (1 / priceBSqrt))
            delta_Bp = dep_tokenB.amount
            delta_L = delta_liquidityB
        else:
            delta_Ap = dep_tokenA.amount
            delta_Bp = dep_tokenB.amount
            delta_L = delta_liquidityA

    else:
        delta_Ap = 0
        delta_Bp = dep_tokenB.amount
        delta_L = delta_Bp / (priceBSqrt - priceASqrt)

    assert delta_Ap >= 0, f"Invalid delta of token A {delta_Ap}"
    assert delta_Bp >= 0, f"Invalid delta of token B {delta_Bp}"
    assert delta_L > 0, f"Invalid lp token delta {delta_L}"

    delta_lp = (total_liquidity * delta_L.numerator * curL.denominator) // (
        delta_L.denominator * curL.numerator
    )
    return (
        Asset(cal_ceil(delta_Ap), dep_tokenA.token),
        Asset(cal_ceil(delta_Bp), dep_tokenB.token),
        delta_lp,
    )


def calculate_withdraw_amount_concentrated(
    pool_tokenA: Asset,
    pool_tokenB: Asset,
    delta_liquidity: int,
    total_liquidity_before: int,
    priceASqrt: fractions.Fraction,
    priceBSqrt: fractions.Fraction,
    precomputed_fraction: fractions.Fraction,
    prec: int,
):
    curL = cal_liquidity_concentrated(
        pool_tokenA.amount,
        pool_tokenB.amount,
        priceASqrt,
        priceBSqrt,
        precomputed_fraction,
        prec,
    )
    priceSqrt = cal_price_sqrt_concentrated(pool_tokenB.amount, curL, priceASqrt)

    delta_L = Fraction(
        curL.numerator * delta_liquidity, total_liquidity_before * curL.denominator
    )

    if priceSqrt < priceASqrt:
        delta_A = delta_L * (1 / priceASqrt - 1 / priceBSqrt)
        delta_B = 0
    elif priceASqrt <= priceSqrt and priceSqrt <= priceBSqrt:
        delta_A = delta_L * (1 / priceSqrt - 1 / priceBSqrt)
        delta_B = delta_L * (priceSqrt - priceASqrt)
    else:
        delta_A = delta_L * (priceBSqrt - priceASqrt)
        delta_B = 0

    assert delta_A >= 0, f"Invalid delta of token A {delta_A}"
    assert delta_B >= 0, f"Invalid delta of token B {delta_B}"
    return Asset(cal_floor(delta_A), pool_tokenA.token), Asset(
        cal_floor(delta_B), pool_tokenB.token
    )


def calculate_profit_sharing(
    root_k_last: int,
    dep_tokenA: Asset,
    dep_tokenB: Asset,
    total_liquidity: int,
    profit_sharing_fee: fractions.Fraction,
):
    if root_k_last <= 0:
        return 0
    root_k = math.sqrt(dep_tokenA.amount * dep_tokenB.amount)
    if root_k <= root_k_last:
        return 0
    numerator = total_liquidity * (root_k - root_k_last)
    denominator = (root_k * (profit_sharing_fee.denominator - 1)) + root_k_last
    liquidity = numerator // denominator
    if liquidity > 0:
        return liquidity
    else:
        return 0


@dataclasses.dataclass(frozen=True)
class BatchingOrder:
    sender: ShelleyAddress
    receiver: ShelleyAddress
    rec_datum_hash: Optional[DatumHash]
    batcher_fee: Asset
    deposit: int
    utxo: TxO
    script_version: Optional[str]


@dataclasses.dataclass(frozen=True)
class Deposit(BatchingOrder):
    tokenA: Asset
    tokenB: Asset
    minimum_lp_coins: int
    pool_id: Token


@dataclasses.dataclass(frozen=True)
class Withdraw(BatchingOrder):
    lp_coins: Asset
    min_tokenA: Asset
    min_tokenB: Asset


###################################################################################
# Building deposit/withdraw txs
###################################################################################

BATCH_ORDER_REDEEMER_FILE_CONTENT = '{"constructor":0,"fields":[]}'
BATCH_ORDER_REDEEMER_FILE = CARDANO_DIR.joinpath("batch_redeemer.txt")
with open(BATCH_ORDER_REDEEMER_FILE, "w") as f:
    f.write(BATCH_ORDER_REDEEMER_FILE_CONTENT)

METADATA_DEPOSIT_FILE = METADATA_DIR.joinpath("deposit.json")
BATCHING_LICENSE_NFT_POLICYID = PolicyId(
    "b686e45c9181618e20e26cf0e2fc1e9f336bb0df914e645b5adad5bd"
)
BATCHING_LICENSE_NFT_POLICYID_CLP = PolicyId(
    "9560ae2a4708f1269fd49f94909a030ae0e2af51dbad34541927a3fc"
)
CREATOR_LICENSE_NFT_POLICYID = PolicyId(
    "ed8074e8768199746ee58aed1f8b005eaa812afa44a268230603e969"
)
CREATOR_LICENSE_NFT_POLICYID_CLP = PolicyId(
    "91835de0e7ae8228a3e79823e8e066897f9c40e1b85c7a88c173a3e9"
)

# TODO not in the contract yet
UTXO_OUT_HASH = ""
UTXO_OUT_FILE = ""

POOL_DATUM_FILE_CONTENT_PV1 = '{"constructor":0,"fields":[{"constructor":0,"fields":[{"bytes":"%s"},{"bytes":"%s"}]},{"constructor":0,"fields":[{"bytes":"%s"},{"bytes":"%s"}]},{"int":%d},{"int":%d},{"constructor":1,"fields":[]},{"int":%d},{"int":%d}]}'
POOL_DATUM_FILE_CONTENT_PS_PV1 = '{"constructor":0,"fields":[{"constructor":0,"fields":[{"bytes":"%s"},{"bytes":"%s"}]},{"constructor":0,"fields":[{"bytes":"%s"},{"bytes":"%s"}]},{"int":%d},{"int":%d},{"constructor":0,"fields":[{"constructor":0,"fields":[%s,{"constructor":1,"fields":[]}]}]},{"int":%d},{"int":%d}]}'
POOL_DATUM_FILE_CONTENT_PV2 = '{"constructor":0,"fields":[{"constructor":0,"fields":[{"bytes":"%s"},{"bytes":"%s"}]},{"constructor":0,"fields":[{"bytes":"%s"},{"bytes":"%s"}]},{"int":%d},{"int":%d}]}'
POOL_DATUM_FILE_CONTENT_CLP = '{"constructor":0,"fields":[{"constructor":0,"fields":[{"bytes":"%s"},{"bytes":"%s"}]},{"constructor":0,"fields":[{"bytes":"%s"},{"bytes":"%s"}]},{"int":%d},{"int":%d},{"constructor":0,"fields":[{"int":%d},{"int":%d}]},{"constructor":0,"fields":[{"int":%d},{"int":%d}]},{"constructor":0,"fields":[{"int":%d},{"int":%d}]},{"int":%d}]}'

POOL_REDEEMER_APPLY_FILE_CONTENT_ENT = '{"constructor":0,"fields":[{"constructor":0,"fields":[{"constructor":0,"fields":[{"bytes":"%s"}]},{"constructor":1,"fields":[]}]},{"int":%d}]}'
POOL_REDEEMER_APPLY_FILE_CONTENT_SKH = '{"constructor":0,"fields":[{"constructor":0,"fields":[{"constructor":0,"fields":[{"bytes":"%s"}]},{"constructor":0,"fields":[{"constructor":0,"fields":[{"constructor":0,"fields":[{"bytes":"%s"}]}]}]}]},{"int":%d}]}'

LP_TOKEN_PV1_SCRIPT_FILE = CARDANO_DIR.joinpath("lp_minting_policy.plutus")
LP_TOKEN_PV2_SCRIPT_FILE = CARDANO_DIR.joinpath("lp_minting_policy_v2.plutus")
LP_TOKEN_CLP_SCRIPT_FILE = CARDANO_DIR.joinpath("lp_minting_policy_clp.plutus")


@lru_cache(maxsize=1)
def LP_TOKEN_PV2_REFUTXO():
    return CL.get_utxo(
        txin="93e5edca3ea8f7417f8c298e155d6ad865d38eed546b35aa8038e8f0360e707b#3"
        if MAINNET
        else "57fb73343572bb13b813aaf6f18243b595cead9de03d5ab6728812bf66a0927c#2"
    )[0]


@lru_cache(maxsize=1)
def LP_TOKEN_CLP_REFUTXO():
    return CL.get_utxo(
        txin="6229530e04fb732aafe0aee5a125cde67b58150026983ff00c8842c2231a8e38#2"
        if MAINNET
        else "1b297ab9cf5e075f037ea6d12b175102d779e51ec1f4c6031cdf72d216d1f076#2"
    )[0]


LP_TOKEN_REDEEMER_CONTENT = '{"bytes":""}'
LP_TOKEN_REDEEMER_FILE = "lp_token_redeemer.txt"
with open(LP_TOKEN_REDEEMER_FILE, "w") as f:
    f.write(LP_TOKEN_REDEEMER_CONTENT)

BATCHER_CONTRACT_PV1_SCRIPT_FILE = CARDANO_DIR.joinpath("batch_order_script.plutus")
BATCHER_CONTRACT_PV2_SCRIPT_FILE = CARDANO_DIR.joinpath("batch_order_v2_script.plutus")
BATCHER_CONTRACT_CLP_SCRIPT_FILE = CARDANO_DIR.joinpath("batch_order_clp_script.plutus")


@lru_cache(maxsize=1)
def BATCHER_CONTRACT_PV2_REFUTXO():
    return CL.get_utxo(
        txin="93e5edca3ea8f7417f8c298e155d6ad865d38eed546b35aa8038e8f0360e707b#1"
        if MAINNET
        else "5a8abcbf45b9b57df095cf754834a15945c9f86c8bd0899e81eeaa583c6ea9c2#1"
    )[0]


@lru_cache(maxsize=1)
def BATCHER_CONTRACT_CLP_REFUTXO():
    return CL.get_utxo(
        txin="b72aaa6bcf47a7904a942bd343ec747b145a5079d4725f30253de27a9173ff75#1"
        if MAINNET
        else "a61c9f4ee1011c79233c3a92cd807963179209f45eeaca3b20cd98b3452f998f#1"
    )[0]


POOL_CONTRACT_PV1_SCRIPT_FILE = CARDANO_DIR.joinpath("pool_script.plutus")
POOL_CONTRACT_PV2_SCRIPT_FILE = CARDANO_DIR.joinpath("pool_v2_script.plutus")
POOL_CONTRACT_CLP_SCRIPT_FILE = CARDANO_DIR.joinpath("pool_clp_script.plutus")


@lru_cache(maxsize=1)
def POOL_CONTRACT_PV2_REFUTXO():
    return CL.get_utxo(
        txin="7d2e796244075c002db3ef68f15b090482f62cf15288603302f569ad5140d8ec#2"
        if MAINNET
        else "57fb73343572bb13b813aaf6f18243b595cead9de03d5ab6728812bf66a0927c#1"
    )[0]


@lru_cache(maxsize=1)
def POOL_CONTRACT_CLP_REFUTXO():
    return CL.get_utxo(
        txin="b72aaa6bcf47a7904a942bd343ec747b145a5079d4725f30253de27a9173ff75#2"
        if MAINNET
        else "a61c9f4ee1011c79233c3a92cd807963179209f45eeaca3b20cd98b3452f998f#2"
    )[0]


NFT_MINTING_PV2_SCRIPT_FILE = CARDANO_DIR.joinpath("nft_minting_policy_v2.plutus")
NFT_MINTING_CLP_SCRIPT_FILE = CARDANO_DIR.joinpath("nft_minting_policy_clp.plutus")


def NFT_MINTING_PV2_REFUTXO():
    return CL.get_utxo(
        txin="7d2e796244075c002db3ef68f15b090482f62cf15288603302f569ad5140d8ec#1"
        if MAINNET
        else "TODO"
    )[0]


@lru_cache(maxsize=1)
def NFT_MINTING_CLP_REFUTXO():
    return CL.get_utxo(
        txin="6229530e04fb732aafe0aee5a125cde67b58150026983ff00c8842c2231a8e38#3"
        if MAINNET
        else "1b297ab9cf5e075f037ea6d12b175102d779e51ec1f4c6031cdf72d216d1f076#3"
    )[0]


NFT_REDEEMER_FILE_CONTENT = """{
    "constructor": 0,
    "fields": [
        {
            "constructor": 0,
            "fields": [
                {
                    "bytes": "%s"
                }
            ]
        },
        {
            "int": %d
        }
    ]
}"""
FACTORY_MINTING_PV2_SCRIPT_FILE = CARDANO_DIR.joinpath(
    "factory_minting_policy_v2.plutus"
)
FACTORY_MINTING_CLP_SCRIPT_FILE = CARDANO_DIR.joinpath(
    "factory_minting_policy_clp.plutus"
)


@lru_cache(maxsize=1)
def FACTORY_MINTING_PV2_REFUTXO():
    return CL.get_utxo(
        txin="93e5edca3ea8f7417f8c298e155d6ad865d38eed546b35aa8038e8f0360e707b#2"
        if MAINNET
        else "TODO"
    )[0]


@lru_cache(maxsize=1)
def FACTORY_MINTING_CLP_REFUTXO():
    return CL.get_utxo(
        txin="6229530e04fb732aafe0aee5a125cde67b58150026983ff00c8842c2231a8e38#1"
        if MAINNET
        else "a38e2139685a6a465d1c455f7073e2d01fdd66332e0a91a21f6a9080c124155c#1"
    )[0]


FACTORY_REDEEMER_FILE_CONTENT = """{
    "constructor": 0,
    "fields": [
        {
            "int": %d
        }
    ]
}"""


def pool_datum_content_pv1(pool: Pool):
    if pool.profit_sharing_dest is None:
        pool_datum = DatumContent(
            POOL_DATUM_FILE_CONTENT_PV1
            % (
                pool.tokenA.token.policy_id,
                pool.tokenA.token.name,
                pool.tokenB.token.policy_id,
                pool.tokenB.token.name,
                pool.lp_asset.amount,
                pool.root_k_last,
                int(pool.fee * 100),
                pool.profit_sharing_fee.denominator,
            )
        )
    else:
        pool_datum = DatumContent(
            POOL_DATUM_FILE_CONTENT_PS_PV1
            % (
                pool.tokenA.token.policy_id,
                pool.tokenA.token.name,
                pool.tokenB.token.policy_id,
                pool.tokenB.token.name,
                pool.lp_asset.amount,
                pool.root_k_last,
                serialize_wallet(pool.profit_sharing_dest),
                int(pool.fee * 100),
                pool.profit_sharing_fee.denominator,
            )
        )
    return pool_datum


def pool_datum_content_pv2(pool: Pool):
    pool_datum = DatumContent(
        POOL_DATUM_FILE_CONTENT_PV2
        % (
            pool.tokenA.token.policy_id,
            pool.tokenA.token.name,
            pool.tokenB.token.policy_id,
            pool.tokenB.token.name,
            pool.lp_asset.amount,
            int(pool.fee * 100),
        )
    )
    return pool_datum


def pool_datum_content_clp(pool: ConcentratedPool):
    pool_datum = DatumContent(
        POOL_DATUM_FILE_CONTENT_CLP
        % (
            pool.tokenA.token.policy_id,
            pool.tokenA.token.name,
            pool.tokenB.token.policy_id,
            pool.tokenB.token.name,
            pool.lp_asset.amount,
            int(pool.fee * 100),
            pool.priceA_sqrt.numerator,
            pool.priceA_sqrt.denominator,
            pool.priceB_sqrt.numerator,
            pool.priceB_sqrt.denominator,
            pool.precomp_frac.numerator,
            pool.precomp_frac.denominator,
            pool.approx_sqrt_prec,
        )
    )
    return pool_datum


####################################################################################################

# Fetching pools
####################################################################################################

MuesliSwap = LpProvider("muesliswap")

MuesliSwapFee = decimal.Decimal("0.3")

MuesliSwapPoolFactoryToken: Dict[Bech32Addr, Token] = {}
MuesliSwapPoolFactoryToken.update(
    (
        p,
        Token(
            "ffcdbb9155da0602280c04d8b36efde35e3416567f9241aff0955269",
            "MuesliSwap_AMM".encode("utf8").hex(),
        ),
    )
    for p in POOL_CONTRACT_PV1
)
MuesliSwapPoolFactoryToken.update(
    (
        p,
        Token(
            "de9b756719341e79785aa13c164e7fe68c189ed04d61c9876b2fe53f",
            "MuesliSwap_AMM".encode("utf8").hex(),
        ),
    )
    for p in POOL_CONTRACT_PV2
)
MuesliSwapPoolFactoryToken.update(
    (
        p,
        Token(
            "f33bf12af1c23d660e29ebb0d3206b0bfc56ffd87ffafe2d36c42a45",
            "MuesliSwap_cLP".encode("utf8").hex(),
        ),
    )
    for p in POOL_CONTRACT_CLP
)
# debug
# MuesliSwapPoolFactoryToken.update((p, Token("639d162fea3896fdcb9a204d5e6f2900c519ae24194339e7109dd410", "MuesliSwap_AMM".encode("utf8").hex())) for p in POOL_CONTRACT_PV2)

MuesliSwapPoolNFTPolicyId: Dict[Bech32Addr, PolicyId] = {}
MuesliSwapPoolNFTPolicyId.update(
    (p, PolicyId("7a8041a0693e6605d010d5185b034d55c79eaf7ef878aae3bdcdbf67"))
    for p in POOL_CONTRACT_PV1
)
MuesliSwapPoolNFTPolicyId.update(
    (p, PolicyId("909133088303c49f3a30f1cc8ed553a73857a29779f6c6561cd8093f"))
    for p in POOL_CONTRACT_PV2
)
MuesliSwapPoolNFTPolicyId.update(
    (p, PolicyId("909133088303c49f3a30f1cc8ed553a73857a29779f6c6561cd8093f"))
    for p in POOL_CONTRACT_CLP
)

# debug
# MuesliSwapPoolNFTPolicyId.update((p, PolicyId("66bfcea5e1e968f988b4149624c4931574accf003a80f8137ac926c7")) for p in POOL_CONTRACT_PV2)


def parse_profit_sharing(datum, mainnet: bool = True):
    if datum["constructor"] == 1:
        return None
    # focus on the datum inside the optional
    datum = datum["fields"][0]
    # assert no out datum hash is set
    assert datum["fields"][1]["constructor"] == 1
    profit_sharing = parse_wallet_address(datum["fields"][0], mainnet)
    return profit_sharing


def serialize_wallet(w: ShelleyAddress):
    if w.is_enterprise:
        return (
            '{"constructor":0,"fields":[{"constructor":0,"fields":[{"bytes":"%s"}]},{"constructor":1,"fields":[]}]}'
            % w.pubkeyhash
        )
    else:
        return (
            '{"constructor":0,"fields":[{"constructor":0,"fields":[{"bytes":"%s"}]},{"constructor":0,"fields":[{"constructor":0,"fields":[{"constructor":0,"fields":[{"bytes":"%s"}]}]}]}]}]}'
            % (w.pubkeyhash, w.stakekeyhash)
        )


def parse_fraction(datum):
    return fractions.Fraction(
        int(datum["fields"][0]["int"]), int(datum["fields"][1]["int"])
    )


def parse_pool_clp(datum):
    try:
        datum = datum["fields"]
        tokenA = parse_token(datum[0])
        tokenB = parse_token(datum[1])
        total_liquidity = int(datum[2]["int"])
        fee = decimal.Decimal(int(datum[3]["int"])) / 100
        priceASqrt = parse_fraction(datum[4])
        priceBSqrt = parse_fraction(datum[5])
        precomp_frac = parse_fraction(datum[6])
        prec = int(datum[7]["int"])
        return (
            tokenA,
            tokenB,
            total_liquidity,
            fee,
            fractions.Fraction(1, 100),
            0,
            None,
            priceASqrt,
            priceBSqrt,
            precomp_frac,
            prec,
        )
    except (KeyError, AssertionError, IndexError, NotImplementedError):
        return None


def fetch_pools():
    all_pools = []
    # TODO
    for pool_contracts in [POOL_CONTRACT_PV1, POOL_CONTRACT_PV2, POOL_CONTRACT_CLP]:
        pools = []
        for pool_contract in pool_contracts:
            for utxo in fetch_utxos(pool_contract):
                try:
                    # finally extract the pool datum and order token1/2 accordingly
                    datum = datums_be.fetch_datum(utxo.datum_hash)
                    assert datum is not None, "datum is None"

                    if pool_contract in POOL_CONTRACT_CLP:
                        pool_content = parse_pool_clp(datum)
                        assert pool_content is not None
                        (
                            tokenA,
                            tokenB,
                            total_liquidity,
                            fee,
                            profit_sharing_fee,
                            root_k_last,
                            profit_sharing,
                            priceASqrt,
                            priceBSqrt,
                            precomp_frac,
                            prec,
                        ) = pool_content
                    else:
                        raise NotImplementedError(
                            f"Pool contract unknown {pool_contract}"
                        )

                    assetA = None
                    assetB = None
                    pool_id = None
                    pool_token_present = False
                    for a in utxo.assets + [Asset(utxo.amount, LOVELACE)]:
                        if a.token == tokenA:
                            assetA = a
                        elif a.token == tokenB:
                            assetB = a
                        elif (
                            a.token.policy_id
                            == MuesliSwapPoolNFTPolicyId[pool_contract]
                        ):
                            pool_id = a.token
                        elif a.token == MuesliSwapPoolFactoryToken[pool_contract]:
                            pool_token_present = True
                    assert not (assetA is None and assetB is None)
                    assert pool_id is not None
                    assert pool_token_present

                    # in the case of concentrated LPs, one token may be missing
                    if assetA is None:
                        assetA = Asset(0, tokenA)
                    if assetB is None:
                        assetB = Asset(0, tokenB)

                    lp_token = Token(
                        MuesliSwapLpTokenPolicyId[pool_contract], pool_id.name
                    )

                    # this does not necessarily hold
                    # assert total_liquidity == floor(sqrt(token1.amount * token2.amount))
                    lp_asset = Asset(total_liquidity, lp_token)
                    try:
                        profit_sharing_amt = next(
                            iter(a for a in utxo.assets if a.token == lp_token)
                        )
                    except StopIteration:
                        profit_sharing_amt = None

                    pool_args = {
                        "tokenA": assetA,
                        "tokenB": assetB,
                        "utxo": utxo,
                        "provider": MuesliSwap,
                        "fee": fee,
                        "batcher_fee": BATCHER_FEE,
                        "deposit": DEPOSIT_LOVELACE,
                        "deposit_fee": BATCHER_FEE,
                        "pool_id": pool_id,
                        "timestamp": datetime.datetime.now(),
                        "lp_asset": lp_asset,
                        "profit_sharing_fee": profit_sharing_fee,
                        "profit_sharing_amt": profit_sharing_amt,
                        "profit_sharing_dest": profit_sharing,
                        "root_k_last": root_k_last,
                    }
                    if pool_contract in POOL_CONTRACT_CLP:
                        pool_args["priceA_sqrt"] = priceASqrt
                        pool_args["priceB_sqrt"] = priceBSqrt
                        pool_args["precomp_frac"] = precomp_frac
                        pool_args["approx_sqrt_prec"] = prec
                        pools.append(ConcentratedPool(**pool_args))
                    else:
                        pools.append(Pool(**pool_args))

                except (AssertionError, blockfrost.ApiError) as e:
                    _LOGGER.info(f"Failed to parse utxo {utxo}: {e}")
                except Exception as e:
                    _LOGGER.error(f"Unexpected error when parsing {utxo}", exc_info=e)
        all_pools.append(pools)
    return all_pools


####################################################################################################
# Fetching batching orders
####################################################################################################

"""
Batch deposit request format
{
      "fields":[
         {
            "fields":[
               {
                  "fields":[
                     {
                        "bytes":"353b8bc29a15603f0b73eac44653d1bd944d92e0e0dcd5eb185164a2"
                     }
                  ],
                  "constructor":0
               },
               {
                  "fields":[
                     {
                        "fields":[
                           {
                              "fields":[
                                 {
                                    "bytes":"da22c532206a75a628778eebaf63826f9d93fbe9b4ac69a7f8e4cd78"
                                 }
                              ],
                              "constructor":0
                           }
                        ],
                        "constructor":0
                     }
                  ],
                  "constructor":0
               }
            ],
            "constructor":0
         },
         {
            "fields":[
               {
                  "fields":[
                     {
                        "bytes":"353b8bc29a15603f0b73eac44653d1bd944d92e0e0dcd5eb185164a2"
                     }
                  ],
                  "constructor":0
               },
               {
                  "fields":[
                     {
                        "fields":[
                           {
                              "fields":[
                                 {
                                    "bytes":"da22c532206a75a628778eebaf63826f9d93fbe9b4ac69a7f8e4cd78"
                                 }
                              ],
                              "constructor":0
                           }
                        ],
                        "constructor":0
                     }
                  ],
                  "constructor":0
               }
            ],
            "constructor":0
         },
         {
            "fields":[
               
            ],
            "constructor":1
         },
         {
            "fields":[
               {
                  "int":1
               }
            ],
            "constructor":0
         },
         {
            "int":2000000
         },
         {
            "int":2000000
         }
      ],
      "constructor":0
   }
"""


class OrderStep(enum.Enum):
    DEPOSIT = 0
    WITHDRAW = 1
    ONESIDEDEPOSIT = 2


@dataclasses.dataclass(frozen=True)
class RawOrderStep:
    order_step: OrderStep


@dataclasses.dataclass(frozen=True)
class RawDeposit(RawOrderStep):
    minimum_lp_coins: int


@dataclasses.dataclass(frozen=True)
class RawWithdraw(RawOrderStep):
    min_tokenA: int
    min_tokenB: int


def parse_order_step(datum):
    order_step = OrderStep(int(datum["constructor"]))
    if order_step == OrderStep.DEPOSIT:
        min_lp_coins = int(datum["fields"][0]["int"])
        return RawDeposit(order_step, min_lp_coins)
    if order_step == OrderStep.WITHDRAW:
        min_tokenA = int(datum["fields"][0]["int"])
        min_tokenB = int(datum["fields"][1]["int"])
        return RawWithdraw(order_step, min_tokenA, min_tokenB)
    if order_step == OrderStep.ONESIDEDEPOSIT:
        raise NotImplementedError("Tried to parse OneSidedSwap")


def parse_batch_clp(datum):
    try:
        send_wallet = parse_wallet_address(datum["fields"][0])
        receive_wallet = parse_wallet_address(datum["fields"][1])
        if datum["fields"][2]["constructor"] == 0:
            rec_datum_hash = datum["fields"][2]["fields"][0]["bytes"]
        else:
            rec_datum_hash = None
        order_step = parse_order_step(datum["fields"][3])
        batcher_fee = Asset(datum["fields"][4]["int"], LOVELACE)
        accompany_lovelace = datum["fields"][5]["int"]
        lp_token = Token(
            MuesliSwapLpTokenPolicyId[POOL_CONTRACT_CLP[0]], datum["fields"][6]["bytes"]
        )
        script_version = datum["fields"][7]["bytes"]
        return (
            send_wallet,
            receive_wallet,
            rec_datum_hash,
            order_step,
            batcher_fee,
            accompany_lovelace,
            lp_token,
            script_version,
        )
    except (KeyError, AssertionError, IndexError, NotImplementedError):
        return None


def collect_pools(
    pools: list[Pool],
) -> Dict[Tuple[Token, Token], List[Pool]]:
    r = defaultdict(list)
    for pool in pools:
        p: Tuple[Token, Token] = tuple(sorted([pool.tokenA.token, pool.tokenB.token]))
        r[p].append(pool)
    return r
