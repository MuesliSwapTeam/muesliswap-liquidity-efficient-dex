from pathlib import Path

import decimal
import uuid
from fractions import Fraction
import math
import contfrac

from cardano_clusterlib import clusterlib
from src.batch_lib import generate_argparser
from time import sleep

from .. import cli_lib, lp_lib
from cardano_python_utils.clusterfuck import all_utxodata
from ..util import *

from cardano_python_utils import clusterfuck, minutxo
from cardano_python_utils.util import load_wallet


WALLET_ADDRESS = Bech32Addr(
    "addr1q8ms3q3ums0czfknehsg2t3xldkk89dpms2j2smk38awku4cvtjm8enawhyjjkcf6eves2cwz4c8y9tvhjuzpvmu4rws8vt8tn"
)
WALLET_SKEYFILE = Path("clp_setup.skey")


METADATA_MINT_FILE_CONTENT = {
    "674": {"msg": ["MuesliSwap Create Pool"]},
}

MINIMUM_LQ = 1000
MINIMUM_POOL_CRATION_ADA = 6000000
MINIMUM_CONC_POOL_CREATION_ADA = 7000000
MIN_POOL_ATTACHED_LVL = 2000000
INIT_RANGE_SQRT_APPROX_EPS = 10e-3

from ..lp_lib import (
    POOL_CONTRACT_CLP,
    MuesliSwapLpTokenPolicyId,
    MuesliSwapPoolFactoryToken,
    MuesliSwapPoolNFTPolicyId,
    ConcentratedPool,
    pool_datum_content_clp,
    pool_creation_id,
)


def calc_initial_lq_concentrated(
    asset_a: Asset,
    asset_b: Asset,
    pASqrt: Fraction,
    pBSqrt: Fraction,
    precomp_frac: Fraction,
    prec: int,
) -> int:
    L = lp_lib.cal_liquidity_concentrated(
        asset_a.amount, asset_b.amount, pASqrt, pBSqrt, precomp_frac, prec
    )
    # P = lp_lib.cal_price_sqrt_concentrated(asset_b.amount, pASqrt, L) ** 2
    return lp_lib.cal_floor(L)


def approximate_sqrt_as_frac(x: float, eps: float) -> Fraction:
    i = 0
    true_sqrt = math.sqrt(x)
    if true_sqrt.is_integer():
        return Fraction(int(true_sqrt))
    a_num, a_den = contfrac.convergent(true_sqrt, i)
    approx = Fraction(a_num, a_den)
    while abs(float(approx) - true_sqrt) > eps:
        i += 1
        a_num, a_den = contfrac.convergent(true_sqrt, i)
        approx = Fraction(a_num, a_den)
    return approx


def create_pool(
    wallet: ShelleyAddress,
    signing_key: Path,
    dest: ShelleyAddress,
    ref_utxo: TxO,
    collaterals: List[TxO],
    creator_license: TxO,
    p_a: float,
    p_b: float,
    prec: int,
):
    # MISC setup
    uid = uuid.uuid4()
    txins = list(
        sorted([ref_utxo, creator_license], key=lambda x: (x.tx_hash, x.index))
    )
    license_index = txins.index(creator_license)
    collaterals_data = [
        clusterfuck.all_utxodata(collateral, wallet.bech32)
        for collateral in collaterals
    ]
    # TODO make configurable
    pool_contract = POOL_CONTRACT_CLP[0]
    pool_fee = decimal.Decimal("0.3")
    # Determine pool assets
    asset_a = ref_utxo.assets[0]
    asset_b = Asset(
        ref_utxo.amount - MINIMUM_CONC_POOL_CREATION_ADA + MIN_POOL_ATTACHED_LVL,
        LOVELACE,
    )
    for a in ref_utxo.assets:
        if a.token != asset_a.token:
            asset_b = a
    asset_a, asset_b = sorted((asset_a, asset_b), key=lambda a: a.token)
    pASqrt = approximate_sqrt_as_frac(p_a, INIT_RANGE_SQRT_APPROX_EPS)
    pBSqrt = approximate_sqrt_as_frac(p_b, INIT_RANGE_SQRT_APPROX_EPS)
    precomp_frac = 1 - pASqrt / pBSqrt
    print("Price range [", float(pASqrt**2), ",", float(pBSqrt**2), "].")
    # determine amount of initial liquidity
    initial_lq = calc_initial_lq_concentrated(
        asset_a, asset_b, pASqrt, pBSqrt, precomp_frac, prec
    )
    # determine pool id
    pool_id = pool_creation_id(ref_utxo.tx_hash, ref_utxo.index)
    lp_token = Token(MuesliSwapLpTokenPolicyId[pool_contract], pool_id)
    # construct pool
    pool_args = {
        "provider": "muesliswapv2",
        "tokenA": asset_a,
        "tokenB": asset_b,
        "fee": pool_fee,
        "profit_sharing_fee": None,
        "batcher_fee": lp_lib.BATCHER_FEE,
        "deposit": lp_lib.DEPOSIT_LOVELACE,
        "utxo": None,
        "pool_id": Token(MuesliSwapPoolNFTPolicyId[pool_contract], pool_id),
        "timestamp": None,
        "profit_sharing_amt": None,
        "lp_asset": Asset(initial_lq, lp_token),
        "profit_sharing_dest": None,
        "root_k_last": 0,
    }
    pool_args["priceA_sqrt"] = pASqrt
    pool_args["priceB_sqrt"] = pBSqrt
    pool_args["precomp_frac"] = precomp_frac
    pool_args["approx_sqrt_prec"] = prec
    pool = ConcentratedPool(**pool_args)

    # generate pool datum
    pool_datum = pool_datum_content_clp(pool)
    pool_datum_file = TX_FILES_DIR / f"datum_{uid}_new_pool.txt"
    if not pool_datum_file.exists():
        with open(pool_datum_file, "w") as pool_datum_file_fp:
            pool_datum_file_fp.write(pool_datum)
    # create factory nft
    mint_factory_txout = clusterlib.TxOut(
        pool_contract,
        1,
        MuesliSwapPoolFactoryToken[pool_contract].to_cardano_cli(),
        datum_embed_file=pool_datum_file,
    )
    mint_factory_redeemer = lp_lib.FACTORY_REDEEMER_FILE_CONTENT % license_index
    mint_factory_redeemer_file = TX_FILES_DIR / f"redeemer_{uid}_factory.txt"
    if not mint_factory_redeemer_file.exists():
        with open(mint_factory_redeemer_file, "w") as mint_factory_redeemer_file_fp:
            mint_factory_redeemer_file_fp.write(mint_factory_redeemer)
    mint_factory = clusterlib.Mint(
        [mint_factory_txout],
        script_file=lp_lib.FACTORY_MINTING_CLP_SCRIPT_FILE,
        reference_txin=lp_lib.FACTORY_MINTING_CLP_REFUTXO(),
        reference_type=clusterlib.ScriptTypes.PLUTUS_V2,
        collaterals=collaterals_data[0],
        redeemer_file=mint_factory_redeemer_file,
        policyid=MuesliSwapPoolFactoryToken[pool_contract].policy_id,
    )
    # create pool nft
    pool_nft = Token(MuesliSwapPoolNFTPolicyId[pool_contract], pool_id)
    mint_pool_nft_txout = clusterlib.TxOut(
        pool_contract,
        1,
        pool_nft.to_cardano_cli(),
        datum_embed_file=pool_datum_file,
    )
    mint_nft_redeemer = lp_lib.NFT_REDEEMER_FILE_CONTENT % (
        ref_utxo.tx_hash,
        ref_utxo.index,
    )
    mint_nft_redeemer_file = TX_FILES_DIR / f"redeemer_{uid}_nft.txt"
    if not mint_nft_redeemer_file.exists():
        with open(mint_nft_redeemer_file, "w") as mint_nft_redeemer_file_fp:
            mint_nft_redeemer_file_fp.write(mint_nft_redeemer)
    mint_pool_nft = clusterlib.Mint(
        [mint_pool_nft_txout],
        script_file=lp_lib.NFT_MINTING_CLP_SCRIPT_FILE,
        reference_txin=lp_lib.NFT_MINTING_CLP_REFUTXO(),
        reference_type=clusterlib.ScriptTypes.PLUTUS_V2,
        collaterals=collaterals_data[1],
        redeemer_file=mint_nft_redeemer_file,
        policyid=pool_nft.policy_id,
    )
    # create lp tokens
    mint_pool_lp_txout = clusterlib.TxOut(
        dest.bech32,
        initial_lq - MINIMUM_LQ,
        lp_token.to_cardano_cli(),
    )
    mint_pool_lp = clusterlib.Mint(
        txouts=[mint_pool_lp_txout],
        script_file=lp_lib.LP_TOKEN_CLP_SCRIPT_FILE,
        reference_txin=lp_lib.LP_TOKEN_CLP_REFUTXO(),
        reference_type=clusterlib.ScriptTypes.PLUTUS_V2,
        collaterals=collaterals_data[2],
        redeemer_file=lp_lib.BATCH_ORDER_REDEEMER_FILE,
        policyid=lp_token.policy_id,
    )
    # send lp assets to pool
    # send minimum pool deposit to pool
    txouts_assets = [
        clusterlib.TxOut(
            pool_contract,
            a.amount,
            a.token.to_cardano_cli(),
            datum_embed_file=pool_datum_file,
        )
        for a in (asset_a, asset_b)  # , Asset(MIN_POOL_ATTACHED_LVL, LOVELACE))
    ]
    if asset_b.token != LOVELACE and asset_a.token != LOVELACE:
        min_attached = (
            minutxo.min_utxo_ada((LOVELACE, asset_a.token, asset_b.token), True)
            + 200000
        )
        txouts_assets.append(
            clusterlib.TxOut(
                pool_contract,
                min_attached,
                clusterlib.DEFAULT_COIN,
                datum_embed_file=pool_datum_file,
            )
        )
    # send some ada with the lp tokens
    txout_lp_attachment = clusterlib.TxOut(
        dest.bech32, MIN_POOL_ATTACHED_LVL, LOVELACE.to_cardano_cli()
    )
    # send back the creator license
    tx_out_creator = [
        clusterlib.TxOut(
            wallet.bech32,
            creator_license.amount + 100000,
        )
    ] + [
        clusterlib.TxOut(
            wallet.bech32,
            a.amount,
            a.token.to_cardano_cli(),
        )
        for a in creator_license.assets
    ]

    # build tx
    plutus_tx_built = cli_lib.CL.build_tx(
        src_address=wallet.bech32,
        tx_name=uid,
        txins=all_utxodata(ref_utxo, wallet.bech32)
        + all_utxodata(creator_license, wallet.bech32),
        txouts=txouts_assets
        + [
            mint_pool_lp_txout,
            mint_pool_nft_txout,
            mint_factory_txout,
            txout_lp_attachment,
        ]
        + tx_out_creator,
        mint=[mint_pool_nft, mint_pool_lp, mint_factory],
        change_address=dest.bech32,
        invalid_hereafter=clusterfuck.slot_no(cli_lib.CL) + 10000,
        join_txouts=True,
        destination_dir=TX_FILES_DIR,
    )
    plutus_tx_signed = cli_lib.CL.sign_tx(
        signing_key_files=[signing_key],
        tx_name=uid,
        tx_body_file=plutus_tx_built.out_file,
        destination_dir=TX_FILES_DIR,
    )
    cli_lib.CL.submit_tx_bare(plutus_tx_signed)
    submit_ext(plutus_tx_signed, BLOCKFROST)


def main(
    signing_key: Path,
    p_a: float,
    p_b: float,
    prec: int,
):
    _, _, wallet_addr = load_wallet(signing_key)
    wallet = ShelleyAddress.from_bech32(wallet_addr.to_primitive())
    while True:
        try:
            utxos = cli_lib.fetch_utxos(wallet.bech32)
            collaterals = []
            pool_requests = []
            license_utxo = []
            for utxo in utxos:
                if utxo.amount >= MIN_COLLATERAL_AMOUNT and len(utxo.assets) == 0:
                    collaterals.append(utxo)
                    continue
                elif any(
                    cli_lib.valid_license(a, lp_lib.CREATOR_LICENSE_NFT_POLICYID)
                    for a in utxo.assets
                ):
                    license_utxo.append(utxo)
                    continue
                elif (
                    utxo.amount < MINIMUM_CONC_POOL_CREATION_ADA or len(utxo.assets) < 1
                ):
                    continue
                sa = sender_addresses(utxo.tx_hash, BLOCKFROST)
                if wallet.bech32 not in sa:
                    pool_requests.append((ShelleyAddress.from_bech32(sa[0]), utxo))
                    continue
            for sa, pr in pool_requests:
                c = random.sample(collaterals, 3)
                l = license_utxo.pop(0)
                create_pool(wallet, signing_key, sa, pr, c, l, p_a, p_b, prec)
        except KeyboardInterrupt as e:
            return
        except Exception as e:
            print(e)
        sleep(2)


if __name__ == "__main__":
    a = generate_argparser()
    a.add_argument(
        "--p_a", help="Lower bound for price range", type=float, default=0.97
    )
    a.add_argument(
        "--p_b", help="Upper bound for price range", type=float, default=1.03
    )
    a.add_argument(
        "--prec",
        help="Precision of pool's approximate sqrt computations",
        type=int,
        default=5,
    )
    args = a.parse_args()

    main(
        args.wallet_signing_key,
        args.p_a,
        args.p_b,
        args.prec,
    )
