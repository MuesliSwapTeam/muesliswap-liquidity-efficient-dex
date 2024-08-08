from pathlib import Path

import uuid

from fractions import Fraction

import cardano_clusterlib.clusterlib as clusterlib
from . import mm_lib, lp_lib, spectrum_lib, teddy_lib
from cardano_python_utils.clusterfuck import utxodata, all_utxodata
from cardano_python_utils.pools.muesliv2_lib import (
    match_datum_content_pv1,
    match_datum_content_pv2,
)
from .lp_lib import (
    Pool,
    POOL_CONTRACT_PV1,
    POOL_CONTRACT_PV2,
    POOL_CONTRACT_CLP,
    POOL_CONTRACT_PV2_REFUTXO,
    POOL_CONTRACT_CLP_REFUTXO,
)
from .util import *
from .cli_lib import CL, ORDERBOOK_CONTRACT_REFUTXO

from cardano_python_utils import minutxo, clusterfuck

_LOGGER = logging.getLogger(__name__)

SWAPPING_LICENSE_NFT_POLICYID = PolicyId(
    "f94fd008635cf307663aadd995ed69d5fbbfd65f84679fc38254d664"
)
SWAPPING_LICENSE_NFT_POLICYID_CLP = PolicyId(
    "7c05046c325f03e0c995c0bd256dcb5516d1edd567b83d19eb35f725"
)
POOL_REDEEMER_SWAP_FILE_CONTENT = '{"constructor":1,"fields":[{"int":%d}]}'
UTXO_OUT_HASH_2 = f"b{0:063d}"


def valid_swapping_license(a: Asset, is_clp=False):
    return lp_lib.valid_license(
        a,
        SWAPPING_LICENSE_NFT_POLICYID
        if not is_clp
        else SWAPPING_LICENSE_NFT_POLICYID_CLP,
    )


def swapping_licenses(wallet_utxos: List[TxO], is_clp=False) -> List[TxO]:
    """
    Query utxos from the wallet containing batcher licenses
    """
    raw_utxos = sorted(wallet_utxos, key=lambda x: x.amount)
    license_utxos = [
        utxo
        for utxo in raw_utxos
        # batcher license needs to be present
        if any(valid_swapping_license(a, is_clp) for a in utxo.assets)
    ]
    return license_utxos


def build_swap_tx(
    old_pool: Pool,
    new_pool: Pool,
    new_order: mm_lib.Order,
    matched: Dict[mm_lib.Order, Fraction],
    redeem: Dict[Tuple[ShelleyAddress, Optional[mm_lib.Order]], Dict[Token, int]],
    batcher_address: ShelleyAddress,
    batcher_skeyfile: Path,
    collaterals: List[FrozenSet[TxO]],
    swapping_license_utxo: TxO,
    matching_license_utxo: TxO,
):
    uid = uuid.uuid4()
    involves_plutusv1 = old_pool.utxo.owner in POOL_CONTRACT_PV1 or any(
        o.utxo.owner == ORDERBOOK_CONTRACT_V2[NETWORK] for o in matched
    )
    assert new_order is not None, "New order must be present"
    metadata = mm_lib.METADATA_MATCH_PARTIAL_FILE_CONTENT.copy()
    metadata[str(METADATA_ADDRESS_KEY)] = "0x" + new_order.creator.hex
    metadata[str(METADATA_BUY_POLICYID_KEY)] = new_order.buy.token.policy_id
    metadata[str(METADATA_BUY_TOKENNAME_KEY)] = new_order.buy.token.name
    metadata[str(METADATA_SELL_POLICYID_KEY)] = new_order.sell.token.policy_id
    metadata[str(METADATA_SELL_TOKENNAME_KEY)] = new_order.sell.token.name
    metadata[str(METADATA_AMOUNT_KEY)] = str(new_order.buy.amount)
    metadata[str(METADATA_LOVELACE_ATTACHED_KEY)] = str(
        MIN_ADA_TRANSFER + new_order.fee
    )
    metadata[str(METADATA_ALLOW_PARTIAL_KEY)] = "1"
    metadata_file = TX_FILES_DIR / f"metadata_match_{uid}.json"
    with open(metadata_file, "w") as mfp:
        json.dump(metadata, mfp)

    wallet_skeyfile = clusterlib.TxFiles(
        signing_key_files=[batcher_skeyfile],
        metadata_json_files=[metadata_file],
    )

    in_txhashes = (
        [old_pool.utxo, matching_license_utxo, swapping_license_utxo]
        + [m.utxo for m in matched]
        + sum((list(c) for c in collaterals), [])
    )
    in_txhashes = sorted(set(in_txhashes), key=lambda x: (x.tx_hash, x.index))

    try:
        txouts = []
        _LOGGER.debug("Building txouts")
        collaterals_data = [
            [utxodata(u, batcher_address.bech32) for u in c] for c in collaterals
        ]

        # explicitely send back the matching license
        # TODO also send back matching license --> keep both in one?
        match_maker_license = all_utxodata(
            matching_license_utxo, batcher_address.bech32
        )
        for o in match_maker_license:
            if o.coin != "lovelace":
                txouts.append(
                    clusterlib.TxOut(
                        o.address,
                        o.amount,
                        o.coin,
                    )
                )
        # send back at least MIN_TRANSFER_ADA together with the batching license
        min_ada = minutxo.min_utxo_ada(
            set(a.token for a in matching_license_utxo.assets)
            | redeem[(batcher_address, None)].keys(),
            False,
        )
        txouts.append(
            clusterlib.TxOut(
                batcher_address.bech32,
                min_ada - redeem[(batcher_address, None)][LOVELACE],
                clusterlib.DEFAULT_COIN,
            )
        )
        # explicitely send back the swapping license
        # if it is not the same utxo
        if matching_license_utxo != swapping_license_utxo:
            swap_maker_license = all_utxodata(
                swapping_license_utxo, batcher_address.bech32
            )
            min_ada = minutxo.min_utxo_ada(
                (a.token for a in swapping_license_utxo.assets), True
            )
            for o in swap_maker_license:
                txouts.append(
                    clusterlib.TxOut(
                        o.address,
                        o.amount if o.coin != clusterlib.DEFAULT_COIN else min_ada,
                        o.coin,
                        datum_hash=UTXO_OUT_HASH_2,
                    )
                )
        # send back 5 ada for each collateral
        for num_col_gen, cs in enumerate(set(collaterals)):
            ctotal = sum(c.amount for c in cs)
            d = min((ctotal // MIN_COLLATERAL_AMOUNT) - 1, 3)
            for b in range(d):
                txouts.append(
                    clusterlib.TxOut(
                        batcher_address.bech32,
                        MIN_COLLATERAL_AMOUNT,
                        clusterlib.DEFAULT_COIN,
                        datum_hash=f"c{b:010x}{num_col_gen:053x}",
                    )
                )
            txouts.append(
                clusterlib.TxOut(
                    batcher_address.bech32,
                    ctotal - d * MIN_COLLATERAL_AMOUNT,
                    clusterlib.DEFAULT_COIN,
                    datum_hash=f"cc{num_col_gen:062x}",
                )
            )
        txouts.pop(-1)
        # send back the new pool to the contract
        # the datum is the same
        pool_datum_hash = old_pool.utxo.datum_hash
        pool_datum = datums_be.fetch_datum(pool_datum_hash)
        pool_datum_file = datums._store_script_datum(pool_datum_hash, pool_datum)
        datum_param = {
            "inline_datum_file": pool_datum_file
        }
        new_pool_tx_tokenA = clusterlib.TxOut(
            old_pool.utxo.owner,
            new_pool.tokenA.amount,
            new_pool.tokenA.token.to_cardano_cli(),
            **datum_param,
        )
        txouts.append(new_pool_tx_tokenA)
        new_pool_tx_tokenB = clusterlib.TxOut(
            old_pool.utxo.owner,
            new_pool.tokenB.amount,
            new_pool.tokenB.token.to_cardano_cli(),
            **datum_param,
        )
        txouts.append(new_pool_tx_tokenB)
        if new_pool.tokenB.token != LOVELACE and new_pool.tokenA.token != LOVELACE:
            new_pool_tx_lvl = clusterlib.TxOut(
                old_pool.utxo.owner,
                new_pool.utxo.amount,
                LOVELACE.to_cardano_cli(),
                **datum_param,
            )
            txouts.append(new_pool_tx_lvl)
        new_pool_tx_factory = clusterlib.TxOut(
            old_pool.utxo.owner,
            1,
            new_pool.pool_id.to_cardano_cli(),
            **datum_param,
        )
        txouts.append(new_pool_tx_factory)
        new_pool_tx_pooltoken = clusterlib.TxOut(
            old_pool.utxo.owner,
            1,
            lp_lib.MuesliSwapPoolFactoryToken[old_pool.utxo.owner].to_cardano_cli(),
            **datum_param,
        )
        txouts.append(new_pool_tx_pooltoken)
        if new_pool.profit_sharing_dest is not None:
            new_pool_tx_profit_sharing_amt = clusterlib.TxOut(
                old_pool.utxo.owner,
                new_pool.profit_sharing_amt.amount,
                new_pool.profit_sharing_amt.token.to_cardano_cli(),
                **datum_param,
            )
            txouts.append(new_pool_tx_profit_sharing_amt)

        # return tokens to all orders
        for (redeemer, rorder), redeem_values in redeem.items():
            redeemer_address = redeemer.bech32
            for token, amount in redeem_values.items():
                out_datum = (
                    ""
                    if rorder is None
                    else mm_lib.UTXO_OUT_FILE_NAME[rorder.utxo.owner]
                )
                utxo = clusterlib.TxOut(
                    redeemer_address,
                    amount,
                    token.to_cardano_cli(),
                    datum_embed_file=out_datum,
                )
                txouts.append(utxo)
        # send back the new order to the contract
        if new_order is not None:
            # calculate new datum and corresponding hash
            if new_order.utxo.owner == ORDERBOOK_CONTRACT_V2[NETWORK]:
                new_order_datum_content = match_datum_content_pv1(
                    new_order.creator,
                    new_order.buy.token,
                    new_order.sell.token,
                    new_order.buy.amount,
                    1,
                )
            elif (
                new_order.utxo.owner == ORDERBOOK_CONTRACT_V3[NETWORK]
                or new_order.utxo.owner == ORDERBOOK_CONTRACT_V4[NETWORK]
            ):
                new_order_datum_content = match_datum_content_pv2(
                    new_order.creator,
                    new_order.buy.token,
                    new_order.sell.token,
                    new_order.buy.amount,
                    1,
                    MIN_ADA_TRANSFER + new_order.fee,
                )
            else:
                raise NotImplementedError(
                    f"Contract not implemented: {new_order.utxo.owner}"
                )
            new_datum_file = datums.store_script_datum_content(new_order_datum_content)
            new_order_tx = clusterlib.TxOut(
                new_order.utxo.owner,
                new_order.sell.amount,
                new_order.sell.token.to_cardano_cli(),
                datum_embed_file=new_datum_file,
            )
            txouts.append(new_order_tx)
            for a_token, a_amount in new_order.attached.items():
                new_order_tx_attached = clusterlib.TxOut(
                    new_order.utxo.owner,
                    a_amount,
                    a_token.to_cardano_cli(),
                    datum_embed_file=new_datum_file,
                )
                txouts.append(new_order_tx_attached)
        # insert matched orders
        matching_license_index = in_txhashes.index(matching_license_utxo)
        plutus_txins = []
        for i, (order, omatch_frac) in enumerate(matched.items()):
            # compute orginal datum value
            datum_file = datums._store_script_datum_content(
                order.datum_hash, order.datum_content
            )
            # compute redeemer value
            if omatch_frac == 1:
                redeemer_value = (
                    mm_lib.MATCH_REDEEMER_FULL_FILE_CONTENT % matching_license_index
                )
            else:
                redeemer_value = mm_lib.MATCH_REDEEMER_PARTIAL_FILE_CONTENT % (
                    omatch_frac.numerator,
                    omatch_frac.denominator,
                    matching_license_index,
                )
            redeemer_file = (
                TX_FILES_DIR
                / f"redeemer_{order.utxo.tx_hash}_{order.utxo.index}_{matching_license_index}.txt"
            )
            with open(redeemer_file, "w") as redeemer_file_fp:
                redeemer_file_fp.write(redeemer_value)
            if involves_plutusv1:
                txin = clusterlib.ScriptTxIn(
                    txins=mm_lib.orderdata(order),
                    collaterals=collaterals_data[i],
                    script_file=ORDERBOOK_CONTRACT_SCRIPT_FILE[order.utxo.owner],
                    redeemer_file=redeemer_file,
                    datum_file=datum_file,
                )
            else:
                txin = clusterlib.ScriptTxIn(
                    txins=mm_lib.orderdata(order),
                    collaterals=collaterals_data[i],
                    reference_txin=ORDERBOOK_CONTRACT_REFUTXO()[order.utxo.owner],
                    reference_type=clusterlib.ScriptTypes.PLUTUS_V2,
                    redeemer_file=redeemer_file,
                    datum_file=datum_file,
                )
            plutus_txins.append(txin)

        _LOGGER.debug("Building plutus txins")
        # for the pool
        # generate POOL redeemer (including the position of the license)
        # the index of the license index utxo is its position in the sorted list of incoming utxos
        swapping_license_index = in_txhashes.index(swapping_license_utxo)
        pool_redeemer: DatumContent = POOL_REDEEMER_SWAP_FILE_CONTENT % (
            swapping_license_index
        )
        pool_redeemer_file = datums.store_script_datum_content(pool_redeemer)
        assert old_pool.utxo.owner in POOL_CONTRACT_CLP, "Only CLP pools supported"
        txin = clusterlib.ScriptTxIn(
            txins=all_utxodata(old_pool.utxo, old_pool.utxo.owner),
            collaterals=collaterals_data[len(matched)],
            reference_txin=POOL_CONTRACT_CLP_REFUTXO(),
            reference_type=clusterlib.ScriptTypes.PLUTUS_V2,
            redeemer_file=pool_redeemer_file,
            datum_file=pool_datum_file,
        )
        plutus_txins.append(txin)
        _LOGGER.debug("Building transaction")
        plutus_tx_built = CL.build_tx(
            src_address=batcher_address.bech32,
            tx_name=uid,
            tx_files=wallet_skeyfile,
            txins=all_utxodata(matching_license_utxo, batcher_address.bech32)
            + sum(collaterals_data, [])
            + (
                all_utxodata(swapping_license_utxo, batcher_address.bech32)
                if swapping_license_utxo != matching_license_utxo
                else []
            ),
            txouts=txouts,
            # add a buffer of 1 ADA fees
            fee_buffer=1000000,
            # change if this should not be the source address
            #! ATTENTION: ONLY WORKS FOR ADA
            # and is not combined with the txouts -> needs at least 1.8 ada as well
            change_address=batcher_address.bech32,
            script_txins=plutus_txins,
            destination_dir=TX_FILES_DIR,
            # the transaction should be valid for a fair amount of time, but not longer than the license
            invalid_hereafter=clusterfuck.slot_no(CL)
            + 10000,  # slot_of_time(datetime.datetime.now() + min(lp_lib.BATCH_VALIDITY, mm_lib.MATCH_VALIDITY) - datetime.timedelta(minutes=5)),
            join_txouts=True,
            required_signers=[batcher_skeyfile],
        )
        _LOGGER.debug("Signing transaction")
        plutus_tx_signed = CL.sign_tx(
            tx_body_file=plutus_tx_built.out_file,
            signing_key_files=[batcher_skeyfile],
            tx_name=uid,
            destination_dir=TX_FILES_DIR,
        )
        _LOGGER.debug("Submitting transaction")
        # check locally if this can be submitted
        CL.submit_tx_bare(tx_file=plutus_tx_signed)
        # new submit function that submits to a pre-configured number of nodes + blockfrost
        submit_ext(plutus_tx_signed, BLOCKFROST)
    except Exception as e:
        _LOGGER.warning("Unexpected error when building transaction.", exc_info=e)
        raise e
