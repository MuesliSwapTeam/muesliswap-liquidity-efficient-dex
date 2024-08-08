from __future__ import annotations

from pathlib import Path

from dataclasses import dataclass

import datetime
from typing import Dict, List

import logging
import uuid
from cardano_clusterlib import clusterlib

from cardano_python_utils.classes import (
    PolicyId,
    Asset,
    TxO,
    DatumHash,
    Token,
    DatumContent,
    Bech32Addr,
    LOVELACE,
    ShelleyAddress,
)

from cardano_python_utils.clusterfuck import all_utxodata
from cardano_python_utils.pools.muesliv2_lib import (
    ORDERBOOK_CONTRACTS,
    ORDERBOOK_CONTRACT_V2,
    ORDERBOOK_CONTRACT_V3,
    ORDERBOOK_CONTRACT_V4,
)
from cardano_python_utils.util import TX_FILES_DIR, submit_ext
from . import cli_lib
from .cli_lib import CL
from .util import (
    MIN_ADA_TRANSFER,
    NETWORK,
    BLOCKFROST,
    METADATA_DIR,
)

logging.basicConfig(
    format="%(asctime)s %(levelname)-8s %(message)s", level=logging.DEBUG
)
_LOGGER = logging.getLogger(__name__)


# uncomment what you want to use
MIN_TRANSFER_AND_FEE_AMOUNT = MIN_ADA_TRANSFER + 2000000
MIN_COLLATERAL_AMOUNT = 5000000


MATCH_REDEEMER_FULL_FILE_CONTENT = '{"constructor":1,"fields":[{"int":%d}]}'
# Note: format by inserting the nominator and denominator of the rational!
# TODO: test if we can by now directly embed this or whether we have to write it out still
MATCH_REDEEMER_PARTIAL_FILE_CONTENT = '{"constructor":2,"fields":[{"constructor":0,"fields":[{"int":%d},{"int":%d}]}, {"int":%d}]}'
UTXO_OUT_FILE_CONTENT = {
    ORDERBOOK_CONTRACT_V2[NETWORK]: '{"constructor":0,"fields":[{"bytes":"%s"}]}'
    % "MuesliSwap_v2".encode("utf8").hex(),
    ORDERBOOK_CONTRACT_V3[NETWORK]: '{"constructor":0,"fields":[{"bytes":"%s"}]}'
    % "MuesliSwap_v2.1".encode("utf8").hex(),
    ORDERBOOK_CONTRACT_V4[NETWORK]: '{"constructor":0,"fields":[{"bytes":"%s"}]}'
    % "MuesliSwap_v2.2".encode("utf8").hex(),
}
UTXO_OUT_FILE_NAME = {
    ORDERBOOK_CONTRACT_V2[NETWORK]: METADATA_DIR / "utxo_out_v2.txt",
    ORDERBOOK_CONTRACT_V3[NETWORK]: METADATA_DIR / "utxo_out_v3.txt",
    ORDERBOOK_CONTRACT_V4[NETWORK]: METADATA_DIR / "utxo_out_v4.txt",
}
for orderbook_contract in ORDERBOOK_CONTRACTS[NETWORK]:
    with open(UTXO_OUT_FILE_NAME[orderbook_contract], "w") as redeemer_file:
        redeemer_file.write(UTXO_OUT_FILE_CONTENT[orderbook_contract])

MATCH_REDEEMER_FULL_FILE = METADATA_DIR / "redeemer_full.txt"
with open(MATCH_REDEEMER_FULL_FILE, "w") as redeemer_file:
    redeemer_file.write(MATCH_REDEEMER_FULL_FILE_CONTENT)

METADATA_MATCH_FILE = METADATA_DIR.joinpath("match.json")
METADATA_SPECTRUM_MATCH_FILE = METADATA_DIR.joinpath("match_spectrum.json")
METADATA_TEDDY_MATCH_FILE = METADATA_DIR.joinpath("match_teddy.json")
METADATA_MATCH_PARTIAL_FILE_CONTENT: Dict[str, ...] = {
    "674": {"msg": ["MuesliSwap Partial Match Order"]},
}
METADATA_TAKE_FILE = METADATA_DIR.joinpath("take.json")
METADATA_SWAP_REQUEST_CONTENT: Dict[str, ...] = {
    "674": {"msg": ["MuesliSwap Swap Order"]},
}

LICENSE_NFT_POLICYID = PolicyId(
    "5817c34e5702473304f3cf676299176d3824e55b8c0bfa94830429fd"
)
MATCH_VALIDITY = datetime.timedelta(days=1)
UTXO_OUT_HASH_2 = f"b{0:063}"


@dataclass(frozen=True)
class Order:
    creator: ShelleyAddress
    buy: Asset
    sell: Asset
    arrival: datetime.datetime
    utxo: TxO
    datum_hash: DatumHash
    datum_content: DatumContent
    attached: Dict[Token, int]
    fee: int
    partial: int = 1

    def __eq__(self, other):
        if isinstance(other, self.__class__):
            return self.utxo == other.utxo
        return False

    def __hash__(self):
        return hash(self.utxo)

    def __repr__(self):
        return f"Order(buy {self.buy.amount} {self.buy.token} for {self.sell.amount} {self.sell.token} by {self.creator.bech32} ({self.utxo}))"


def orderdata(order: Order) -> List[clusterlib.UTXOData]:
    """Construct utxodata objects from an order"""

    return [
        clusterlib.UTXOData(
            utxo_hash=order.utxo.tx_hash,
            utxo_ix=order.utxo.index,
            address=order.creator.bech32,
            amount=order.sell.amount,
            coin=order.sell.token.to_cardano_cli(),
            datum_hash=order.datum_hash,
        )
    ] + [
        clusterlib.UTXOData(
            utxo_hash=order.utxo.tx_hash,
            utxo_ix=order.utxo.index,
            address=order.creator.bech32,
            amount=a_amount,
            coin=a_token.to_cardano_cli(),
            datum_hash=order.datum_hash,
        )
        for a_token, a_amount in order.attached.items()
    ]


def valid_batcher_license(a: Asset):
    try:
        return (
            a.token.policy_id == LICENSE_NFT_POLICYID
            # the name of the asset is the unix timestamp where it is still valid
            and datetime.datetime.fromtimestamp(int(a.token.name, 16) / 1000)
            > datetime.datetime.now() + MATCH_VALIDITY
        )
    except (ValueError, OverflowError):
        # in case that the value of the batcher license is waaay to large
        return False


def match_maker_licenses(wallet_utxos: List[TxO]) -> list[TxO]:
    """
    Query utxos from the wallet containing batcher licenses
    """
    raw_utxos = sorted(wallet_utxos, key=lambda x: x.amount)
    license_utxos = [
        utxo
        for utxo in raw_utxos
        # batcher license needs to be present
        if any(valid_batcher_license(a) for a in utxo.assets)
    ]
    return license_utxos


def move_utxos(
    src_address: Bech32Addr,
    src_skeyfile: Path,
    target_address: Bech32Addr,
):
    """
    Builds a transaction that moves all utxos from one wallet to another
    :return:
    """
    wallet_skeyfile = clusterlib.TxFiles(
        signing_key_files=[src_skeyfile],
    )
    uid = str(uuid.uuid4())
    try:
        utxos = cli_lib.fetch_utxos(src_address)
        if not utxos:
            return
        # count how much money is present
        txins = []
        txouts = []
        max_utxo = None
        for i, u in enumerate(utxos):
            txins.extend(all_utxodata(u, src_address))
            datum_hash = f"{i:064d}"
            # append asset amount
            txouts.extend(
                clusterlib.TxOut(
                    target_address,
                    a.amount,
                    a.token.to_cardano_cli(),
                    datum_hash=datum_hash,
                )
                for a in u.assets
            )
            # append lovelace amount
            lov_out = clusterlib.TxOut(target_address, u.amount, datum_hash=datum_hash)
            txouts.append(lov_out)
            if max_utxo is None or u.amount > max_utxo.amount:
                max_utxo = lov_out
        assert max_utxo is not None
        # subtract fees from the largest txout, assuming it will be sufficient to cover the txfees
        txouts.remove(max_utxo)
        txouts.append(
            clusterlib.TxOut(
                max_utxo.address,
                max_utxo.amount - MIN_TRANSFER_AND_FEE_AMOUNT,
                datum_hash=max_utxo.datum_hash,
            )
        )
        _LOGGER.debug("Building transaction")
        tx_built = CL.build_tx(
            src_address=src_address,
            tx_name=uid,
            tx_files=wallet_skeyfile,
            # use only the necessary collaterals
            txins=txins,
            # send to redeemer what was redeemed, send back split the remainder
            txouts=txouts,
            change_address=target_address,
            destination_dir=TX_FILES_DIR,
            # join UTXOs --> will be constrained by the unique datums attached to each utxo group
            join_txouts=True,
        )
        _LOGGER.debug("Signing transaction")
        tx_signed = CL.sign_tx(
            tx_body_file=tx_built.out_file,
            signing_key_files=wallet_skeyfile.signing_key_files,
            tx_name=uid,
            destination_dir=TX_FILES_DIR,
        )
        _LOGGER.debug("Submitting transaction")
        CL.submit_tx_bare(tx_file=tx_signed)
        submit_ext(tx_signed, BLOCKFROST)
    except Exception as e:
        _LOGGER.warning("Unexpected error when building transaction.", exc_info=e)
        raise e
