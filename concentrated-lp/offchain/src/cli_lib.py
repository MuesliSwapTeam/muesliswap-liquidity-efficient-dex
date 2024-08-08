from __future__ import annotations

import time

import datetime

from functools import lru_cache

import os

import subprocess  # type: ignore
import blockfrost  # type: ignore

from cardano_python_utils import utxos
from cardano_python_utils.classes import *

# logger setup
import cardano_clusterlib.clusterlib as clusterlib

from cardano_python_utils.pools.muesliv2_lib import (
    ORDERBOOK_CONTRACT_V3,
    ORDERBOOK_CONTRACT_V4,
)
from cardano_python_utils.utxos import (
    ApplyMempoolUTxOBackend,
    KupoUTxOBackend,
    FallbackUTxOBackend,
)
from .util import TxO, NETWORK, KUPO_FALLBACK_URL
from secret import KUPO_URL, MAINNET, MEMPOOL_SERVER
import secret
import logging
from .config import SHORT_SLEEP


_LOGGER = logging.getLogger(__name__)
logging.basicConfig(
    format="%(asctime)s %(levelname)-8s %(message)s", level=logging.DEBUG
)
# Note: this way the secret variables need only be set if the environment does not specify them
if os.environ.get("CARDANO_NODE_STATE_DIR") is None:
    os.environ["CARDANO_NODE_STATE_DIR"] = secret.STATE_DIR
if os.environ.get("CARDANO_NODE_SOCKET_PATH") is None:
    os.environ["CARDANO_NODE_SOCKET_PATH"] = secret.CARDANO_NODE_SOCKET_PATH

CARDANO_CLI_PATH = "cardano-cli"
CARDANO_NETWORK = ["--mainnet"] if MAINNET else ["--testnet-magic", "1"]
try:
    CL_EKG_HOST = secret.EKG_HOST
    CL_EKG_PORT = secret.EKG_PORT
except AttributeError:
    logging.warning("EKG_HOST and EKG_PORT not set, using default values")
    CL_EKG_HOST = "localhost"
    CL_EKG_PORT = 12788

CL = None
while CL is None:
    try:
        CL = clusterlib.ClusterLib(
            state_dir=os.environ.get("CARDANO_NODE_STATE_DIR"), tx_era="babbage"
        )
    except Exception:
        logging.warning("Could not connect to cardano-node, retrying in 5 seconds...")
        time.sleep(SHORT_SLEEP)

if MEMPOOL_SERVER is not None:
    MEMPOOL_CLIENT = utxos.MempoolClient(MEMPOOL_SERVER)
else:
    MEMPOOL_CLIENT = None

BATCH_VALIDITY = datetime.timedelta(days=1)


@lru_cache(maxsize=1)
def ORDERBOOK_CONTRACT_REFUTXO():
    return {
        ORDERBOOK_CONTRACT_V3[NETWORK]: CL.get_utxo(
            txin="fa9691ae4f5fb4135bdd67a9c3deb80e5ec3301b091f66002c8ce56e3ab3d967#1"
            if MAINNET
            else "01ac966386e380dcab8a209ab0ff7859d497a98583ebf035486ee88d4c42da7b#1"
        )[0],
        ORDERBOOK_CONTRACT_V4[NETWORK]: CL.get_utxo(
            txin="7e4142b7a040eae45d14513000adf91ab42da33a1bd5ccffcfe851b3d93e1e5e#1"
            if MAINNET
            else "adbf39367059f14fd00f5b1fd466f9b8101ef4732b1a34d01a0af65278bec337#0"
        )[0],
    }


#################################################################################################
#        Query Transactions and Outputs from script address (mostly for internal use)          #
#################################################################################################

kupo_max_sync_delay = None
combined_utxo_backend = FallbackUTxOBackend(
    KupoUTxOBackend(KUPO_URL, kupo_max_sync_delay),
    KupoUTxOBackend(KUPO_FALLBACK_URL, kupo_max_sync_delay),
)
combined_utxo_backend = (
    ApplyMempoolUTxOBackend(
        MEMPOOL_CLIENT,
        combined_utxo_backend,
    )
    if MEMPOOL_CLIENT is not None
    else combined_utxo_backend
)


def fetch_utxos(*addr: Bech32Addr) -> List[TxO]:
    return sum((combined_utxo_backend.fetch_utxos(a) for a in addr), [])


def valid_license(a: Asset, license_policy_id: PolicyId):
    try:
        return (
            a.token.policy_id == license_policy_id
            # the name of the asset is the unix timestamp where it is still valid
            and datetime.datetime.fromtimestamp(int(a.token.name, 16) / 1000)
            # and datetime.datetime.fromtimestamp(int(bytes.fromhex(a.token.name).decode("utf8"), 10) / 1000)
            > datetime.datetime.now() + BATCH_VALIDITY
        )
    except (ValueError, OverflowError):
        # in case that the value of the batcher license is waaay to large
        return False


def valid_licenses(wallet_utxos: List[TxO], license_policy: PolicyId) -> List[TxO]:
    """
    Query utxos from the wallet containing batcher licenses
    """
    raw_utxos = sorted(wallet_utxos, key=lambda x: x.amount)
    license_utxos = [
        utxo
        for utxo in raw_utxos
        # batcher license needs to be present
        if any(valid_license(a, license_policy) for a in utxo.assets)
    ]
    return license_utxos
