from pathlib import Path

from typing import TypeVar, Callable, cast, Union, Literal

from cardano_python_utils import datums, kupo

from cardano_python_utils.pools.muesliv2_lib import (
    ORDERBOOK_CONTRACT_V2,
    ORDERBOOK_CONTRACT_V3,
    ORDERBOOK_CONTRACT_V4,
)
from secret import *
from cardano_python_utils.util import *

_LOGGER = logging.getLogger(__name__)

NETWORK = MAINNET_NETWORK if MAINNET else TESTNET_NETWORK

KUPO_FALLBACK_URL = "http://localhost:6666"

ROOT_DIR = Path(__file__).parent.parent
DATA_DIR: Path = ROOT_DIR.joinpath("data")
DATA_DIR.mkdir(exist_ok=True, parents=True)
KEYS_DIR: Path = ROOT_DIR.joinpath("keys")
CARDANO_DIR: Path = ROOT_DIR.joinpath("cardano")
METADATA_DIR: Path = ROOT_DIR.joinpath("metadata")
for dir in (DATA_DIR, KEYS_DIR, CARDANO_DIR, METADATA_DIR):
    dir.mkdir(exist_ok=True, parents=True)

StakeKeyType = Union[Bech32Addr, Literal["-"]]

ORDERBOOK_CONTRACT_SCRIPT_FILE = {
    ORDERBOOK_CONTRACT_V2[NETWORK]: CARDANO_DIR.joinpath("orderbook_v2.plutus"),
    ORDERBOOK_CONTRACT_V3[NETWORK]: CARDANO_DIR.joinpath("orderbook_v3.plutus"),
    ORDERBOOK_CONTRACT_V4[NETWORK]: CARDANO_DIR.joinpath("orderbook_v4.plutus"),
}

# read minimum ada transfer
MIN_ADA_TRANSFER = int(1700000)
MIN_COLLATERAL_AMOUNT = 5000000
MAX_COLLATERAL_AMOUNT = 1000000000
MATCHMAKER_FEE = 950000

# blockfrost api object
BLOCKFROST = blockfrost.BlockFrostApi(
    BLOCKFROST_PROJECT_ID,
    base_url="https://cardano-mainnet.blockfrost.io/api"
    if MAINNET
    else "https://cardano-preprod.blockfrost.io/api",
)

# metadata value keys
METADATA_ADDRESS_KEY: int = 1000
METADATA_BUY_POLICYID_KEY: int = 1002
METADATA_BUY_TOKENNAME_KEY: int = 1003
METADATA_AMOUNT_KEY: int = 1004
METADATA_LOVELACE_ATTACHED_KEY: int = 1005
METADATA_ALLOW_PARTIAL_KEY: int = 1007
METADATA_SELL_POLICYID_KEY: int = 1008
METADATA_SELL_TOKENNAME_KEY: int = 1009

datums_be = datums.CombinedDatumBackend(
    kupo.KupoDatumBackend(KUPO_URL),
    kupo.KupoDatumBackend(KUPO_FALLBACK_URL),
    datums.BlockFrostDatumBackend(BLOCKFROST),
)

F = TypeVar("F", bound=Callable[..., Any])


def nofail(msg: str):
    """
    Create a decorator which catches and logs all exceptions the wrapped function might throw.
    """

    def decorator(func: F) -> F:
        def wrapper(*args, **kwargs):
            try:
                func(*args, **kwargs)
            except Exception as e:
                _LOGGER.error(msg, exc_info=e)

        return cast(F, wrapper)

    return decorator
