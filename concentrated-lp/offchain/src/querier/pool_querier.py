from __future__ import annotations

from cardano_python_utils import clusterfuck
from typing import Iterable, Callable, TypeVar
import pickle

from src.cli_lib import CL
from .. import lp_lib
from ..util import *

# logger setup
_LOGGER = logging.getLogger(__name__)

# define serializer module and file opening type
# strictly speaking, this is a ModuleType, but then the call to dump is not considered safe
serializer = pickle
FILE_OPEN_TYPE = "wb"

# NOTE: this could be much cleaner using metaprogramming (setattr(sys.modules[__name__], var, val)
#       but mypy cannot handle this so we leave it like this (possibly temporarily)
from src.util import DATA_DIR

# filenames to store data required for different queries
POOLS_FILENAME: str = os.path.join(DATA_DIR, "pools")
# suffix for temporary files containing updated data
NEW_EXTENSION: str = ".new"
NEW_POOLS_FILENAME: str = POOLS_FILENAME + NEW_EXTENSION


#################################################################################################
#                                            Helpers                                            #
#################################################################################################


F = TypeVar("F", bound=Callable[..., Any])


#################################################################################################
#                                            Pools                                              #
#################################################################################################


@nofail("failed to write order data to file")
def create_pools(pools: Iterable[lp_lib.Pool]):
    _LOGGER.info("creating buy and sell orders data")
    pools_filename = POOLS_FILENAME
    new_pools_filename = NEW_POOLS_FILENAME
    with open(new_pools_filename, FILE_OPEN_TYPE) as pools_file:
        serializer.dump(
            list(pools),
            pools_file,
        )
    os.rename(new_pools_filename, pools_filename)


#################################################################################################
#                                         Orderbooks                                            #
#################################################################################################

last_tip = None


@nofail("failed to create data")
def create_data():
    """
    Create all the required data for the server using only a single query for open orders.
    """

    global last_tip
    tip = clusterfuck.slot_no(CL)
    if tip == last_tip:
        return

    _LOGGER.info("creating all data")
    start = time.perf_counter()

    # query and sort orders and grup them by their type
    # query and sort orders and grup them by their type
    pools = sum(lp_lib.fetch_pools(), [])
    # create the required data
    create_pools(pools=pools)

    last_tip = tip
    end = time.perf_counter()
    _LOGGER.info(f"finished in {end - start} seconds")


def main():
    while True:
        try:
            create_data()
            time.sleep(2)
        except KeyboardInterrupt:
            exit()
        except Exception:
            time.sleep(20)


def open_pools() -> List[lp_lib.Pool]:
    _LOGGER.debug(f"Querying pools on pool contracts")
    try:
        # read all open orders
        with open(POOLS_FILENAME, "rb") as pools_file:
            pools = pickle.load(pools_file)
        return pools
    except Exception as e:
        _LOGGER.error("failed to read open orders", exc_info=e)
    return []


if __name__ == "__main__":
    main()
