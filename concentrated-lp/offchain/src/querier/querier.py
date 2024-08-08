from __future__ import annotations

from cardano_python_utils import clusterfuck
from typing import Iterable, Callable, TypeVar, cast, Set
import pickle

from src.cli_lib import CL
from src.util import *

from .. import order_lib as ql
from ..order_lib import Order, BuyOrder, SellOrder
from ..config import SHORT_SLEEP, LONG_SLEEP

# logger setup
_LOGGER = logging.getLogger(__name__)

# define serializer module and file opening type
# strictly speaking, this is a ModuleType, but then the call to dump is not considered safe
serializer = pickle
FILE_OPEN_TYPE = "wb"

# NOTE: this could be much cleaner using metaprogramming (setattr(sys.modules[__name__], var, val)
#       but mypy cannot handle this so we leave it like this (possibly temporarily)
# directory in which to store data
from src.util import DATA_DIR

# filenames to store data required for different queries
BUYORDERS_FILENAME: str = os.path.join(DATA_DIR, "buyorders")
SELLORDERS_FILENAME: str = os.path.join(DATA_DIR, "sellorders")
# append extension corresponding to serialization method
# suffix for temporary files containing updated data
NEW_EXTENSION: str = ".new"
NEW_SELLORDERS_FILENAME: str = SELLORDERS_FILENAME + NEW_EXTENSION
NEW_BUYORDERS_FILENAME: str = BUYORDERS_FILENAME + NEW_EXTENSION


#################################################################################################
#                                            Helpers                                            #
#################################################################################################


def group_by_pairs(
    orders: Iterable[Order],
) -> Tuple[
    Set[Token],
    Dict[Tuple[Token, Token], List[BuyOrder]],
    Dict[Tuple[Token, Token], List[BuyOrder]],
]:
    """Group the given orders by the traded token pair and also return a set of all tokens involved."""
    # By convention, the token with shorter policyid+name is the token that
    # defines the base
    tokens: Set[Token] = set()
    token_buy_orders: Dict[Tuple[Token, Token], List[BuyOrder]] = defaultdict(list)
    token_sell_orders: Dict[Tuple[Token, Token], List[SellOrder]] = defaultdict(list)
    for o in orders:
        tokens.add(o.sell.token)
        tokens.add(o.buy.token)
        if o.sell.token < o.buy.token:
            trade_pair = (o.sell.token, o.buy.token)
            token_buy_orders[trade_pair].append(o)
        else:
            trade_pair = (o.buy.token, o.sell.token)
            token_sell_orders[trade_pair].append(o)
    return tokens, token_buy_orders, token_sell_orders


#################################################################################################
#                                            Orders                                             #
#################################################################################################


@nofail("failed to write order data to file")
def create_orders_data(
    sell_orders: Iterable[BuyOrder], buy_orders: Iterable[BuyOrder], v1: bool = False
):
    _LOGGER.info("creating buy and sell orders data")
    buy_filename = SELLORDERS_FILENAME
    new_buy_filename = NEW_BUYORDERS_FILENAME
    sell_filename = BUYORDERS_FILENAME
    new_sell_filename = NEW_SELLORDERS_FILENAME
    with open(new_buy_filename, FILE_OPEN_TYPE) as buy_orders_file:
        serializer.dump(
            list(buy_orders),
            buy_orders_file,
        )
    with open(new_sell_filename, FILE_OPEN_TYPE) as sell_orders_file:
        serializer.dump(
            list(sell_orders),
            sell_orders_file,
        )
    os.rename(new_buy_filename, buy_filename)
    os.rename(new_sell_filename, sell_filename)


#################################################################################################
#                                         Orderbooks                                            #
#################################################################################################

last_tip = None


@nofail("failed to create data")
def create_data():
    """
    Create all the required data for the server using only a single query for open orders.
    """
    _LOGGER.info("creating all data")
    start = time.perf_counter()

    global last_tip
    tip = clusterfuck.slot_no(CL)
    if tip == last_tip:
        return

    # query and sort orders and grup them by their type
    tokens, buy_orders, sell_orders = group_by_pairs(ql.query_orders())
    # create the required data
    create_orders_data(
        sell_orders=sum(sell_orders.values(), start=[]),
        buy_orders=sum(buy_orders.values(), start=[]),
    )

    last_tip = tip
    end = time.perf_counter()
    _LOGGER.info(f"finished in {end - start} seconds")


def main():
    while True:
        try:
            create_data()
            time.sleep(SHORT_SLEEP)
        except KeyboardInterrupt:
            exit()
        except Exception:
            time.sleep(LONG_SLEEP)


def open_orders() -> Tuple[List[Order], List[Order]]:
    _LOGGER.debug(
        f"Querying open orders on {ORDERBOOK_CONTRACT_V2} and {ORDERBOOK_CONTRACT_V3}"
    )
    # read all open orders
    buy, sell = [], []
    try:
        with open(SELLORDERS_FILENAME, "rb") as buy_orders_file:
            buy = pickle.load(buy_orders_file)
    except Exception as e:
        _LOGGER.error("failed to read open orders", exc_info=e)
    try:
        with open(BUYORDERS_FILENAME, "rb") as sell_orders_file:
            sell = pickle.load(sell_orders_file)
    except Exception as e:
        _LOGGER.error("failed to read open orders", exc_info=e)
    return buy, sell


if __name__ == "__main__":
    main()
