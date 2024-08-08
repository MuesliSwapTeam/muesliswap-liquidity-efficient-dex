from __future__ import annotations

from typing import Iterable
import blockfrost  # type: ignore
import cardano_clusterlib.clusterlib as clusterlib  # type: ignore
import pony.orm as pony

from cardano_python_utils.pools.muesliv2_lib import (
    parse_order,
    match_datum_content_pv1,
    match_datum_content_pv2,
    ORDERBOOK_CONTRACTS,
)
from .util import *
from cardano_python_utils.minutxo import min_utxo_ada
from .cli_lib import fetch_utxos
from cardano_python_utils.datums import DatumHashError
from . import mm_lib

# logger setup

_LOGGER = logging.getLogger(__name__)
logging.basicConfig(
    format="%(asctime)s %(levelname)-8s %(message)s", level=logging.INFO
)

METADATA_CACHE_SIZE: int = 1000000
TRADER_CACHE_SIZE: int = 1000000


# caching in DB config
DB_NAME = ROOT_DIR.joinpath("metadata.sqlite" if MAINNET else "metadata_testnet.sqlite")
db = pony.Database()


class TTx(db.Entity):
    tx_hash = pony.Required(str, index=True, unique=True)
    metadata = pony.Set("TMetadata")


class TMetadata(db.Entity):
    tx = pony.Required(TTx, index=True)
    label = pony.Required(str)
    json_metadata = pony.Optional(str)


db.bind(provider="sqlite", filename=str(DB_NAME.absolute()), create_db=True)
db.generate_mapping(create_tables=True)
pony.set_sql_debug(False)


@lru_cache(maxsize=1000)
def lru_cached_blockfrost_metadata(tx_hash: str, ttl_hash=None):
    try:
        return BLOCKFROST.transaction_metadata(tx_hash, return_type="json")
    except blockfrost.ApiError as e:
        _LOGGER.error("Error retreiving metadata", exc_info=e)
        return None


def metadata_to_json(md: dict):
    if "int" in md:
        return md["int"]
    if "string" in md:
        return md["string"]
    if "bytes" in md:
        return "0x" + md["bytes"]
    if "list" in md:
        return [metadata_to_json(m) for m in md["list"]]
    if "map" in md:
        return {metadata_to_json(i["k"]): metadata_to_json(i["v"]) for i in md["map"]}


@lru_cache(maxsize=1000)
def kupo_metadata(tx_hash: str, tx_index: int, ttl_hash=None):
    try:
        res = requests.get(f"{KUPO_URL}/matches/{tx_index}@{tx_hash}").json()
        tx_slot = res[0]["created_at"]["slot_no"]
        header_hash = res[0]["created_at"]["header_hash"]
        metadata_req = requests.get(
            f"{KUPO_URL}/metadata/{tx_slot}?transaction_id={tx_hash}"
        )
        metadata = metadata_req.json()
        metadata_header_hash = metadata_req.headers["X-Block-Header-Hash"]
        assert (
            metadata_header_hash == header_hash
        ), "Block header hash changed while fetching slot"
        schema = metadata[0]["schema"]
        schema_dict = [
            {"label": k, "json_metadata": metadata_to_json(v)}
            for k, v in schema.items()
        ]
        return schema_dict
    except Exception as e:
        _LOGGER.error(f"Can not fetch metadata of {tx_hash} via Kupo", exc_info=e)
        return None


@lru_cache(maxsize=METADATA_CACHE_SIZE)
@pony.db_session
def query_transaction_metadata(tx_hash: TxHash, tx_index: TxIndex) -> Dict[int, str]:
    """Simple caching wrapper to get transaction metadata from Blockfrost."""

    _LOGGER.info(f"obtaining transaction metadata for {tx_hash}")
    # additionally cache in a small sqlite DB
    tx = pony.get(tx for tx in TTx if tx.tx_hash == tx_hash)
    if tx is None:
        _LOGGER.info(f"querying transaction metadata for {tx_hash} via blockfrost")
        metadata_responses = kupo_metadata(tx_hash, tx_index, ttl_hash(120))
        if metadata_responses is None:
            metadata_responses = lru_cached_blockfrost_metadata(tx_hash, ttl_hash(120))
        assert metadata_responses is not None, f"Error obtaining metadata for {tx_hash}"
        tx = TTx(tx_hash=tx_hash)
        for resp in metadata_responses:
            TMetadata(
                tx=tx,
                label=str(resp["label"]),
                json_metadata=str(resp["json_metadata"]),
            )
        pony.commit()
    metadata = pony.select(md for md in TMetadata if md.tx == tx)

    return {int(response.label): response.json_metadata for response in metadata}


@dataclass(eq=False)
class Order:
    """base class representing a limit order.

    Fields:
    wallet       -- wallet address of the trader that submitted the order
    tx           -- the transaction representing the order on the blockchain
    buy          -- asset containing type and amount of tokens to buy
    sell         -- asset containing type and amount of tokens to sell
    attached     -- list of other things attached to transaction belonging to owner wallet
    fee          -- amount lovelace dedicated to matchmaker fee
    partial      -- can this order be matched partially
    """

    wallet: ShelleyAddress
    tx: Tx
    buy: Asset
    sell: Asset
    attached: Dict[Token, int]
    fee: int
    partial: int

    def __eq__(self, other):
        if not isinstance(other, Order):
            return False
        return other.tx == self.tx

    def __hash__(self):
        return hash(self.tx)


@dataclass(frozen=True)
class OrderParams:
    wallet: ShelleyAddress
    buy_token: Token
    sell_token: Token
    buy_amount: int
    allow_partial: bool
    lovelace_attached: int


class Tx:
    """Represents a transaction that in turn might or might not represent a limit order.
    Note that this only contains outputs belonging to the script.

    Fields:
    tx_hash  -- the transaction's hash
    utxos    -- list of the transaction's outputs
    metadata -- transaction's metadata parsed into a dictionary
    datum_content -- transaction's datum
    """

    tx_hash: TxHash
    outputs: List[TxO]
    order_params: Optional[OrderParams]
    datum_content: Optional[DatumContent]

    def __init__(self, tx_hash: TxHash, outputs: List[TxO]):
        self.tx_hash = tx_hash
        self.outputs = outputs

    def __eq__(self, other):
        if not isinstance(other, Tx):
            return False
        return self.tx_hash == other.tx_hash

    def __hash__(self):
        return hash(self.tx_hash)

    def _query_metadata(self):
        try:
            self.metadata = query_transaction_metadata(
                self.tx_hash, self.outputs[0].index
            )
        except Exception as e:
            _LOGGER.warning(
                f"failed to query transaction metadata for {self.tx_hash} from blockfrost",
                exc_info=e,
            )
            raise ValueError(
                f"failed to query transaction metadata for {self.tx_hash} from blockfrost, "
                "refusing to construct transaction object"
            )

    def is_order(self) -> bool:
        """Determine whether this transaction is (and can be converted to) a limit order."""
        # an order transaction must have exactly one output going to the script
        if len(self.outputs) != 1:
            return False
        if self.outputs[0].inline_datum:
            return False
        # and this output must then have either no (buy order) or one asset (sell order) or two assets (paritally matched order-order)
        if len(self.outputs[0].assets) not in (0, 1, 2):
            return False
        if self.outputs[0].amount < MIN_ADA_TRANSFER:
            _LOGGER.info(
                f"transaction {self.tx_hash} looks like buy order, "
                f"but sends {self.outputs[0].amount}, which is not more "
                f"than the minimum transfer {MIN_ADA_TRANSFER}"
            )
            return False
        # try to fetch the datum directly from the chain
        self.order_params = None
        # except for OB v2 where the lvl attached info is missing
        if self.outputs[0].owner != ORDERBOOK_CONTRACT_V2[NETWORK]:
            try:
                datum = datums_be.fetch_datum(self.outputs[0].datum_hash)
                assert datum is not None, "Returned datum is None for some reason"
                self.datum_content = DatumContent(json.dumps(datum))
                self.order_params = OrderParams(*parse_order(datum, MAINNET))
            except Exception as e:
                # Datum could not be obtained
                _LOGGER.debug("Could not obtain datum", exc_info=e)
        if self.order_params is None:
            # fetch order parameters from metadata
            self._query_metadata()
            # we need to verify that the metadata has the correct form
            if not self._metadata_matches():
                return False
            wallet = ShelleyAddress.from_hex(self.metadata[METADATA_ADDRESS_KEY])
            buy_policy_id_bytes_hex = self.metadata[METADATA_BUY_POLICYID_KEY]
            buy_name_bytes_hex = self.metadata[METADATA_BUY_TOKENNAME_KEY]
            sell_policy_id_bytes_hex = self.metadata[METADATA_SELL_POLICYID_KEY]
            sell_name_bytes_hex = self.metadata[METADATA_SELL_TOKENNAME_KEY]
            amount = int(self.metadata[METADATA_AMOUNT_KEY])
            fee = int(self.metadata[METADATA_LOVELACE_ATTACHED_KEY])
            try:
                allow_partial_vals = [int(self.metadata[METADATA_ALLOW_PARTIAL_KEY])]
            except (KeyError, ValueError):
                _LOGGER.info("Allow partial not given, trying alternatives")
                allow_partial_vals = [0, 1]

            datum_hash_matches = False
            for allow_partial in allow_partial_vals:
                # the value will persist after the loop
                if self.outputs[0].owner == ORDERBOOK_CONTRACT_V2[NETWORK]:
                    self.datum_content = match_datum_content_pv1(
                        wallet,
                        Token(buy_policy_id_bytes_hex, buy_name_bytes_hex),
                        Token(sell_policy_id_bytes_hex, sell_name_bytes_hex),
                        amount,
                        allow_partial,
                    )
                elif (
                    self.outputs[0].owner == ORDERBOOK_CONTRACT_V3[NETWORK]
                    or self.outputs[0].owner == ORDERBOOK_CONTRACT_V4[NETWORK]
                ):
                    self.datum_content = match_datum_content_pv2(
                        wallet,
                        Token(buy_policy_id_bytes_hex, buy_name_bytes_hex),
                        Token(sell_policy_id_bytes_hex, sell_name_bytes_hex),
                        amount,
                        allow_partial,
                        fee,
                    )
                else:
                    raise NotImplementedError(
                        f"Contract version {self.outputs[0].owner} not implemented"
                    )
                # and we need to verify that the metadata has the correct content
                if self._datum_hash_matches():
                    datum_hash_matches = True
                    break
            if not datum_hash_matches:
                return False
            # write the datum into the script datum endpoint -> is returned by "script_datum" next time, avail for matcher
            datums._store_script_datum_content(
                self.outputs[0].datum_hash, self.datum_content
            )

            self.order_params = OrderParams(
                wallet,
                Token(buy_policy_id_bytes_hex, buy_name_bytes_hex),
                Token(sell_policy_id_bytes_hex, sell_name_bytes_hex),
                amount,
                allow_partial == 1,
                fee,
            )
        if self.order_params.buy_amount < 0 and self.order_params.buy_token != LOVELACE:
            return False
        return True

    def _datum_hash_matches(self):
        # check that datum hash matches
        _LOGGER.debug(f"Checking datum hash of {self.tx_hash}")
        try:
            hash = datums.datum_content_hash(self.datum_content)
        except DatumHashError as e:
            _LOGGER.warning(f"Failed to compute hash of {self.tx_hash}: {e}")
            return False

        return hash == self.outputs[0].datum_hash

    def _metadata_matches(self) -> bool:
        """Determine whether the transaction's metadata has the correct format for an order.
        that is, it must have the required fields (and no more)
        and these must additionally match the order type (i.e. buy/sell).
        """
        # Cardano's general metadata fromat is a (compact) json (i.e. a dict in this code) where
        # * top-level keys are integers
        # * the key of each value is its type, which must be one of the following:
        #     - int
        #     - string
        #     - bytes
        #     - list (not used by us)
        #     - map (not used by us)
        _LOGGER.debug(f"verifying metadata format for transaction {self.tx_hash}")
        _LOGGER.debug(str(self.metadata))
        required_key_types: List[Tuple[int, type, str]] = [
            (METADATA_ADDRESS_KEY, str, "trader wallet"),
            (METADATA_BUY_POLICYID_KEY, str, "buy-policy-id"),
            (METADATA_BUY_TOKENNAME_KEY, str, "buy-tokenname"),
            (METADATA_AMOUNT_KEY, int, "amount"),
            # (METADATA_ALLOW_PARTIAL_KEY, int, "allow partial"),
            (METADATA_SELL_POLICYID_KEY, str, "sell-policy-id"),
            (METADATA_SELL_TOKENNAME_KEY, str, "sell-tokenname"),
        ]
        for key, typ, name in required_key_types:
            if key not in self.metadata:
                _LOGGER.info(
                    f"transaction {self.tx_hash} is not an order, missing key {key}"
                )
                return False
            if typ == int:
                try:
                    int(self.metadata[key])
                except (ValueError, TypeError):
                    _LOGGER.info(
                        f"transaction {self.tx_hash} is not an order, {key}'s value "
                        f"'{self.metadata[key]}' could not be converted to int"
                    )
                    return False
            elif typ != str:
                _LOGGER.error(
                    f"transaction {self.tx_hash} is not an order, "
                    f"no rule to match against metadata of type {typ.__name__}"
                )
                return False
        return True

    def to_order(self) -> Order:
        """Construct the limit order this transaction represents (ValueError if it does not)."""
        if not self.is_order():
            raise ValueError(
                "trying to convert transaction that does not represent a limit order"
            )
        # now we can assume the following:
        # * the transaction has exactly one output
        # * this output has either 0 (buy) or 1 (sell) output(s)
        # * the metadata contains the correct fields in the correct format

        # compute how much ada will be required to be attached to this order on return
        # which takes into account both involved assets and the datum hash
        min_ada = max(
            MIN_ADA_TRANSFER,
            min_utxo_ada(
                [self.order_params.buy_token, self.order_params.sell_token], True
            ),
        )

        buy_asset = Asset(self.order_params.buy_amount, self.order_params.buy_token)

        sell_asset = None
        attached = defaultdict(int)
        # empty sell policy id => sells ADA
        if not self.order_params.sell_token.policy_id:
            # in that case it sells all ada attached minus minimum transfer
            _sell_amount = self.outputs[0].amount - min_ada
            sell_asset = Asset(_sell_amount, LOVELACE)
            attached[LOVELACE] += min_ada
        else:
            attached[LOVELACE] += self.outputs[0].amount
        for asset in self.outputs[0].assets:
            if asset.token == self.order_params.sell_token:
                sell_asset = asset
            else:
                attached[asset.token] += asset.amount
        assert buy_asset is not None
        assert sell_asset is not None

        # do not allow fees of more than 1.5 ADA (otherwise this could skew the price)
        fee = min(
            max(self.order_params.lovelace_attached - min_ada, 0),
            min_ada,
        )

        order = Order(
            wallet=self.order_params.wallet,
            tx=self,
            buy=buy_asset,
            sell=sell_asset,
            attached=attached,
            partial=self.order_params.allow_partial,
            fee=fee,
        )
        return order


def query_txs(addr: Bech32Addr) -> List[Tx]:
    """Return all tx-hashes with utxos belonging to the given wallet address using cardano-cli."""
    tx_dict: Dict[TxHash, List[TxO]] = {}
    for utxo in fetch_utxos(addr):
        tx_hash: TxHash = TxHash(utxo.tx_hash)
        if tx_hash in tx_dict:
            tx_dict[tx_hash].append(utxo)
        else:
            tx_dict[tx_hash] = [utxo]
    txs: List[Tx] = []
    for tx_hash, outputs in tx_dict.items():
        try:
            txs.append(Tx(tx_hash, outputs))
        except:
            _LOGGER.warning(f"could not construct transaction object for {tx_hash}")
    return txs


#################################################################################################
#                                Query Orders from script address                               #
#################################################################################################


# cache open orders
@lru_cache(maxsize=100000)
def cached_to_order(utxo: TxO):
    # This will not handle exceptions, so that no-orders are not cached
    # the reason is that orders might not be orders just because their metadata is still missing from BF
    tx = Tx(tx_hash=utxo.tx_hash, outputs=[utxo])
    return tx.to_order()


def sloppy_to_order(utxo: TxO):
    # very lazily (but so far correctly) assume that every tx = one order
    # try to convert, only doing is_order check once
    # it is far more likely that the tx is an order
    # so exceptions should be faster than if/else
    try:
        return cached_to_order(utxo)
    except (ValueError, AssertionError):
        # except, as no-order is far less likely
        pass
    except Exception as e:
        _LOGGER.error(f"Unexpected error when parsing {utxo}", exc_info=e)
    return None


def query_orders() -> Iterable[Order]:
    """
    Query all currently open limit orders sorted and filtered according to the given arguments.
    """
    # obtain list of all orders
    orders = []
    for script_addr in ORDERBOOK_CONTRACTS[NETWORK]:
        try:
            orders.extend(filter(None, map(sloppy_to_order, fetch_utxos(script_addr))))
        except Exception as e:
            _LOGGER.error("exception occured upon querying limit orders", exc_info=e)
            raise e
    return orders


# Buy and Sell orders are just naming conventions for orders that sell/buy a specific token
BuyOrder = NewType("BuyOrder", Order)
SellOrder = NewType("SellOrder", Order)


@lru_cache(maxsize=1000000)
def massage_buy(bo: BuyOrder) -> mm_lib.Order:
    # send to enterprise address if wallet in metadata broken
    o = mm_lib.Order(
        creator=bo.wallet,
        buy=bo.buy,
        sell=bo.sell,
        arrival=datetime.datetime.now(),
        utxo=bo.tx.outputs[0],
        datum_hash=bo.tx.outputs[0].datum_hash,
        datum_content=bo.tx.datum_content,
        attached=bo.attached,
        fee=bo.fee,
        partial=bo.partial,
    )
    return o


@lru_cache(maxsize=1000000)
def massage_sell(bo: SellOrder) -> mm_lib.Order:
    # send to enterprise address if wallet in metadata broken
    o = mm_lib.Order(
        creator=bo.wallet,
        buy=bo.buy,
        sell=bo.sell,
        arrival=datetime.datetime.now(),
        utxo=bo.tx.outputs[0],
        datum_hash=bo.tx.outputs[0].datum_hash,
        datum_content=bo.tx.datum_content,
        attached=bo.attached,
        fee=bo.fee,
        partial=bo.partial,
    )
    return o


def collect(
    buy: list[BuyOrder],
    sell: list[SellOrder],
    buy_token: Token,
    sell_token: Token,
):
    # fix on arbitrary side as "buy"
    # this partitions the set
    # further seperate into groups of orders of the same amount
    _LOGGER.debug(f"Separating orders by bought amount")
    # apply required filters
    buy_d = []
    for o in map(massage_buy, buy):
        if o.buy.token == buy_token and o.sell.token == sell_token:
            buy_d.append(o)
    sell_d = []
    for o in map(massage_sell, sell):
        if o.buy.token == sell_token and o.sell.token == buy_token:
            sell_d.append(o)
    return buy_d, sell_d


def collect_mapped(
    buy: list[Order],
    sell: list[Order],
    buy_token: Token,
    sell_token: Token,
):
    # fix on arbitrary side as "buy"
    # this partitions the set
    # further seperate into groups of orders of the same amount
    _LOGGER.debug(f"Separating orders by bought amount")
    # apply required filters
    buy_d = []
    for o in buy:
        if o.buy.token == buy_token and o.sell.token == sell_token:
            buy_d.append(o)
    sell_d = []
    for o in sell:
        if o.buy.token == sell_token and o.sell.token == buy_token:
            sell_d.append(o)
    return buy_d, sell_d


def remap(
    buy: List[BuyOrder], sell: List[SellOrder]
) -> Tuple[List[mm_lib.Order], List[mm_lib.Order]]:
    return list(map(massage_buy, buy)), list(map(massage_sell, sell))


def collect_pairs(
    buy: Iterable[Order],
    sell: Iterable[Order],
    exclude: set[tuple[Token, Token]],
) -> Tuple[
    Dict[Token, Dict[Token, List[Order]]], Dict[Token, Dict[Token, List[Order]]]
]:
    # fix on arbitrary side as "buy"
    # this partitions the set
    # further seperate into groups of orders of the same amount
    _LOGGER.debug(
        f"Separating orders by bought amount, collecting different token pairs"
    )
    # apply required filters
    buy_dd = defaultdict(lambda: defaultdict(list))
    for o in buy:
        if (o.buy.token, o.sell.token) not in exclude:
            buy_dd[o.buy.token][o.sell.token].append(o)
    sell_dd = defaultdict(lambda: defaultdict(list))
    for o in sell:
        if (o.buy.token, o.sell.token) not in exclude:
            sell_dd[o.sell.token][o.buy.token].append(o)
    return buy_dd, sell_dd


if __name__ == "__main__":
    order = kupo_metadata(
        "fe10b6e80713cff9317d28f431c9120793a4119c28fd063f9e1b7d0be0a850c1", 0
    )
    print(order)
    order = lru_cached_blockfrost_metadata(
        "fe10b6e80713cff9317d28f431c9120793a4119c28fd063f9e1b7d0be0a850c1", 0
    )
    print(order)
    # print(f"matches {order._datum_hash_matches}")
