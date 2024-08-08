from __future__ import annotations

from collections import defaultdict
import time

from typing import DefaultDict

import argparse

from watchdog.observers import Observer
from watchdog.events import FileSystemEventHandler

import cardano_python_utils.util
from ..config import SHORT_SLEEP
from .cli_lib import valid_license
from .mm_lib import *
from .querier import querier, pool_querier
from .order_lib import remap
from .querier.querier import open_orders
from .querier.pool_querier import open_pools
from .util import DATA_DIR

_LOGGER = logging.getLogger(__name__)

# this should be ~ once matcher fee from the FE
# but optimally also pay for the contract costs
MIN_MATCHING_GAIN_PER_PARTICIPANT = 460000
MIN_MATCHING_GAIN_INITIAL = 2 * MIN_MATCHING_GAIN_PER_PARTICIPANT
MIN_SWAP_SIZE = 1500000

MAX_GAIN = 1_000_000

STAKING_ADDRESS = Bech32Addr(
    "stake1uxux9ednue7htjfftvyavkvc9v8p2urjz4ktewpqkd723hg8343xg"
)


class ObserveOrderData(FileSystemEventHandler):
    def __init__(
        self,
        cli_args: argparse.Namespace,
        license_policies: list[PolicyId],
        redeemer: Bech32Addr,
        return_threshold: int = 1000_000_000,
        custom_retain_amount: Optional[int] = None,
        custom_retain_tokens: List[Token] = (),
        requires_collateral: bool = True,
    ):
        """
        :param cli_args: command line arguments
        :param license_policies: list of license policies that this batcher needs
        :param redeemer: redeemer address when return threshold lovelace is reached
        :param return_threshold: threshold in lovelace when to return funds
        :param custom_retain_amount: custom amount to retain when returning funds
        :param custom_retain_tokens: custom tokens to retain when returning funds
        :param requires_collateral: whether this batcher requires collateral
        """
        self.wallet_skeyfile = cli_args.wallet_signing_key
        wallet_vkey, wallet_skey, wallet_addr = cardano_python_utils.util.load_wallet(
            self.wallet_skeyfile, network=NETWORK
        )
        if cli_args.print_address:
            print(f"Batcher pure address {wallet_addr}")
        # check content of enterprise address and move to staking address
        # update to using staking address
        stake_key = cli_args.staking_address
        if cli_args.print_address:
            print(f"Batcher uses stake key {stake_key}")
        if stake_key != "-":
            combined_address = cardano_python_utils.util.combine(
                str(wallet_addr), stake_key
            )

            try:
                move_utxos(str(wallet_addr), self.wallet_skeyfile, combined_address)
            except:
                _LOGGER.warning(
                    "Could not move all utxos from address to staked address. Funds may be missing. Consider refilling"
                )
        else:
            combined_address = wallet_addr.to_primitive()
        self.wallet_address = ShelleyAddress.from_bech32(combined_address)
        if cli_args.print_address:
            print(f"Batcher uses address {self.wallet_address.bech32}")
            exit(0)
        self.watch_paths = {
            Path(querier.BUYORDERS_FILENAME).absolute(),
            Path(pool_querier.POOLS_FILENAME).absolute(),
        }
        self.refetch = True
        self.redeemer = redeemer

        self.return_threshold = return_threshold
        self.custom_retain_amount = custom_retain_amount
        self.requires_collateral = requires_collateral
        self.custom_retain_tokens = custom_retain_tokens

        self.utxos = []
        self.collaterals = []
        self.license_policies = license_policies
        self.licenses: DefaultDict[PolicyId, List[TxO]] = defaultdict(list)
        self.pools = open_pools()
        self.orders = remap(*open_orders())

    def trigger_batcher(self):
        raise NotImplementedError("Abstract function")

    def on_moved(self, event):
        _LOGGER.debug(f"Event recognized in {event.src_path}")
        try:
            if Path(event.dest_path).absolute() in self.watch_paths:
                _LOGGER.info(f"{event.dest_path} was updated")
                if (
                    self.refetch
                    or not self.collaterals
                    or any(not l for l in self.licenses.values())
                ):
                    utxos = cli_lib.fetch_utxos(self.wallet_address.bech32)
                    self.utxos = utxos
                    self.refetch = False
                    self.collaterals = cardano_python_utils.util.fetch_collaterals(
                        utxos
                    )
                    if not self.collaterals and self.requires_collateral:
                        _LOGGER.warning(
                            f"No collateral found. Send at least 5 ADA to {self.wallet_address.bech32}"
                        )
                    for license in self.license_policies:
                        licenses = cli_lib.valid_licenses(utxos, license)
                        if not licenses:
                            _LOGGER.warning(
                                f"No license found. Send at least one with PID {license} to {self.wallet_address.bech32}"
                            )
                        self.licenses[license] = licenses

                    # send back money if it is too much (~ > 1000 ADA)
                    if split_and_return_tx(
                        self.redeemer,
                        utxos,
                        self.wallet_address.bech32,
                        self.wallet_skeyfile,
                        lambda x: any(
                            valid_license(x, l) for l in self.license_policies
                        ),
                        self.return_threshold,
                        self.custom_retain_amount,
                        self.custom_retain_tokens,
                    ):
                        self.refetch = True
                        return
                self.pools = open_pools()
                self.orders = remap(*open_orders())
                self.trigger_batcher()
        except Exception as e:
            _LOGGER.error("Unknown exception", exc_info=e)
            pass


def generate_argparser():
    a = argparse.ArgumentParser()
    a.add_argument(
        "--staking_address",
        help="Staking address to be used to stake the matchmaker fundings",
        type=str,
        default=STAKING_ADDRESS,
    )
    a.add_argument(
        "wallet_signing_key",
        help="Secret key file of wallet address to be used to pay fees and claim rewards for matching",
        type=Path,
    )
    a.add_argument(
        "--print-address",
        help="Print the address of the wallet and exit",
        action="store_true",
    )
    return a


def schedule_observer(event):
    observer = Observer()
    observer.schedule(event, path=str(DATA_DIR.absolute()), recursive=False)
    observer.start()
    while True:
        try:
            time.sleep(SHORT_SLEEP)
        except KeyboardInterrupt:
            observer.stop()
            observer.join()
            exit(0)
