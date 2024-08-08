import datetime

import fire
import pycardano

from src.onchain.utils.network import show_tx, context
from src.onchain.utils.to_script_context import to_address
from opshin.prelude import Token
from pycardano import (
    OgmiosChainContext,
    TransactionBuilder,
    Redeemer,
    AuxiliaryData,
    AlonzoMetadata,
    Metadata,
    TransactionOutput,
    Value,
)

from src.offchain.util import (
    token_from_string,
    asset_from_token,
    with_min_lovelace,
    sorted_utxos,
    amount_of_token_in_value,
    combine_with_stake_key,
)
from src.onchain.utils import get_signing_info, ogmios_url, network, kupo_url
from src.onchain.utils.contracts import get_contract


def main(
    wallet: str = "matchmaker",
):
    # Get payment address
    _, payment_skey, payment_address = get_signing_info(wallet, network=network)
    _, _, orderbook_address = get_contract("license_check", True, context)
    orderbook_staking_address = pycardano.Address(
        staking_part=orderbook_address.payment_part, network=network
    )

    orderbook_registration_cert = pycardano.StakeRegistration(
        pycardano.StakeCredential(orderbook_staking_address.staking_part)
    )

    # Build the transaction
    builder = TransactionBuilder(context)
    builder.add_input_address(payment_address)
    builder.certificates = [orderbook_registration_cert]

    # Sign the transaction
    signed_tx = builder.build_and_sign(
        signing_keys=[payment_skey],
        change_address=payment_address,
    )

    # Submit the transaction
    context.submit_tx(signed_tx)

    show_tx(signed_tx)


if __name__ == "__main__":
    fire.Fire(main)
