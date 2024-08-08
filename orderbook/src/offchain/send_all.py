from typing import Optional

import click
import fire
from pycardano import (
    OgmiosChainContext,
    Network,
    Address,
    TransactionBuilder,
    TransactionOutput,
    VerificationKeyHash,
)

from src.onchain.utils import get_signing_info, get_address, ogmios_url, network
from src.onchain.utils.network import context


def main(name: str, beneficiary: str, sender_address: str = None):
    # Get payment address
    payment_address = (
        Address.from_primitive(sender_address)
        if sender_address is not None
        else get_address(name)
    )

    # Get the beneficiary VerificationKeyHash (PubKeyHash)
    beneficiary_address = Address.from_primitive(beneficiary)

    # Build the transaction
    builder = TransactionBuilder(context)
    for utxo in context.utxos(payment_address):
        builder.add_input(utxo)

    # Sign the transaction
    payment_vkey, payment_skey, payment_address = get_signing_info(name)
    signed_tx = builder.build_and_sign(
        signing_keys=[payment_skey],
        change_address=beneficiary_address,
    )

    # Submit the transaction
    context.submit_tx(signed_tx.to_cbor())

    show_tx(signed_tx)


if __name__ == "__main__":
    fire.Fire(main)
