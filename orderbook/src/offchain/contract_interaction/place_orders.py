import datetime

import click
import pycardano
from pycardano import (
    TransactionBuilder,
    TransactionOutput,
    Asset,
    AuxiliaryData,
    AlonzoMetadata,
    Metadata,
)

from src.onchain.contracts import orderbook
from src.onchain.utils import get_signing_info, get_address, network
from src.onchain.utils.contracts import get_contract
from src.onchain.utils.network import context, show_tx
from src.onchain.utils.to_script_context import to_address

free_minting_contract_script, free_minting_contract_hash, _ = get_contract(
    "free_mint", True
)


@click.command()
@click.argument("name")
@click.argument("beneficiary")
# Seller (0) or Buyer (1)
@click.argument("role", type=int)
@click.option(
    "--number",
    type=int,
    default=1,
    help="Number of orders placed",
)
@click.option(
    "--sell-amount",
    type=int,
    default=300,
    help="Amount of token to sell",
)
@click.option(
    "--buy-amount",
    type=int,
    default=100,
    help="Amount of token to buy",
)
def main(
    name: str,
    beneficiary: str,
    role: int,
    number: int,
    sell_amount: int,
    buy_amount: int,
    sell_token: str = "muesli",
    buy_token: str = "swap",
):
    _, payment_skey, payment_address = get_signing_info(name, network=network)
    _, _, orderbook_v3_address = get_contract("orderbook", True)

    # Build the transaction
    builder = TransactionBuilder(context)
    builder.add_input_address(payment_address)
    builder.auxiliary_data = AuxiliaryData(
        data=AlonzoMetadata(
            metadata=Metadata({674: {"msg": ["MuesliSwap Place Order"]}})
        )
    )

    for _ in range(number):
        # Get the beneficiary VerificationKeyHash (PubKeyHash)
        if beneficiary == "random":
            beneficiary_pkh = pycardano.PaymentVerificationKey.from_signing_key(
                pycardano.PaymentSigningKey.generate()
            ).hash()
            beneficiary_address = pycardano.Address(
                payment_part=beneficiary_pkh,
                network=network,
            )
        else:
            beneficiary_address = get_address(beneficiary)
            beneficiary_pkh = beneficiary_address.payment_part

        sell_token = (
            free_minting_contract_hash,
            pycardano.AssetName(sell_token.encode()),
        )
        buy_token = (
            free_minting_contract_hash,
            pycardano.AssetName(buy_token.encode()),
        )
        if role:
            sell_token, buy_token = buy_token, sell_token

        # Create the vesting datum
        min_utxo = 2_300_000
        return_reward = 650_000
        params = orderbook.OrderParams(
            beneficiary_pkh.payload,
            to_address(beneficiary_address),
            orderbook.Token(buy_token[0].payload, buy_token[1].payload),
            orderbook.Token(sell_token[0].payload, sell_token[1].payload),
            1,
            orderbook.FinitePOSIXTime(int(datetime.datetime.now().timestamp())),
            return_reward,
            min_utxo,
        )
        # Make datum
        datum = orderbook.Order(
            params,
            buy_amount,
            orderbook.Nothing(),
            return_reward,
        )

        builder.add_output(
            TransactionOutput(
                address=orderbook_v3_address,
                amount=pycardano.Value(
                    coin=min_utxo + return_reward,
                    multi_asset=pycardano.MultiAsset(
                        {sell_token[0]: Asset({sell_token[1]: sell_amount})}
                    ),
                ),
                datum=datum,
            )
        )

    # Sign the transaction
    signed_tx = builder.build_and_sign(
        signing_keys=[payment_skey],
        change_address=payment_address,
    )

    # Submit the transaction
    context.submit_tx(signed_tx.to_cbor())

    show_tx(signed_tx)


if __name__ == "__main__":
    main()
