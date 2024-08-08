import datetime
import math
from fractions import Fraction
from typing import Optional

import click
import fire
import pycardano
from pycardano import (
    TransactionBuilder,
    TransactionOutput,
    AuxiliaryData,
    AlonzoMetadata,
    Metadata,
)

from src.offchain.util import sorted_utxos
from src.onchain.contracts import orderbook
from src.onchain.utils import get_signing_info, network
from src.onchain.utils.contracts import get_contract
from src.onchain.utils.network import context, show_tx
from src.onchain.utils.to_script_context import to_tx_out_ref


def main(
    name: str,
    max_amount: int = 50,
    fraction: Optional[float] = Fraction(3, 20),
    take_more_reward: Optional[int] = None,
):
    payment_vkey, payment_skey, payment_address = get_signing_info(
        name, network=network
    )
    orderbook_v3_script, _, orderbook_v3_address = get_contract(
        "orderbook", True, context
    )
    free_minting_contract_script, free_minting_contract_hash, _ = get_contract(
        "free_mint", True, context
    )
    (license_check_script, _, license_check_address) = get_contract(
        "license_check", True, context
    )

    # Find an order wanting to buy free_mint tokens
    found_orders = []
    for utxo in context.utxos(orderbook_v3_address):
        try:
            order_datum = orderbook.Order.from_cbor(utxo.output.datum.cbor)
        except Exception as e:
            continue
        if order_datum.params.buy.policy_id != free_minting_contract_hash.payload:
            continue
        found_orders.append((utxo, order_datum))
    if not found_orders:
        print("No orders found")
        return

    # Find a valid license
    valid_license_utxo = None
    payment_utxos = context.utxos(payment_address)
    for utxo in payment_utxos:
        if utxo.output.amount.multi_asset.get(free_minting_contract_hash) is None:
            continue
        license_name = list(
            utxo.output.amount.multi_asset[free_minting_contract_hash].keys()
        )[0]
        license_expiry = int.from_bytes(license_name.payload, "big")
        if license_expiry < datetime.datetime.now().timestamp():
            continue
        valid_license_utxo = utxo
        break
    if valid_license_utxo is None:
        print("No valid licenses found")
        return
    ratio = Fraction(fraction)

    for amount_filled in range(min(len(found_orders), max_amount), 0, -1):
        found_orders_filtered = found_orders[:amount_filled]

        all_inputs_sorted = sorted_utxos(
            payment_utxos + [u[0] for u in found_orders_filtered]
        )
        license_input_index = all_inputs_sorted.index(valid_license_utxo)

        # Build the transaction
        builder = TransactionBuilder(context)
        builder.auxiliary_data = AuxiliaryData(
            data=AlonzoMetadata(
                metadata=Metadata({674: {"msg": ["MuesliSwap Fill Order Partial "]}})
            )
        )
        for u in payment_utxos:
            builder.add_input(u)
        builder.mint = pycardano.MultiAsset()
        # add withdrawal which checks the license presence
        builder.add_withdrawal_script(
            license_check_script,
            pycardano.Redeemer(license_input_index),
        )
        builder.withdrawals = pycardano.Withdrawals(
            {
                bytes(
                    pycardano.Address(
                        staking_part=license_check_address.payment_part, network=network
                    )
                ): 0
            }
        )

        for i, (order_utxo, order_datum) in enumerate(found_orders_filtered):
            order_input_index = all_inputs_sorted.index(order_utxo)
            order_output_index = i

            new_buy_amount = int(math.ceil(order_datum.buy_amount * (1 - ratio)))
            adjusted_ratio = 1 - Fraction(new_buy_amount, order_datum.buy_amount)

            sell_token = order_datum.params.sell
            sell_token = (
                pycardano.ScriptHash(sell_token.policy_id),
                pycardano.AssetName(sell_token.token_name),
            )
            sell_amount = int(
                math.floor(
                    order_utxo.output.amount.multi_asset.get(sell_token[0]).get(
                        sell_token[1]
                    )
                    * adjusted_ratio
                )
            )
            sell_asset = pycardano.Value(
                multi_asset=pycardano.MultiAsset(
                    {sell_token[0]: pycardano.Asset({sell_token[1]: sell_amount})}
                )
            )
            buy_token = order_datum.params.buy
            buy_token = (
                pycardano.ScriptHash(buy_token.policy_id),
                pycardano.AssetName(buy_token.token_name),
            )
            buy_amount = order_datum.buy_amount - new_buy_amount
            buy_asset = pycardano.Value(
                multi_asset=pycardano.MultiAsset(
                    {buy_token[0]: pycardano.Asset({buy_token[1]: buy_amount})}
                )
            )

            _taken_reward = take_more_reward or int(
                math.floor(order_datum.batch_reward * adjusted_ratio)
            )
            _return_value = (
                order_utxo.output.amount - _taken_reward - sell_asset + buy_asset
            )

            fill_order_redeemer = pycardano.Redeemer(
                orderbook.PartialMatch(
                    input_index=order_input_index,
                    output_index=order_output_index,
                    filled_amount=buy_amount,
                )
            )

            builder.add_script_input(
                order_utxo,
                orderbook_v3_script,
                None,
                fill_order_redeemer,
            )
            out_datum = orderbook.Order(
                order_datum.params,
                new_buy_amount,
                to_tx_out_ref(order_utxo.input),
                order_datum.batch_reward - _taken_reward,
            )
            builder.add_output(
                TransactionOutput(
                    address=order_utxo.output.address,
                    amount=_return_value,
                    datum=out_datum,
                ),
            )
            builder.mint += buy_asset.multi_asset
        if builder.mint:
            builder.add_minting_script(
                free_minting_contract_script, pycardano.Redeemer(0)
            )
        else:
            builder.mint = None

        # Sign the transaction
        try:
            signed_tx = builder.build_and_sign(
                signing_keys=[payment_skey],
                change_address=payment_address,
                auto_ttl_offset=1000,
                auto_validity_start_offset=0,
            )

            # Submit the transaction
            context.submit_tx(signed_tx.to_cbor())
            show_tx(signed_tx)
            print(f"filled {amount_filled} orders")
            break
        except Exception as e:
            if amount_filled == 1:
                raise e
            print(f"{amount_filled} failed, trying less ({e})")
            continue


if __name__ == "__main__":
    fire.Fire(main)
