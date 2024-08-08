import datetime
from typing import Optional

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
from src.onchain.utils.from_script_context import from_address
from src.onchain.utils.network import context, show_tx
from src.onchain.utils.to_script_context import to_tx_out_ref

free_minting_contract_script, free_minting_contract_hash, _ = get_contract(
    "free_mint", True
)


def main(
    name: str,
    steal: bool = False,
    take_more_reward: Optional[int] = None,
    steal_tokens: bool = False,
):
    payment_vkey, payment_skey, payment_address = get_signing_info(
        name, network=network
    )
    orderbook_v3_script, _, orderbook_v3_address = get_contract(
        "orderbook", True, context
    )

    # Find an expired order
    expired_order_utxo = None
    expired_order_datum = None
    for utxo in context.utxos(orderbook_v3_address):
        try:
            order_datum = orderbook.Order.from_cbor(utxo.output.datum.cbor)
        except Exception as e:
            continue
        expiry_date = order_datum.params.expiry_date
        if isinstance(expiry_date, orderbook.FinitePOSIXTime):
            if expiry_date.time > datetime.datetime.now().timestamp() * 1000:
                continue
        elif isinstance(expiry_date, orderbook.PosInfPOSIXTime):
            continue
        expired_order_utxo = utxo
        expired_order_datum = order_datum
        break
    if expired_order_utxo is None:
        print("No expired orders found")
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
        if license_expiry < datetime.datetime.now().timestamp() * 1000:
            continue
        valid_license_utxo = utxo
        break
    if valid_license_utxo is None:
        print("No valid licenses found")
        return

    all_inputs_sorted = sorted_utxos(payment_utxos + [expired_order_utxo])
    license_input_index = all_inputs_sorted.index(valid_license_utxo)
    order_input_index = all_inputs_sorted.index(expired_order_utxo)
    order_output_index = 0
    return_expired_redeemer = pycardano.Redeemer(
        orderbook.ReturnExpired(
            license_index=license_input_index,
            input_index=order_input_index,
            output_index=order_output_index,
        )
    )

    owner_address = from_address(expired_order_datum.params.owner_address)

    # Build the transaction
    builder = TransactionBuilder(context)
    builder.auxiliary_data = AuxiliaryData(
        data=AlonzoMetadata(
            metadata=Metadata({674: {"msg": ["MuesliSwap Return Expired"]}})
        )
    )
    for u in payment_utxos:
        builder.add_input(u)
    builder.add_script_input(
        expired_order_utxo,
        orderbook_v3_script,
        None,
        return_expired_redeemer,
    )
    _taken_reward = take_more_reward or expired_order_datum.params.return_reward
    if not steal_tokens:
        _return_value = expired_order_utxo.output.amount - _taken_reward
    else:
        _return_value = expired_order_utxo.output.amount.coin - _taken_reward
    builder.add_output(
        TransactionOutput(
            address=owner_address if not steal else payment_address,
            amount=_return_value,
            datum=to_tx_out_ref(expired_order_utxo.input),
        ),
    )
    # Sign the transaction
    signed_tx = builder.build_and_sign(
        signing_keys=[payment_skey],
        change_address=payment_address,
        auto_ttl_offset=1000,
        auto_validity_start_offset=0,
    )

    # Submit the transaction
    context.submit_tx(signed_tx.to_cbor())

    show_tx(signed_tx)


if __name__ == "__main__":
    fire.Fire(main)
