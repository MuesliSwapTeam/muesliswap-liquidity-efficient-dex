# create reference UTxOs
from time import sleep

import click
import pycardano
from pycardano import TransactionBuilder, TransactionOutput, OgmiosChainContext, Network
from cardano_python_utils.util import load_wallet, load_contract
from secret import OGMIOS_URL, MAINNET, KUPO_URL

network = Network.MAINNET if MAINNET else Network.TESTNET

# Load chain context
try:
    context = OgmiosChainContext(OGMIOS_URL, network=network, kupo_url=KUPO_URL)
except ConnectionError:
    print("Ogmios failed to connect, it is not available")
    context = None


@click.command()
@click.argument(
    "contract",
    type=str,
    # help="Select the plutus contract to submit",
)
def main(contract: str):
    owner = "payment_scripts.skey"
    owner_vkey, owner_skey, owner_addr = load_wallet(owner, network)
    contract_cbor = load_contract(contract)
    script_addr = pycardano.Address(
        payment_part=pycardano.script_hash(script=contract_cbor), network=network
    )
    txbuilder = TransactionBuilder(context)
    txbuilder.add_output(
        TransactionOutput(script_addr, amount=50000000, script=contract_cbor)
    )
    txbuilder.add_input_address(owner_addr)
    tx = txbuilder.build_and_sign(signing_keys=[owner_skey], change_address=owner_addr)
    context.submit_tx(tx.to_cbor())


if __name__ == "__main__":
    main()
