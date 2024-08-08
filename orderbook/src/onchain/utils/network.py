import os

import ogmios
from pycardano import Network, OgmiosChainContext, Transaction

ogmios_host = os.getenv("OGMIOS_API_HOST", "localhost")
ogmios_port = os.getenv("OGMIOS_API_PORT", "1338")
ogmios_protocol = os.getenv("OGMIOS_API_PROTOCOL", "ws")
ogmios_url = f"{ogmios_protocol}://{ogmios_host}:{ogmios_port}"

kupo_host = os.getenv("KUPO_API_HOST", None)
kupo_port = os.getenv("KUPO_API_PORT", "6669")
kupo_protocol = os.getenv("KUPO_API_PROTOCOL", "http")
kupo_url = f"{kupo_protocol}://{kupo_host}:{kupo_port}" if kupo_host else None

network = Network.MAINNET if os.getenv("NETWORK") == "MAINNET" else Network.TESTNET

# Load chain context
try:
    context = OgmiosChainContext(ogmios_url, network=network, kupo_url=kupo_url)
except Exception:
    try:
        context = ogmios.OgmiosChainContext(
            host=ogmios_host,
            port=int(ogmios_port),
            secure=ogmios_protocol == "wss",
        )
    except Exception as e:
        print("No ogmios available")
        context = None


def show_tx(tx: Transaction):
    print(f"transaction id: {tx.id}")
    print(
        f"Cardanoscan: https://{'preprod.' if network == Network.TESTNET else ''}cexplorer.io/tx/{tx.id}"
    )
