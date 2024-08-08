import json
from pathlib import Path
from typing import Optional

import cbor2
from pycardano import (
    Address,
    PlutusV2Script,
    plutus_script_hash,
    ChainContext,
    PlutusV1Script,
)

from src.onchain.utils import network

build_dir = Path(__file__).parent.parent.parent.parent.joinpath("build")


def module_name(module):
    return Path(module.__file__).stem


def get_contract(name, compressed=False, context: ChainContext = None):
    with open(
        build_dir.joinpath(f"{name}{'_compressed' if compressed else ''}/script.cbor")
    ) as f:
        contract_cbor_hex = f.read().strip()
    contract_cbor = bytes.fromhex(contract_cbor_hex)

    contract_plutus_script = PlutusV2Script(contract_cbor)
    contract_script_hash = plutus_script_hash(contract_plutus_script)
    contract_script_address = Address(contract_script_hash, network=network)
    if context is not None:
        ref_utxo = get_ref_utxo(contract_plutus_script, context)
        if ref_utxo is not None:
            contract_plutus_script = ref_utxo
    return contract_plutus_script, contract_script_hash, contract_script_address


def get_pluto_contract(name):
    with open(build_dir.joinpath(f"{name}.plutus")) as f:
        contract_plutus = json.load(f)
    contract_cbor = cbor2.loads(bytes.fromhex(contract_plutus["cborHex"]))

    contract_plutus_script = PlutusV1Script(contract_cbor)
    contract_script_hash = plutus_script_hash(contract_plutus_script)
    contract_script_address = Address(contract_script_hash, network=network)
    return contract_plutus_script, contract_script_hash, contract_script_address


def get_ref_utxo(
    contract: PlutusV2Script,
    context: ChainContext,
    custom_script_address: Optional[Address] = None,
):
    script_address = custom_script_address or Address(
        plutus_script_hash(contract), network=network
    )
    for utxo in context.utxos(script_address):
        if utxo.output.script == contract:
            return utxo
    return None
