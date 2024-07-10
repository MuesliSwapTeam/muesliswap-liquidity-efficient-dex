import json
import subprocess
import sys
from pathlib import Path
from typing import Union

from opshin.ledger.api_v2 import StakingHash, ScriptCredential

from src.contracts import orderbook, license_check, free_mint
from src.utils.contracts import get_contract


def build_compressed(
    type: str, script: Union[Path, str], cli_options=("--cf",), args=()
):
    script = Path(script)
    command = [
        sys.executable,
        "-m",
        "opshin",
        *cli_options,
        "build",
        type,
        script,
        *args,
        "--recursion-limit",
        "2000",
    ]
    subprocess.run(command, check=True)

    built_contract = Path(f"build/{script.stem}/script.cbor")
    built_contract_compressed_cbor = Path(f"build/tmp.cbor")

    with built_contract_compressed_cbor.open("wb") as fp:
        subprocess.run(
            ["plutonomy-cli", built_contract, "--default"], stdout=fp, check=True
        )

    subprocess.run(
        [
            sys.executable,
            "-m",
            "uplc",
            "build",
            "--from-cbor",
            built_contract_compressed_cbor,
            "-o",
            f"build/{script.stem}_compressed",
            "--recursion-limit",
            "2000",
        ],
        check=True,
    )


def main():
    print(f"building minting {free_mint.__file__}")
    build_compressed("minting", Path(free_mint.__file__).absolute())
    _, free_mint_policy_id, _ = get_contract("free_mint", True)
    print(f"building rewarding {license_check.__file__}")
    build_compressed(
        "rewarding",
        Path(license_check.__file__).absolute(),
        cli_options=["--cf"],
        args=(json.dumps({"bytes": free_mint_policy_id.payload.hex()}),),
    )

    _, _, license_check_address = get_contract("license_check", True)
    print(f"building spending {orderbook.__file__}")
    build_compressed(
        "spending",
        Path(orderbook.__file__).absolute(),
        cli_options=["--cf"],
        args=(
            StakingHash(ScriptCredential(license_check_address.payment_part.payload))
            .to_cbor()
            .hex(),
        ),
    )


if __name__ == "__main__":
    main()
