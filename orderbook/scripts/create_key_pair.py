from pathlib import Path

import click
from pycardano import Address, Network, PaymentSigningKey, PaymentVerificationKey

keys_dir = Path(__file__).parent.parent.joinpath("keys")


@click.command()
@click.argument("name")
def main(name):
    """
    Creates a testnet signing key, verification key, and address.
    """
    keys_dir.mkdir(exist_ok=True)
    skey_path = keys_dir.joinpath(f"{name}.skey")
    vkey_path = keys_dir.joinpath(f"{name}.vkey")
    addr_path = keys_dir.joinpath(f"{name}.addr")
    test_addr_path = keys_dir.joinpath(f"{name}.test_addr")

    if skey_path.exists():
        signing_key = PaymentSigningKey.load(str(skey_path))
        print(f"signing key file ${skey_path} already exists, loaded")
    else:
        signing_key = PaymentSigningKey.generate()
        signing_key.save(str(skey_path))
        print(f"wrote signing key to: {skey_path}")

    verification_key = PaymentVerificationKey.from_signing_key(signing_key)
    if not vkey_path.exists():
        verification_key.save(str(vkey_path))
        print(f"wrote verification key to: {vkey_path}")
    else:
        print(f"verification key file ${vkey_path} already exists, loaded")
        loaded_verification_key = PaymentVerificationKey.load(str(vkey_path))
        assert (
            verification_key == loaded_verification_key
        ), "verification keys do not match, aborting"

    address = Address(payment_part=verification_key.hash(), network=Network.MAINNET)
    with open(addr_path, mode="w") as f:
        f.write(str(address))

    test_addr = Address(payment_part=verification_key.hash(), network=Network.TESTNET)
    with open(test_addr_path, mode="w") as f:
        f.write(str(test_addr))

    print(f"wrote mainnet address to: {addr_path}")
    print(f"wrote testnet address to: {test_addr_path}")


if __name__ == "__main__":
    main()
