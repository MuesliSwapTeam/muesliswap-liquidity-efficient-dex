import click
from pycardano import Address, Network, PaymentSigningKey, PaymentVerificationKey

from cardano_python_utils.util import get_signing_info
from ..util import KEYS_DIR

keys_dir = KEYS_DIR


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
    addr_testnet_path = keys_dir.joinpath(f"{name}.addr_test")

    if skey_path.exists():
        print(f"signing key file ${skey_path} already exists, loading")
        signing_key, verification_key, _ = get_signing_info(
            skey_path, network=Network.MAINNET
        )
    else:
        signing_key = PaymentSigningKey.generate()
        signing_key.save(str(skey_path))
        verification_key = PaymentVerificationKey.from_signing_key(signing_key)

    if vkey_path.exists():
        print(f"verification key file ${vkey_path} already exists, comparing")
        verification_key_loaded = PaymentVerificationKey.load(str(vkey_path))
        if verification_key != verification_key_loaded:
            raise ValueError(
                f"verification key file ${vkey_path} already exists, but does not match"
            )
    else:
        verification_key.save(str(vkey_path))

    address = Address(payment_part=verification_key.hash(), network=Network.MAINNET)
    with open(addr_path, mode="w") as f:
        f.write(str(address))
    address_testnet = Address(
        payment_part=verification_key.hash(), network=Network.TESTNET
    )
    with open(addr_testnet_path, mode="w") as f:
        f.write(str(address_testnet))

    print(f"wrote signing key to: {skey_path}")
    print(f"wrote verification key to: {vkey_path}")
    print(f"wrote mainnet address to: {addr_path}")
    print(f"wrote testnet address to: {addr_path}")


if __name__ == "__main__":
    main()
