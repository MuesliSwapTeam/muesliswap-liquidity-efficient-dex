import argparse
import copy


import cardano_clusterlib.clusterlib as cardano_clusterlib
from cardano_clusterlib import clusterlib

import cli_lib
from cardano_python_utils import minutxo, clusterfuck
from cardano_python_utils.clusterfuck import utxodata
from src.util import *

WALLET_SKEYFILE = Path(
    "payment_PIGYToken.skey" if MAINNET else "payment_mint_license.skey"
)
WALLET_ADDRESS = Bech32Addr(
    "addr1vyklezgztsh7hekewxdglwwud67eg6wa8npy8eeeajdcqysel33ar"
)

DEST_ADDRESS = Bech32Addr(
    "addr1q8dfc0ranqlgfadwr60gus36klkyd3svyu97runtvmve7ny7dmdgup7uuqjfx0fkftwu29vkfr3vpxqwzdnw8f89c3dq9h8nj2"
    if MAINNET
    else "addr_test1qp2746rhpulv26akjx4awwrqyzzj9u788yxtm68uk9dhvnkrwk2rtvqglxpcj6qm50ng3mcsswrh8k6kt2c6ft9zew6qvhdzdk"
)
STAKING_ADDRESS = Bech32Addr(
    "stake1uxux9ednue7htjfftvyavkvc9v8p2urjz4ktewpqkd723hg8343xg"
)

LICENSE_VALIDITY = datetime.datetime(2024, 1, 1)
MATCHER_LICENSE_SCRIPT_FILE = CARDANO_DIR.joinpath("matcher_license.script")
MATCHER_LICENSE_SKEY_FILE = CARDANO_DIR.joinpath("matcher_license.skey")
BATCHER_LICENSE_SCRIPT_FILE = CARDANO_DIR.joinpath("batcher_license.script")
BATCHER_LICENSE_SKEY_FILE = CARDANO_DIR.joinpath("batcher_license.skey")
SWAPPER_LICENSE_SCRIPT_FILE = CARDANO_DIR.joinpath("swapper_license.script")
SWAPPER_LICENSE_SKEY_FILE = CARDANO_DIR.joinpath("swapper_license.skey")
CREATOR_LICENSE_SCRIPT_FILE = CARDANO_DIR.joinpath("creator_license.script")
CREATOR_LICENSE_SKEY_FILE = CARDANO_DIR.joinpath("creator_license.skey")
MATCHER_LICENSE_SCRIPT_FILE = CARDANO_DIR.joinpath("matcher_license.script")
BATCHER_LICENSE_SCRIPT_FILE = CARDANO_DIR.joinpath("batcher_license.script")
SWAPPER_LICENSE_SCRIPT_FILE = CARDANO_DIR.joinpath("swapper_license.script")
CREATOR_LICENSE_SCRIPT_FILE = CARDANO_DIR.joinpath("creator_license.script")
MATCHER_LICENSE_SKEY_FILE = CARDANO_DIR.joinpath("matcher_license.skey")
BATCHER_LICENSE_SKEY_FILE = CARDANO_DIR.joinpath("batcher_license.skey")
SWAPPER_LICENSE_SKEY_FILE = CARDANO_DIR.joinpath("swapper_license.skey")
CREATOR_LICENSE_SKEY_FILE = CARDANO_DIR.joinpath("creator_license.skey")

METADATA_MINT_FILE_CONTENT = {
    "674": {"msg": ["MuesliSwap Matching License Mint"]},
}


def mint(wallet: ShelleyAddress, signing_key: Path, dest: ShelleyAddress, amount: int):
    tokenname = hex(round(LICENSE_VALIDITY.timestamp() * 1000))[2:]
    # has to have even length
    if len(tokenname) % 2 == 1:
        tokenname = "0" + tokenname
    license_tokens = [
        (Token(BATCHER_LICENSE_POLICY_ID, tokenname), BATCHER_LICENSE_SCRIPT_FILE),
        (Token(MATCHER_LICENSE_POLICY_ID, tokenname), MATCHER_LICENSE_SCRIPT_FILE),
        (Token(SWAPPER_LICENSE_POLICY_ID, tokenname), SWAPPER_LICENSE_SCRIPT_FILE),
        (Token(CREATOR_LICENSE_POLICY_ID, tokenname), CREATOR_LICENSE_SCRIPT_FILE),
        (Token(BATCHER_LICENSE_POLICY_ID_CLP, tokenname), BATCHER_LICENSE_SCRIPT_FILE),
        (Token(MATCHER_LICENSE_POLICY_ID_CLP, tokenname), MATCHER_LICENSE_SCRIPT_FILE),
        (Token(SWAPPER_LICENSE_POLICY_ID_CLP, tokenname), SWAPPER_LICENSE_SCRIPT_FILE),
        (Token(CREATOR_LICENSE_POLICY_ID_CLP, tokenname), CREATOR_LICENSE_SCRIPT_FILE),
    ]

    utxos = cli_lib.fetch_utxos(wallet.bech32)
    spend_utxos = []
    collected = 0
    min_sendalong = minutxo.min_utxo_ada([t[0] for t in license_tokens], False)
    while collected < MIN_ADA_TRANSFER + min_sendalong:
        u = utxos.pop(-1)
        if not u.assets:
            spend_utxos.append(u)
            collected += u.amount
    txins = [utxodata(u, wallet.bech32) for u in spend_utxos]
    txname = uuid.uuid4()

    mint_txouts = []
    txouts = []
    mint = []
    for license_token, license_script in license_tokens:
        mint_tx = cardano_clusterlib.TxOut(
            dest.bech32,
            amount,
            license_token.to_hex(),
        )
        mint_txouts.append(mint_tx)
        mint.append(cardano_clusterlib.Mint([mint_tx], license_script))
    txouts.append(
        cardano_clusterlib.TxOut(
            dest.bech32,
            min_sendalong,
        )
    )
    metadata = copy.deepcopy(METADATA_MINT_FILE_CONTENT)
    metadata["674"]["msg"].append(f"Valid until {LICENSE_VALIDITY.isoformat()}")
    metadata_file = TX_FILES_DIR + "/metadata_mint_" + str(tokenname) + ".json"
    with open(metadata_file, "w") as mfp:
        json.dump(metadata, mfp)
    wallet_skeyfile = clusterlib.TxFiles(
        signing_key_files=[
            signing_key,
            CREATOR_LICENSE_SKEY_FILE,
            BATCHER_LICENSE_SKEY_FILE,
            MATCHER_LICENSE_SKEY_FILE,
            SWAPPER_LICENSE_SKEY_FILE,
        ],
        metadata_json_files=[metadata_file],
    )
    rem_lvl = collected - min_sendalong
    txouts.append(cardano_clusterlib.TxOut(wallet.bech32, rem_lvl))
    fee = cli_lib.CL.calculate_tx_fee(
        src_address=wallet.bech32,
        txins=txins,
        tx_name=txname,
        tx_files=wallet_skeyfile,
        txouts=txouts + mint_txouts,
        mint=mint,
        join_txouts=True,
        invalid_hereafter=slot_of_time(LICENSE_VALIDITY),
        destination_dir=TX_FILES_DIR,
    )
    txouts.pop(-1)
    rem_lvl -= fee
    txouts.append(cardano_clusterlib.TxOut(wallet.bech32, rem_lvl))
    mint_tx = cli_lib.CL.build_raw_tx(
        src_address=wallet.bech32,
        txins=txins,
        tx_name=txname,
        tx_files=wallet_skeyfile,
        txouts=txouts + mint_txouts,
        mint=mint,
        fee=fee,
        join_txouts=True,
        destination_dir=TX_FILES_DIR,
    )
    mint_tx_signed = cli_lib.CL.sign_tx(
        signing_key_files=wallet_skeyfile.signing_key_files,
        tx_name=txname,
        tx_body_file=mint_tx.out_file,
        destination_dir=TX_FILES_DIR,
    )
    cli_lib.CL.submit_tx_bare(mint_tx_signed)
    submit_ext(mint_tx_signed, BLOCKFROST)
    print(f"Submitted successfully, waiting for confirmation. Tx: {mint_tx_signed}")


def main(wallet: ShelleyAddress, signing_key: Path):
    mint(wallet, signing_key, dest_address, amount)
    # for utxo in cli_lib.fetch_utxos(wallet.bech32):
    #     sa = sender_addresses(utxo.tx_hash, BLOCKFROST)
    #     if wallet.bech32 in sa or utxo.amount < MIN_ADA_TRANSFER or utxo.assets:
    #         continue
    #     mint(wallet, signing_key, ShelleyAddress.from_bech32(sa[0]), 1)


if __name__ == "__main__":
    a = argparse.ArgumentParser()
    a.add_argument(
        "--wallet_signing_key",
        help="Secret key file of wallet address to be used to mint licenses",
        type=Path,
        default=WALLET_SKEYFILE,
    )
    a.add_argument("--amount", help="Amount of licenses to mint", type=int, default=40)
    a.add_argument(
        "--dest", help="Mint destination", type=Bech32Addr, default=DEST_ADDRESS
    )
    args = a.parse_args()

    # init wallet address
    wallet_address = ShelleyAddress.from_bech32(args.wallet_address)
    assert wallet_address == clusterfuck.address_from_signing_key(
        args.wallet_signing_key, cli_lib.CL
    )
    dest_address = ShelleyAddress.from_bech32(args.dest)
    amount = args.amount

    main(wallet_address, args.wallet_signing_key)
