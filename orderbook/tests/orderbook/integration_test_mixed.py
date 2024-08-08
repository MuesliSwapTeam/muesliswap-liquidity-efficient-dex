import time
from datetime import datetime, timedelta

from ogmios.errors import ResponseError
from src.offchain.mint_free import mint_free
from src.offchain.mint_license import mint_license
from src.offchain.init_stake_key import init_stake_key
from src.offchain.contract_interaction.place_orders import place_orders
from src.offchain.contract_interaction import fill_order_mixed


def test(
    wallet_a: str = "owner",
    wallet_b: str = "beneficiary",
    wallet_matchmaker: str = "matchmaker",
    token_a: str = "token_a",
    token_b: str = "token_b",
    tx_wait_time: int = 120,
):
    mint_free(name=wallet_a, token_name=token_a, amount=100)
    mint_free(name=wallet_b, token_name=token_b, amount=42)
    mint_license(
        name=wallet_matchmaker,
        amount=1,
        license_expiry=int((datetime.now() + timedelta(days=365)).timestamp()),
    )

    print("Waiting for transaction to be confirmed...")
    time.sleep(tx_wait_time)
    print("Assuming transaction is on-chain.")

    place_orders(
        name=wallet_a,
        beneficiary=wallet_a,
        role=0,
        number=1,
        sell_amount=100,
        buy_amount=42,
        sell_token=token_a,
        buy_token=token_b,
    )
    place_orders(
        name=wallet_b,
        beneficiary=wallet_b,
        role=1,
        number=1,
        sell_amount=42,
        buy_amount=100,
        sell_token=token_a,
        buy_token=token_b,
    )
    try:
        init_stake_key(wallet=wallet_matchmaker)
    except ResponseError as e:
        if "Trying to re-register" not in str(e):
            print("Unexpected error:", str(e))
            exit()

    print("Waiting for transaction to be confirmed...")
    time.sleep(tx_wait_time)
    print("Assuming transaction is on-chain.")

    amount_filled = fill_order_mixed.main(
        name=wallet_matchmaker,
        max_amount=1,
        steal=False,
        take_more_reward=None,
        steal_tokens=False,
    )

    assert amount_filled == 1, f"Expected 1 order to be filled, got {amount_filled}"


if __name__ == "__main__":
    test()
