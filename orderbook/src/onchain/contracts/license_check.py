"""
MuesliSwap License Checker

Checks that a license token is spent in the transaction and still valid.
"""


from src.onchain.contracts.custom_fract import *
from src.onchain.contracts.utils.ext_interval import *


def check_license(
    license_policy: PolicyId,
    license_index: int,
    tx_info: TxInfo,
) -> None:
    license_input = tx_info.inputs[license_index]
    license_name = license_input.resolved.value[license_policy].keys()[0]
    deadline = unsigned_int_from_bytes_big(license_name)
    maximum_tx_validity: FinitePOSIXTime = tx_info.valid_range.upper_bound.limit
    assert deadline >= maximum_tx_validity.time, "License expired"


def validator(license_policy: PolicyId, redeemer: int, context: ScriptContext):
    tx_info = context.tx_info
    purpose = context.purpose
    assert isinstance(purpose, Rewarding), "Invalid script purpose"
    # check that the spending license is present at the right input
    license_index = redeemer
    check_license(license_policy, license_index, tx_info)