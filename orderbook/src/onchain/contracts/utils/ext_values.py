from opshin.prelude import *

EMTPY_TOKENNAME_DICT: Dict[TokenName, int] = {}
EMPTY_VALUE_DICT: Dict[PolicyId, Dict[TokenName, int]] = {}


def merge_without_duplicates(a: List[bytes], b: List[bytes]) -> List[bytes]:
    """
    Merge two lists without duplicates
    Note: The cost of this is O(n^2), can we assume that the lists are small?
    Rough estimate allows 1000 bytes / 32 bytes per policy id ~ 31 policy ids
    However for token names no lower bound on the length is given, so we assume 1000 bytes / 1 byte per token name ~ 1000 token names
    """
    return [x for x in a if not x in b] + b


def _subtract_token_names(
    a: Dict[TokenName, int], b: Dict[TokenName, int]
) -> Dict[TokenName, int]:
    """
    Subtract b from a, return a - b
    """
    if not b:
        return a
    elif not a:
        return {tn_amount[0]: -tn_amount[1] for tn_amount in b.items()}
    return {
        tn: a.get(tn, 0) - b.get(tn, 0)
        for tn in merge_without_duplicates(a.keys(), b.keys())
    }


def subtract_value(a: Value, b: Value) -> Value:
    """
    Subtract b from a, return a - b
    """
    if not b:
        return a
    elif not a:
        return {
            pid_tokens[0]: {
                tn_amount[0]: -tn_amount[1] for tn_amount in pid_tokens[1].items()
            }
            for pid_tokens in b.items()
        }
    return {
        pid: _subtract_token_names(
            a.get(pid, EMTPY_TOKENNAME_DICT), b.get(pid, EMTPY_TOKENNAME_DICT)
        )
        for pid in merge_without_duplicates(a.keys(), b.keys())
    }


def subtract_lovelace(a: Value, b: int) -> Value:
    """
    Subtract b lovelace from a, return a - b
    """
    return subtract_value(a, {b"": {b"": b}})


def _add_token_names(
    a: Dict[TokenName, int], b: Dict[TokenName, int]
) -> Dict[TokenName, int]:
    """
    Add b to a, return a + b
    """
    if not a:
        return b
    if not b:
        return a
    return {
        tn: a.get(tn, 0) + b.get(tn, 0)
        for tn in merge_without_duplicates(a.keys(), b.keys())
    }


def add_value(a: Value, b: Value) -> Value:
    """
    Add b to a, return a + b
    """
    if not a:
        return b
    if not b:
        return a
    return {
        pid: _add_token_names(
            a.get(pid, EMTPY_TOKENNAME_DICT), b.get(pid, EMTPY_TOKENNAME_DICT)
        )
        for pid in merge_without_duplicates(a.keys(), b.keys())
    }


def add_lovelace(a: Value, b: int) -> Value:
    """
    Add b lovelace to a, return a + b
    """
    return add_value(a, {b"": {b"": b}})


def value_from_token(token: Token, amount: int) -> Value:
    """
    Create a value from a single token
    """
    return {token.policy_id: {token.token_name: amount}}


def total_value(value_store_inputs: List[TxOut]) -> Value:
    """
    Calculate the total value of all inputs
    """
    total_value = EMPTY_VALUE_DICT
    for txo in value_store_inputs:
        total_value = add_value(total_value, txo.value)
    return total_value


def check_greater_or_equal_value(a: Value, b: Value) -> None:
    """
    Check that the value of a is greater or equal to the value of b, i.e. a >= b
    """
    for policy_id, tokens in b.items():
        for token_name, amount in tokens.items():
            assert (
                a.get(policy_id, {b"": 0}).get(token_name, 0) >= amount
            ), f"Value of {policy_id.hex()}.{token_name.hex()} is too low"


def check_preserves_value(
    previous_state_input: TxOut, next_state_output: TxOut
) -> None:
    """
    Check that the value of the previous state input is equal to the value of the next state output
    """
    previous_state_value = previous_state_input.value
    next_state_value = next_state_output.value
    check_greater_or_equal_value(next_state_value, previous_state_value)


def token_amount_in_value(v: Value, t: Token) -> int:
    """Returns the amount of token t in value v. Returns 0 if token is not present"""
    return v.get(t.policy_id, EMTPY_TOKENNAME_DICT).get(t.token_name, 0)


def token_amount_in_output(o: TxOut, t: Token) -> int:
    """Returns the amount of token t in output o. Returns 0 if token is not present"""
    return token_amount_in_value(o.value, t)


def token_present_in_output(token: Token, output: TxOut) -> bool:
    """
    Returns whether the given token is contained in the output
    """
    return token_amount_in_output(output, token) > 0


def token_amount_in_value_unsafe(v: Value, t: Token) -> int:
    """Returns the amount of token t in value v. Fails if token not present"""
    return v[t.policy_id][t.token_name]


def token_amount_in_output_unsafe(o: TxOut, t: Token) -> int:
    """Returns the amount of token t in output o. Fails if token not present"""
    return token_amount_in_value_unsafe(o.value, t)


def check_token_present_in_output(token: Token, output: TxOut) -> None:
    """
    Passes if the given token is contained in the output
    """
    token_amount_in_output_unsafe(output, token)
