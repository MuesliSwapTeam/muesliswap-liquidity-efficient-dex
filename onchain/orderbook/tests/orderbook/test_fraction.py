import math
import typing

import hypothesis
from hypothesis import strategies as hst

from opshin.std.fractions import Fraction
from src.contracts.custom_fract import *
import fractions

non_zero_int = hst.integers().filter(lambda x: x != 0)
int_pair = hst.tuples(hst.integers(), non_zero_int)
Int_Pair = typing.Tuple[int, int]


@hypothesis.given(int_pair)
def test_between_zero_and_one_fraction(p: Int_Pair):
    onchain_frac = Fraction(p[0], p[1])
    frac = fractions.Fraction(p[0], p[1])
    assert (0 < frac < 1) == between_zero_and_one(
        onchain_frac.numerator, onchain_frac.denominator
    )


@hypothesis.given(int_pair, hst.integers())
def test_scale_one_sub_scale_fraction_integer_only_int(p: Int_Pair, s: int):
    onchain_frac = Fraction(p[0], p[1])
    frac = fractions.Fraction(p[0], p[1])
    s *= (1 - frac).denominator
    expected_res = (1 - frac) * s
    assert expected_res.denominator == 1
    try:
        res = scale_one_sub_fraction_integer(
            onchain_frac.numerator, onchain_frac.denominator, s
        )
    except AssertionError:
        assert expected_res.denominator != 1
    else:
        assert res == expected_res


@hypothesis.given(int_pair, hst.integers())
def test_scale_one_sub_scale_fraction_integer(p: Int_Pair, s: int):
    onchain_frac = Fraction(p[0], p[1])
    frac = fractions.Fraction(p[0], p[1])
    expected_res = (1 - frac) * s
    try:
        res = scale_one_sub_fraction_integer(
            onchain_frac.numerator, onchain_frac.denominator, s
        )
    except AssertionError:
        assert expected_res.denominator != 1
    else:
        assert res == expected_res


@hypothesis.given(int_pair, hst.integers())
def test_floor_scale_fraction(p: Int_Pair, s: int):
    onchain_frac = Fraction(p[0], p[1])
    frac = fractions.Fraction(p[0], p[1])
    expected_res = int(math.floor(frac * s))
    res = floor_scale_fraction(onchain_frac.numerator, onchain_frac.denominator, s)
    assert res == expected_res


@hypothesis.given(int_pair, hst.integers())
def test_ceil_scale_fraction(p: Int_Pair, s: int):
    onchain_frac = Fraction(p[0], p[1])
    frac = fractions.Fraction(p[0], p[1])
    expected_res = int(math.ceil(frac * s))
    res = ceil_scale_fraction(onchain_frac.numerator, onchain_frac.denominator, s)
    assert res == expected_res
