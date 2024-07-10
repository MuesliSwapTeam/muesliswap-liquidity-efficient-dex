import typing

import hypothesis
from hypothesis import strategies as hst

from opshin.std.fractions import Fraction
from src.contracts.custom_fract import *
from src.contracts.utils.ext_fract import *
import fractions

non_zero_int = hst.integers().filter(lambda x: x != 0)
int_pair = hst.tuples(hst.integers(), non_zero_int)
Int_Pair = typing.Tuple[int, int]


@hypothesis.given(int_pair)
def test_gt_zero_fraction(p: Int_Pair):
    onchain_frac = Fraction(p[0], p[1])
    frac = fractions.Fraction(p[0], p[1])
    assert (frac > 0) == gt_zero_fraction(onchain_frac)


@hypothesis.given(int_pair)
def test_lt_one_fraction(p: Int_Pair):
    onchain_frac = Fraction(p[0], p[1])
    frac = fractions.Fraction(p[0], p[1])
    hypothesis.assume(frac >= 0)
    assert (frac < 1) == lt_one_fraction(onchain_frac)


@hypothesis.given(int_pair)
def test_sub_one_fraction(p: Int_Pair):
    onchain_frac = Fraction(p[0], p[1])
    frac = fractions.Fraction(p[0], p[1])
    res = one_sub_fraction(onchain_frac)
    assert (1 - frac) == fractions.Fraction(res.numerator, res.denominator)


@hypothesis.given(int_pair, hst.integers())
def test_scale_fraction(p: Int_Pair, s: int):
    onchain_frac = Fraction(p[0], p[1])
    frac = fractions.Fraction(p[0], p[1])
    res = scale_fraction(onchain_frac, s)
    assert s * frac == fractions.Fraction(res.numerator, res.denominator)


@hypothesis.given(int_pair, hst.integers())
def test_ge_int_fraction(p: Int_Pair, s: int):
    onchain_frac = Fraction(p[0], p[1])
    frac = fractions.Fraction(p[0], p[1])
    res = ge_int_fraction(s, onchain_frac)
    assert (s >= frac) == res


# does not hold!
# @hypothesis.given(int_pair, hst.integers())
# def test_no_need_to_norm_fraction(p: Int_Pair, s: int):
#     onchain_frac = norm_fraction(Fraction(p[0], p[1]))
#     res = scale_fraction(one_sub_fraction(onchain_frac), s)
#     assert norm_fraction(res) == res
