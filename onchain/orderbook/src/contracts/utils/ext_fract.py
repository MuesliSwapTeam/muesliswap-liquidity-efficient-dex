from opshin.std.fractions import *


def gt_zero_fraction(f: Fraction) -> bool:
    """Returns f > 0"""
    return f.denominator * f.numerator > 0


def lt_one_fraction(f: Fraction) -> bool:
    """
    Returns f < 1
    Note: only works if f >= 0
    """
    return abs(f.numerator) < abs(f.denominator)


def one_sub_fraction(f: Fraction) -> Fraction:
    """Returns 1 - f"""
    return Fraction(f.denominator - f.numerator, f.denominator)


def scale_fraction(f: Fraction, s: int) -> Fraction:
    """Returns f * s"""
    return Fraction(s * f.numerator, f.denominator)


def ge_int_fraction(a: int, b: Fraction) -> bool:
    """returns a >= b"""
    if b.denominator >= 0:
        res = a * b.denominator >= b.numerator
    else:
        res = a * b.denominator <= b.numerator
    return res
