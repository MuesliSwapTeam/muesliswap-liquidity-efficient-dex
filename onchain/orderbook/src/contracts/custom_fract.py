from opshin.std.fractions import *


def between_zero_and_one(numerator: int, denominator: int) -> bool:
    """
    Returns 0 < f < 1
    """
    return (denominator * numerator > 0) and (abs(numerator) < abs(denominator))


def scale_one_sub_fraction_integer(f_numerator: int, f_denominator: int, s: int) -> int:
    """
    Returns (1 - f) * s assuming that the result is integer

    i.e. equivalent to
    res = norm_fraction(
        scale_fraction(one_sub_fraction(f), s)
    )
    assert (
        res.denominator == 1
    ), "New buy amount is not a proper integer"
    return res.numerator
    but faster
    """
    one_sub_numerator = f_denominator - f_numerator
    one_sub_denominator = f_denominator
    scale_numerator = s * one_sub_numerator
    scale_denominator = one_sub_denominator
    norm_signs_numerator = sign(scale_denominator) * scale_numerator
    norm_signs_denominator = abs(scale_denominator)
    g = gcd(norm_signs_numerator, norm_signs_denominator)
    norm_gcd_numerator = norm_signs_numerator // g
    norm_gcd_denominator = norm_signs_denominator // g
    assert norm_gcd_denominator == 1, "Result is not a proper integer"
    return norm_gcd_numerator


def floor_scale_fraction(f_numerator: int, f_denominator: int, s: int) -> int:
    """
    Returns floor(f * s)
    """
    scale_numerator = f_numerator * s
    scale_denominator = f_denominator
    return scale_numerator // scale_denominator


def ceil_scale_fraction(f_numerator: int, f_denominator: int, s: int) -> int:
    """
    Returns ceil(f * s)
    """
    scale_numerator = f_numerator * s
    scale_denominator = f_denominator
    return (
        scale_numerator + scale_denominator - sign(scale_denominator)
    ) // scale_denominator