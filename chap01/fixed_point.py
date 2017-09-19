def fixed_point(f, guess):
    def close_enough(v1, v2):
        return abs(v1 - v2) < 0.0001
    try_with_guess = f(guess)
    if close_enough(guess, try_with_guess):
        return try_with_guess
    return fixed_point(f, try_with_guess)


def square_root(x):
    return fixed_point(
        lambda y: (y + (x / y)) / 2,
        1.0)

