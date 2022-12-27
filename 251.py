#!/usr/bin/env python

def generate_lines(filename):
    with open(sys.argv[1], 'rt') as f:
        for line in f:
            yield line[:-1]

snafu_values = {
    '=': -2,
    '-': -1,
    '0': 0,
    '1': 1,
    '2': 2,
}
snafu_digits = {
    -2: '=',
    -1: '-',
    0: '0',
    1: '1',
    2: '2',
}

def parse_snafu(s):
    factor = 1
    value = 0
    for snafu_digit in reversed(s):
        value += snafu_values[snafu_digit] * factor
        factor *= 5
    return value

def to_snafu(n):
    digits = []
    nn = n
    while nn:
        nn, rest = divmod(nn, 5)
        if rest > 2:
            rest = rest - 5
            nn += 1
        digits.insert(0, snafu_digits[rest])
    return ''.join(digits)


if __name__ == "__main__":
    import sys
    lines = generate_lines(sys.argv[1])
    total = sum(map(parse_snafu, lines))
    print(to_snafu(total))

