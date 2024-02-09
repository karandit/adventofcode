def solve(data, f):
    lines = data.splitlines()
    sum = 0
    for ln in lines:
        ln_proc = f(ln)
        digits = list(c for c in ln_proc if c.isdecimal())
        sum += int(digits[0] + digits[-1])
    return sum

def insert_digits(s):
    for i, nr in enumerate(["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"], start=1):
        s = s.replace(nr, nr[:1] + str(i) + nr[1:])
    return s

def aoc202301(data):
    part1 = solve(data, lambda x: x)
    part2 = solve(data, insert_digits)
    return part1, part2
