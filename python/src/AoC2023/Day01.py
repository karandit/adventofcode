#with open("../../inputs/2023/day01S.txt") as f:
with open("../../../inputs/2023/day01.txt") as f:
    contents = f.read()
lines = contents.splitlines()

def solve(f):
    sum = 0
    for ln in lines:
        ln_proc = f(ln)
        digits = list(c for c in ln_proc if c.isdecimal())
        sum += int(digits[0] + digits[-1])
    return sum

part1 = solve(lambda x: x)
print(part1)

def insert_digits(s):
    for i, nr in enumerate(["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"], start=1):
        s = s.replace(nr, nr[:1] + str(i) + nr[1:])
    return s

part2 = solve(insert_digits)
print(part2)

