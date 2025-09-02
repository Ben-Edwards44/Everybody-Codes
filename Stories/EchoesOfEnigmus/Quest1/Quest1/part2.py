import re


def read_input():
    with open("part2.txt", "r") as file:
        data = file.read()

    lines = data.splitlines()
    nums = [[int(i) for i in re.findall("[0-9]+", x)] for x in lines]

    return nums


def square_mult(base, exp, mod):
    if exp == 1:
        return base % mod
    
    prev = square_mult(base, exp >> 1, mod)
    new = prev * prev

    if exp & 1 == 1:
        new *= base

    return new % mod


def eni(n, exp, mod):
    rems = []
    prev_score = square_mult(n, exp - 5, mod)
    for _ in range(5):
        score = prev_score * n % mod
        prev_score = score
        rems = [str(score)] + rems

        if len(rems) > 5:
            rems.pop()

    return int("".join(rems))


def main():
    nums = read_input()

    best = 0
    for i in nums:
        result = eni(i[0], i[3], i[6]) + eni(i[1], i[4], i[6]) + eni(i[2], i[5], i[6])
        best = max(best, result)

    print(best)


main()