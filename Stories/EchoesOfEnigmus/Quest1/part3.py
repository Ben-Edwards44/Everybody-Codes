import re


def read_input():
    with open("part3.txt", "r") as file:
        data = file.read()

    lines = data.splitlines()
    nums = [[int(i) for i in re.findall("[0-9]+", x)] for x in lines]

    return nums


def eni(n, exp, mod):
    rems = []
    prev_score = 1
    for _ in range(mod):
        score = prev_score * n % mod
        prev_score = score

        if score in rems:
            break
        else:
            rems.append(score)

    start = []
    repeating = []
    seen = False
    for i in rems:
        if i == score:
            seen = True

        if seen:
            repeating.append(i)
        else:
            start.append(i)

    need = exp - len(start)

    num_whole = need // len(repeating)
    extra = need % len(repeating)

    return num_whole * sum(repeating) + sum(repeating[:extra]) + sum(start)


def main():
    nums = read_input()

    best = 0
    for i in nums:
        result = eni(i[0], i[3], i[6]) + eni(i[1], i[4], i[6]) + eni(i[2], i[5], i[6])
        best = max(best, result)

    print(best)


main()