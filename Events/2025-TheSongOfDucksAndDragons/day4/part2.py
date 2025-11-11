def get_input():
    with open("part2.txt", "r") as file:
        data = file.read()

    return [int(i) for i in data.splitlines()]


def main():
    teeth = get_input()

    turns = 1
    for i, x in enumerate(teeth):
        if i == 0:
            continue

        turns = turns * teeth[i - 1] / x

    needed = 10000000000000 / turns

    if needed % 1 == 0:
        print(int(needed))
    else:
        print(int(needed) + 1)


main()