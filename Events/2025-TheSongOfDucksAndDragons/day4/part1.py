def get_input():
    with open("part1.txt", "r") as file:
        data = file.read()

    return [int(i) for i in data.splitlines()]


def main():
    teeth = get_input()

    turns = 2025
    for i, x in enumerate(teeth):
        if i == 0:
            continue

        turns = turns * teeth[i - 1] / x

    print(int(turns))


main()