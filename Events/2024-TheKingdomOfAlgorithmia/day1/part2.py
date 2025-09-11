def get_input():
    with open("part2.txt", "r") as file:
        data = file.read()

    return data.strip()


def get_potions(input):
    potions = 0
    for i in range(0, len(input), 2):
        pair = input[i : i + 2]

        for x in pair:
            if x == "B":
                potions += 1
            elif x == "C":
                potions += 3
            elif x == "D":
                potions += 5

        if "x" not in pair:
            potions += 2

    return potions


def main():
    inp = get_input()
    potions = get_potions(inp)

    print(potions)


main()