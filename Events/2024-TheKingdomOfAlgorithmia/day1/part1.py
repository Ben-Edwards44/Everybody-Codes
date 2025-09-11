def get_input():
    with open("part1.txt", "r") as file:
        data = file.read()

    return data.strip()


def get_potions(input):
    potions = 0
    for i in input:
        if i == "B":
            potions += 1
        elif i == "C":
            potions += 3

    return potions


def main():
    inp = get_input()
    potions = get_potions(inp)

    print(potions)


main()