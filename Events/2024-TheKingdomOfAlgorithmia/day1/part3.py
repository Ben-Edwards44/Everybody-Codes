def get_input():
    with open("part3.txt", "r") as file:
        data = file.read()

    return data.strip()


def get_potions(input):
    potions = 0
    for i in range(0, len(input), 3):
        pair = input[i : i + 3]

        for x in pair:                
            if x == "B":
                potions += 1
            elif x == "C":
                potions += 3
            elif x == "D":
                potions += 5

        num_x = pair.count("x")

        if num_x == 0:
            potions += 6
        elif num_x == 1:
            potions += 2

    return potions


def main():
    inp = get_input()
    potions = get_potions(inp)

    print(potions)


main()