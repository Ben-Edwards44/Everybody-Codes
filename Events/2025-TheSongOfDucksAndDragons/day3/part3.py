def get_input():
    with open("part3.txt", "r") as file:
        data = file.read().strip()

    return [int(i) for i in data.split(",")]


def main():
    sizes = sorted(get_input())

    piles = [sizes[0]]

    for i in sizes[1:]:
        done = False
        for j, k in enumerate(piles):
            if k < i:
                done = True
                piles[j] = i
                break

        if not done:
            piles.append(i)

    print(len(piles))


main()