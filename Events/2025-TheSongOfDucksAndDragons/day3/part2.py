def get_input():
    with open("part2.txt", "r") as file:
        data = file.read().strip()

    return [int(i) for i in data.split(",")]


def main():
    sizes = sorted(get_input())

    total = 0
    done = 0
    for i, x in enumerate(sizes):
        if i == 0 or x > sizes[i - 1]:
            done += 1
            total += x

        if done >= 20:
            break

    print(total)


main()