def get_input():
    with open("part3.txt", "r") as file:
        data = file.read().splitlines()

    start = int(data[0])
    end = int(data[-1])

    teeth = [[int(i) for i in x.split("|")] for x in data[1:-1]]

    return start, end, teeth


def main():
    start, end, teeth = get_input()

    prev = start
    turns = 100
    for current, next in teeth:
        turns = turns * prev / current

        prev = next

    turns = turns * prev / end

    print(int(turns))


main()