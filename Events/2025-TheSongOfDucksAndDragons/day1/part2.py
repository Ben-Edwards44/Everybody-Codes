def get_input():
    with open("part2.txt", "r") as file:
        data = file.read()

    names, inst = data.split("\n\n")

    return names.split(","), inst.split(",")


def next_pos(current, names, inst):
    move = int(inst[1:])

    if inst[0] == "L":
        mult = -1
    else:
        mult = 1

    new_pos = current + mult * move
    
    return new_pos % len(names)


def main():
    names, insts = get_input()

    pos = 0
    for i in insts:
        pos = next_pos(pos, names, i)

    print(names[pos])


main()