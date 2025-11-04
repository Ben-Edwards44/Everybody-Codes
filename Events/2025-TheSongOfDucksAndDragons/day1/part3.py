def get_input():
    with open("part3.txt", "r") as file:
        data = file.read()

    names, inst = data.split("\n\n")

    return names.split(","), inst.split(",")


def swap(names, inst):
    move = int(inst[1:])

    if inst[0] == "L":
        mult = -1
    else:
        mult = 1

    swap_pos = mult * move % len(names)
    
    names[0], names[swap_pos] = names[swap_pos], names[0]


def main():
    names, insts = get_input()

    for i in insts:
        swap(names, i)

    print(names[0])


main()