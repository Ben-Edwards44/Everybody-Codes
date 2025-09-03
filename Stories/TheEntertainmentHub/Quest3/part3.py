import re


SHOW_RESULT = True


class Die:
    def __init__(self, faces, seed):
        self.faces = faces
        self.seed = seed

        self.pulse = seed
        self.prev_face_inx = 0

        self.rolls = []

    def roll(self, roll_number):
        cache_inx = roll_number - 1
        if cache_inx < len(self.rolls):
            return self.rolls[cache_inx]

        spin = roll_number * self.pulse

        self.prev_face_inx = (self.prev_face_inx + spin) % len(self.faces)
        self.pulse = ((self.pulse + spin) % self.seed) + 1 + roll_number + self.seed

        landed = self.faces[self.prev_face_inx]

        self.rolls.append(landed)  #new roll is guaranteed to be just 1 more than the previous

        return landed


def extract_nums(string):
    nums = re.findall(r"-?[0-9]+", string)

    return [int(i) for i in nums]


def get_input():
    with open("part3.txt", "r") as file:
        data = file.read()

    dice_str, map_str = data.split("\n\n")

    dice = []
    for i in dice_str.splitlines():
        nums = extract_nums(i)
        dice.append(Die(nums[1:-1], nums[-1]))

    map = [[int(i) for i in x] for x in map_str.splitlines()]

    return dice, map


def write_file(filename, text):
    with open(filename, "w") as file:
        file.write(text)


def get_alive_neighbours(map, x, y, roll, accessible, new_alive):
    for i in range(-1, 2):
        inx1 = x + i
        if not 0 <= inx1 < len(map): continue

        for j in range(-1, 2):
            inx2 = y + j
            if not 0 <= inx2 < len(map[0]) or i != 0 and j != 0: continue

            if map[inx1][inx2] == roll:
                new_alive.add((inx1, inx2))
                accessible.add((inx1, inx2))


def get_accessible_for_die(map, die):
    accesible = set()

    roll_number = 1
    first_roll = die.roll(roll_number)
    alive_cells = set()
    for i, x in enumerate(map):
        for j, k in enumerate(x):
            if k == first_roll:
                accesible.add((i, j))
                alive_cells.add((i, j))

    while len(alive_cells) > 0:
        roll_number += 1
        new_roll = die.roll(roll_number)

        new_alive = set()
        for x, y in alive_cells:
            get_alive_neighbours(map, x, y, new_roll, accesible, new_alive)

        alive_cells = new_alive

    return accesible


def get_max_coins(all_dice, map):
    all_accessible = set()
    for i in all_dice:
        current_accesible = get_accessible_for_die(map, i)

        for x in current_accesible:
            all_accessible.add(x)

    if SHOW_RESULT:
        print_solution(map, all_accessible)

    return len(all_accessible)


def print_solution(map, all_accessible):
    solution = ""
    for i in range(len(map)):
        line = ""
        for x in range(len(map[0])):
            if (i, x) in all_accessible:
                char = "#"
            else:
                char = "."

            line += char
        
        solution += f"{line}\n"

    write_file("part3_solution.txt", solution)


def main():
    dice, map = get_input()
    max_coins = get_max_coins(dice, map)

    print(max_coins)


main()