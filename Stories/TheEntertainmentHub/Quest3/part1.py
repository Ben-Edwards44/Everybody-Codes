import re


class Die:
    def __init__(self, faces, seed):
        self.faces = faces
        self.seed = seed

        self.pulse = seed
        self.prev_face_inx = 0

    def roll(self, roll_number):
        spin = roll_number * self.pulse

        self.prev_face_inx = (self.prev_face_inx + spin) % len(self.faces)
        self.pulse = ((self.pulse + spin) % self.seed) + 1 + roll_number + self.seed

        landed = self.faces[self.prev_face_inx]

        return landed


def extract_nums(string):
    nums = re.findall(r"-?[0-9]+", string)

    return [int(i) for i in nums]


def get_input():
    with open("part1.txt", "r") as file:
        data = file.read()

    lines = data.splitlines()

    dice = []
    for i in lines:
        nums = extract_nums(i)
        dice.append(Die(nums[1:-1], nums[-1]))

    return dice
    

def get_points(roll_number, dice):
    return sum(i.roll(roll_number) for i in dice)


def main():
    dice = get_input()

    total_points = 0
    roll_number = 0

    while total_points < 10000:
        roll_number += 1
        total_points += get_points(roll_number, dice)

    print(roll_number)


main()
