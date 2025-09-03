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
    

class Player:
    def __init__(self, die, track, id):
        self.die = die
        self.track = track
        self.id = str(id)

        self.pos = 0

    def make_move(self, roll_number):
        rolled = self.die.roll(roll_number)

        if rolled == self.track[self.pos]:
            self.pos += 1

        return self.pos >= len(self.track)


def extract_nums(string):
    nums = re.findall(r"-?[0-9]+", string)

    return [int(i) for i in nums]


def get_input():
    with open("part2.txt", "r") as file:
        data = file.read()

    die, track = data.split("\n\n")

    dice = []
    for i in die.splitlines():
        nums = extract_nums(i)
        dice.append(Die(nums[1:-1], nums[-1]))

    track = [int(i) for i in track.strip()]

    players = [Player(x, track, i + 1) for i, x in enumerate(dice)]

    return players


def main():
    players = get_input()

    order = []
    roll_number = 1
    while len(players) > 1:
        new_players = []
        for i in players:
            finished = i.make_move(roll_number)

            if finished:
                order.append(i.id)
            else:
                new_players.append(i)

        players = new_players
        roll_number += 1

    order.append(players[0].id)  #no need to wait for final one to finish

    print(",".join(order))


main()
