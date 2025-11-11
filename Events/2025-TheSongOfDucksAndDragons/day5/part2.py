class Segment:
    def __init__(self, value):
        self.value = value

        self.left = None
        self.right = None

    def can_add(self, new):
        added = False
        if new < self.value and self.left is None:
            self.left = new
            added = True
        elif new > self.value and self.right is None:
            self.right = new
            added = True

        return added
    

def get_input():
    with open("part2.txt", "r") as file:
        data = file.read().splitlines()

    nums = []
    for i in data:
        _, n = i.split(":")
        nums.append([int(x) for x in n.split(",")])

    return nums
    

def make_spine(nums):
    spine = [Segment(nums[0])]

    for i in nums[1:]:
        added = False
        for x in spine:
            if x.can_add(i):
                added = True
                break

        if not added:
            spine.append(Segment(i))

    return spine


def get_quality(nums):
    spine = make_spine(nums)

    return int("".join(str(i.value) for i in spine))


def main():
    nums = get_input()
    qs = [get_quality(i) for i in nums]

    print(max(qs) - min(qs))


main()