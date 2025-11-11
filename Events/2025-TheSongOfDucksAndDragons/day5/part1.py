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
    with open("part1.txt", "r") as file:
        data = file.read()

    _, nums = data.split(":")

    return [int(i) for i in nums.split(",")]
    

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


def main():
    nums = get_input()
    spine = make_spine(nums)

    print("".join(str(i.value) for i in spine))


main()