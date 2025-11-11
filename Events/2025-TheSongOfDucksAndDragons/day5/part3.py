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
    
    def get_num(self):
        n = ""
        if self.left is not None:
            n += str(self.left)
        
        n += str(self.value)

        if self.right is not None:
            n += str(self.right)

        return int(n)
    

def get_input():
    with open("part3.txt", "r") as file:
        data = file.read().splitlines()

    ids = []
    nums = []
    for i in data:
        id, n = i.split(":")

        ids.append(int(id))
        nums.append([int(x) for x in n.split(",")])

    return ids, nums
    

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


def get_qualities(ids, all_nums):
    all_qs = {}
    for i, x in enumerate(all_nums):
        spine = make_spine(x)
        q = int("".join(str(i.value) for i in spine))

        all_qs[ids[i]] = (spine, q)

    return all_qs


def left_higher(left_id, right_id, qs):
    ls, lq = qs[left_id]
    rs, rq = qs[right_id]

    if lq != rq:
        return lq > rq
    
    for i, x in enumerate(ls):
        l = x.get_num()
        r = rs[i].get_num()

        if l != r:
            return l > r
        
    return left_id > right_id


def sort_ids(ids, qs):
    swapped = True
    while swapped:
        swapped = False

        for i, x in enumerate(ids):
            if i == 0:
                continue

            if left_higher(ids[i - 1], x, qs):
                swapped = True
                ids[i - 1], ids[i] = x, ids[i - 1]

    return ids


def main():
    ids, nums = get_input()
    qs = get_qualities(ids, nums)
    sorted_ids = sort_ids(ids, qs)[::-1]

    total = 0
    for i, x in enumerate(sorted_ids):
        total += (i + 1) * x

    print(total)


main()