class Comp:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def __add__(self, other):
        return Comp(self.x + other.x, self.y + other.y)
    
    def __mul__(self, other):
        return Comp(self.x * other.x - self.y * other.y, self.x * other.y + self.y * other.x)
    
    def __truediv__(self, other):
        x = self.x // other.x
        y = self.y // other.y

        if x < 0 and self.x % other.x != 0:
            x += 1
        if y < 0 and self.y % other.y != 0:
            y += 1

        return Comp(x, y)
    
    def __repr__(self):
        return f"[{self.x},{self.y}]"
    

def get_input():
    with open("part2.txt", "r") as file:
        data = file.read().strip()

    l, r = data.split(",")

    x = int(l[3:])
    y = int(r[:-1])

    return Comp(x, y)
    

def cycle(result, examine_point):
    return (result * result) / Comp(100000, 100000) + examine_point


def should_engrave(point):
    check = Comp(0, 0)
    for _ in range(100):
        check = cycle(check, point)

        if not abs(check.x) < 1000000 or not abs(check.y) < 1000000:
            return False
        
    return True


def main():
    a = get_input()

    total = 0
    for i in range(101):
        for j in range(101):
            point = a + Comp(10 * i, 10 * j)
    
            if should_engrave(point):
                total += 1

    print(total)


main()