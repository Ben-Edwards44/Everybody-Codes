class Comp:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def __add__(self, other):
        return Comp(self.x + other.x, self.y + other.y)
    
    def __mul__(self, other):
        return Comp(self.x * other.x - self.y * other.y, self.x * other.y + self.y * other.x)
    
    def __truediv__(self, other):
        return Comp(self.x // other.x, self.y // other.y)
    
    def __repr__(self):
        return f"[{self.x},{self.y}]"
    

def get_input():
    with open("part1.txt", "r") as file:
        data = file.read().strip()

    l, r = data.split(",")

    x = int(l[3:])
    y = int(r[:-1])

    return Comp(x, y)
    

def cycle(result, a):
    return result * result / Comp(10, 10) + a


def main():
    a = get_input()

    r = Comp(0, 0)
    for _ in range(3):
        r = cycle(r, a)

    print(r)


main()