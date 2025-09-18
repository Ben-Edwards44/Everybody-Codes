def get_input():
    with open("part1.txt", "r") as file:
        data = file.read()

    return data.splitlines()


def can_dig(x, y, grid):
    height = grid[x][y]

    for i in range(-1, 2):
        inx1 = i + x
        if not 0 <= inx1 < len(grid): continue

        for j in range(-1, 2):
            inx2 = j + y
            if not 0 <= inx2 < len(grid[0]) or i != 0 and j != 0: continue

            if grid[inx1][inx2] != height:
                return False
            
    return True


def dig(grid, input):
    new_grid = []
    more = False
    for i, x in enumerate(grid):
        new_grid.append([])
        for j, height in enumerate(x):
            if input[i][j] == "#" and can_dig(i, j, grid):
                height += 1
                more = True

            new_grid[-1].append(height)

    return new_grid, more


def main():
    input = get_input()
    grid = [[0 for _ in i] for i in input]

    more = True
    while more:
        grid, more = dig(grid, input)

    total = sum(sum(i) for i in grid)

    print(total)

main()