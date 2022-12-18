#!/usr/bin/env python
from itertools import count

class Cell:
    def __init__(self, c):
        self.c = c
        self.distance = None

    def elevation(self):
        c = self.c
        if c == 'S': c = 'a'
        if c == 'E': c = 'z'
        return ord(c)


class Grid:
    def __init__(self, lines):
        self.data = [ Cell(c) for l in lines for c in l[:-1] ]
        self.width = len(lines[0]) - 1
        self.height = len(lines)

    def __getitem__(self, xy):
        return self.data[self.xy_to_i(*xy)]

    def find(self, c):
        return self.i_to_xy(next(filter(lambda t: t[1].c == c, zip(count(), self.data)))[0])

    def i_to_xy(self, i):
        y,x = divmod(i, self.width)
        return x,y

    def xy_to_i(self, x, y):
        return y * self.width + x

    def is_out_of_bounds(self, x, y):
        return not (0 <= x < self.width and 0 <= y < self.height)

    def is_too_steep(self, a, b):
        # backwards, so down only one
        return not (self[b].elevation() >= self[a].elevation() - 1)

    def is_farther(self, a, b):
        nd = self[a].distance + 1
        ed = self[b].distance
        return ed is not None and ed <= nd

    def should_update(self, a, b):
        return not self.is_out_of_bounds(*b) \
                and not self.is_too_steep(a, b) \
                and not self.is_farther(a, b)

    def update(self, a, b):
        if self.should_update(a, b):
            self[b].distance = self[a].distance + 1
            return b

    def print_distances(self):
        distances = [str(cell.distance) for cell in self.data]
        print("\n".join(("".join(distances[i:i+self.width]) for i in range(0, len(self.data), self.width))))


def grid_from_file(filename):
    with open(filename, 'rt') as f:
        return Grid(f.readlines())

def step_neighbour(grid, current_coord, direction):
    neighbour_coord = direction(*current_coord)
    return grid.update(current_coord, neighbour_coord)

def left(x, y): return x-1, y
def right(x, y): return x+1, y
def up(x, y): return x, y-1
def down(x, y): return x, y+1

def step(grid, current_coord):
    return list(filter(None,
           map(lambda direction: step_neighbour(grid, current_coord, direction),
               (left, right, up, down))))

def walk(grid):
    start = grid.find("E")
    grid[start].distance = 0
    coords_to_update = {start}
    while coords_to_update:
        current_coord = coords_to_update.pop()
        changed_coords = step(grid, current_coord)
        coords_to_update.update(changed_coords)


if __name__ == "__main__":
    import sys
    g = grid_from_file(sys.argv[1])
    walk(g)
    end = g.find('E')
    d = g[end].distance
    distances = [ cell.distance for cell in g.data if cell.distance is not None and cell.c == 'a']
    print(min(distances))

