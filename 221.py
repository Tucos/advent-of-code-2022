#!/usr/bin/env python
from operator import attrgetter
from itertools import takewhile

class Cell:
    def __init__(self, c, x, y):
        self.c = c
        self.left = None
        self.right = None
        self.up = None
        self.down = None
        self.x = x
        self.y = y

    def __repr__(self):
        return f'{self.x},{self.y}'

class Direction:
    def __init__(self, attr, score):
        self.attr = attr
        self.ag = attrgetter(attr)
        self.cw = None
        self.ccw = None
        self.score = score

    def setr(self, cw, ccw):
        self.cw = cw
        self.ccw = ccw

    def __call__(self, *args, **kwargs):
        return self.ag(*args, **kwargs)

    def __repr__(self):
        return f'{self.attr}'

left = Direction('left', 2)
right = Direction('right', 0)
up = Direction('up', 3)
down = Direction('down', 1)
left.setr(up, down)
right.setr(down, up)
up.setr(right, left)
down.setr(left, right)


def move(n):
    def _move(cell, direction):
        for _ in range(n):
            tgt = direction(cell)
            if tgt.c == '#':
                break
            else:
                cell = tgt
        return cell, direction
    return _move

def rotate(d):
    def _rotate(cell, direction):
        return cell, [direction.cw, direction.ccw][d=='L']
    return _rotate


def grid_from_lines(lines):
    grid = []
    u = {}
    first_y_in_column = {}
    last_y_in_column = {}
    y = 0
    for l in lines:
        y += 1
        l = l[:-1]
        row = {}
        left = None
        first = None
        x = 0
        for c in l:
            x += 1
            if c == ' ':
                continue
            row[x] = n = Cell(c, x, y)
            if left:
                n.left = left
                left.right = n
            if x in u:
                n.up = u[x]
                u[x].down = n
            if not first:
                first = n
            if x not in first_y_in_column:
                first_y_in_column[x] = y
            last_y_in_column[x] = y
            left = n

        first.left = n
        n.right = first
        u = row
        grid.append(row)
    for x in last_y_in_column:
        grid[last_y_in_column[x]-1][x].down = grid[first_y_in_column[x]-1][x]
        grid[first_y_in_column[x]-1][x].up = grid[last_y_in_column[x]-1][x]

    return grid, grid[0][min(grid[0])]

def actions(l):
    result = []
    it = iter(l)
    try:
        while True:
            try:
                dgts = []
                nxt = next(it)
                while nxt.isdigit():
                    dgts.append(nxt)
                    nxt = next(it)
                k = int(''.join(dgts))
                result.append(move(k))
            except ValueError:
                pass
            result.append(rotate(nxt))
    except StopIteration:
        pass
    return result


def read_file(filename):
    with open(filename, 'rt') as f:
        lines = f.readlines()
    g, start = grid_from_lines(lines[:-2])
    a = actions(lines[-1][:-1])
    return g, start, a


def walk(start, actions):
    cell, direction = start, right
    for a in actions:
        #s1 = f'{a}({cell}, {direction} ->'
        cell, direction = a(cell, direction)
        #print(f'{s1} -> {cell}, {direction}')
    return cell, direction


def score(cell, direction):
    return 1000 * cell.y + 4 * cell.x + direction.score

if __name__ == '__main__':
    import sys
    g, start, actions = read_file(sys.argv[1])
    print(f'score: {score(*walk(start, actions))}')
