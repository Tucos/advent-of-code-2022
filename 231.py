#!/usr/bin/env python
from collections import defaultdict
from functools import reduce

def left(x, y): return x-1, y
def right(x, y): return x+1,y
def up_left(x, y): return x-1, y-1
def up(x, y): return x, y-1
def up_right(x, y): return x+1, y-1
def down_left(x, y): return x-1,y+1
def down(x, y): return x, y+1
def down_right(x, y): return x+1,y+1


def determine_target(pos, elves, directions):
    if all(move(*pos) not in elves for move in (up_left, up, up_right, left, right, down_left, down, down_right)):
        return None
    for preds, tgt_move in directions:
        if all(pred(*pos) not in elves for pred in preds):
            return tgt_move(*pos)
    return None


def grid_from_file(filename):
    elves = set()
    with open(filename, 'rt') as f:
        y = 0
        for line in f:
            x = 0
            for char in line:
                if char == '#':
                    elves.add((x,y))
                x += 1
            y += 1
    return elves

def exec_round(elves, directions):
    # each elf determines new position
    destinations = defaultdict(list)
    for elf in elves:
        target = determine_target(elf, elves, directions)
        #print(f'elf {elf} suggests {target}')
        if target is not None:
            destinations[target].append(elf)
        else:
            destinations[elf].append(elf) # safe, because no elf moves towards a target
    # each elf moves, if they were only one moving
    result = set()
    for destination, dest_elves in destinations.items():
        if len(dest_elves) == 1:
            #print(f'elf {dest_elves} moves to {target}')
            result.add(destination)
        else:
            for e in dest_elves:
                result.add(e)
    return result

def bbox(elves):
    def m4(acc, el):
        xmin, xmax, ymin, ymax = acc
        x, y = el
        return (min(xmin, x), max(xmax, x), min(ymin, y), max(ymax, y))

    it = iter(elves)
    x0,y0 = next(it)
    return reduce(m4, it, (x0, x0, y0, y0))


def print_grid(elves):
    xmin, xmax, ymin, ymax = bbox(elves)
    print('\n'.join(
        ''.join(
            '#' if (x,y) in elves else '.'
            for x in range(xmin, xmax+1)
        )
        for y in range(ymin, ymax+1)))

if __name__ == "__main__":
    import sys
    elves = grid_from_file(sys.argv[1])
    #print_grid(elves)
    directions = [((up_left, up, up_right), up),
                  ((down_left, down, down_right), down),
                  ((up_left, left, down_left), left),
                  ((up_right, right, down_right), right)]
    for iround in range(10):
        elves = exec_round(elves, directions)
        directions.append(directions.pop(0))
        #print(f'After round {iround+1}')
        #print_grid(elves)

    xmin, xmax, ymin, ymax = bbox(elves)
    print(f'empty tiles: {(xmax+1-xmin) * (ymax+1-ymin) - len(elves)}')
