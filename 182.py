#!/usr/bin/env python

def coord_from_line(line):
    return tuple(int(x) for x in line.split(','))

def blocks_from_file(filename):
    coords = set()
    with open(filename, 'rt') as f:
        for line in f:
            coords.add(coord_from_line(line))
    return coords

def apply_delta(x,y,z, dx,dy,dz):
    return (x+dx, y+dy, z+dz)

def count_open_sides(filled, coord):
    count = 0
    for dxyz in ((-1, 0, 0), (1, 0, 0),
                 (0, -1, 0), (0, 1, 0),
                 (0, 0, -1), (0, 0, 1)):
        adj = apply_delta(*coord, *dxyz)
        if adj in filled:
            count += 1
    return count

def count_open_sides_total(coords, filled):
    count = 0
    for c in coords:
        count += count_open_sides(filled, c)
    return count

def flood_outside(coords):
    assert (-1,-1,-1) not in coords

    limx = max(c[0] for c in coords) + 1
    limy = max(c[1] for c in coords) + 1
    limz = max(c[2] for c in coords) + 1

    stack = [(1,1,1)]
    filled = { (1,1,1) }
    while stack:
        coord = stack.pop(0)
        for dxyz in ((-1, 0, 0), (1, 0, 0),
                     (0, -1, 0), (0, 1, 0),
                     (0, 0, -1), (0, 0, 1)):
            adj = apply_delta(*coord, *dxyz)
            if -1 <= adj[0] <= limx and -1 <= adj[1] <= limy and -1 <= adj[2] <= limz:
                if adj not in coords and adj not in filled:
                    stack.append(adj)
                    filled.add(adj)
    return filled

if __name__ == "__main__":
    import sys
    coords = blocks_from_file(sys.argv[1])
    filled = flood_outside(coords)
    count = count_open_sides_total(coords, filled)
    print(f'total: {count}')
    # first guess: 2035, too low
