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

def count_open_sides(coords, coord):
    count = 0
    for dxyz in ((-1, 0, 0), (1, 0, 0),
                 (0, -1, 0), (0, 1, 0),
                 (0, 0, -1), (0, 0, 1)):
        adj = apply_delta(*coord, *dxyz)
        if adj not in coords:
            count += 1
    return count

def count_open_sides_total(coords):
    count = 0
    for c in coords:
        count += count_open_sides(coords, c)
    return count

if __name__ == "__main__":
    import sys
    coords = blocks_from_file(sys.argv[1])
    print(f'total: {count_open_sides_total(coords)}')
