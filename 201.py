#!/usr/bin/env python
from operator import attrgetter
from itertools import pairwise

class Node:
    def __init__(self, x):
        self.x = x
        self.next = None
        self.prev = None

    def move(self, n):
        #print(f'lst: {list_to_string(self)}')
        cur_prev = self.prev
        cur_next = self.next
        #step = attrgetter('prev' if self.x < 0 else 'next')
        step = attrgetter('next')
        tgt = self
        for i in range(self.x%n if self.x>=0 else self.x%n-1):
            tgt = step(tgt)
        if tgt is self:
            return

        #print(f'target: {tgt} {tgt.x}')
        cur_prev.next = cur_next
        cur_next.prev = cur_prev
        #if self.x > 0:
        self.prev = tgt
        self.next = tgt.next
        self.prev.next = self
        self.next.prev = self
        #elif self.x < 0:
        #    self.next = tgt
        #    self.prev = tgt.prev
        #    self.next.prev = self
        #    self.prev.next = self
        #print(f'lst: {list_to_string(self)}')
        #print(f'tgt: {list_to_string(tgt)}')


def list_to_string(node):
    lst = [node.x]
    head = node
    node = node.next
    while node is not head:
        lst.append(node.x)
        node = node.next
    return str(lst)

def list_from_file(filename):
    with open(filename, 'rt') as f:
        nodes = [Node(int(x)) for x in f.readlines()]

    def link(a, b):
        a.next = b
        b.prev = a

    for pair in pairwise(nodes):
        link(*pair)
    link(nodes[-1], nodes[0])
    return nodes

def xth(head, x):
    step = attrgetter('next')
    a = zero
    for i in range(x):
        a = step(a)
    return a

if __name__ == "__main__":
    import sys
    nodes = list_from_file(sys.argv[1])
    #print(f'lst: {list_to_string(nodes[0])}')
    for node in nodes: node.move(len(nodes))
    zero = next(filter(lambda n: n.x==0, nodes))
    # 1000th, 2000th, 3000th
    a = xth(zero, 1000%len(nodes))
    b = xth(zero, 2000%len(nodes))
    c = xth(zero, 3000%len(nodes))
    print(f'a={a.x}, b={b.x}, c={c.x}')
    print(f'sum: {sum(n.x for n in (a,b,c))}')
