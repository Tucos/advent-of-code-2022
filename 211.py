#!/usr/bin/env python
import operator

class Monkey:
    def __init__(self, name, value, oper=None, l=None, r=None):
        self.name = name
        self.value = value
        self.oper = oper
        self.l = l
        self.r = r
        self.dependents = []
        self.needed = [0,2][l is not None]

    def calculate(self, values):
        if self.l in values and self.r in values:
            self.value = self.oper(values[self.l].value, values[self.r].value)
            return True
        return False

    def __repr__(self):
        if self.value:
            return f'{self.name}: {self.value}'
        else:
            return f'{self.name}: {self.l} {self.oper.__name__} {self.r}'


def add_dep(m, value_monkeys, expr_monkeys):
    def _add_dep(t):
        try:
            o,r = value_monkeys[t], True
        except KeyError:
            o,r = expr_monkeys[t], False
        o.dependents.append(m)
        return r

    if l := _add_dep(m.l):
        m.needed -= 1
    if r := _add_dep(m.r):
        m.needed -= 1
    return l and r

def monkeys_from_file(filename):
    opers = {
        '+': operator.add,
        '-': operator.sub,
        '/': operator.floordiv,
        '*': operator.mul,
    }
    value_monkeys = {}
    expr_monkeys = {}
    with open(filename, 'rt') as f:
        for line in f:
            name, expr = line.split(': ')
            parts = expr.split()
            if len(parts) == 1:
                value_monkeys[name] = Monkey(name, int(expr))
            else:
                expr_monkeys[name] = Monkey(name, None, opers[parts[1]], parts[0], parts[2])

    ready = []
    for m in expr_monkeys.values():
        if add_dep(m, value_monkeys, expr_monkeys):
            ready.append(m)

    return value_monkeys, ready

def calculate(values, ready):
    while ready:
        m = ready.pop(0)
        m.calculate(value_monkeys)
        value_monkeys[m.name] = m

        for o in m.dependents:
            o.needed -= 1
            if not o.needed:
                ready.append(o)


if __name__ == "__main__":
    import sys
    value_monkeys, ready = monkeys_from_file(sys.argv[1])
    calculate(value_monkeys, ready)
    print(value_monkeys['root'])
