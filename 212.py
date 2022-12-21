#!/usr/bin/env python
import operator

def inv_oper_l(op):
    return {
        # n = l + ?  =>  ? = n - l
        operator.add: lambda l, n: n - l,
        # n = l - ?  =>  ? = l - n
        operator.sub: lambda l, n: l - n,
        # n = l * ?  =>  ? = n / l
        operator.mul: lambda l, n: n / l,
        # n = l / ?  =>  ? = l / n
        operator.floordiv: lambda l, n: l / n,
    }[op]

def inv_oper_r(op):
    return {
        # n = ? + r  =>  ? = n - r
        operator.add: lambda r, n: n - r,
        # n = ? - r  =>  ? = n + r
        operator.sub: lambda r, n: n + r,
        # n = ? * r  =>  ? = n / r
        operator.mul: lambda r, n: n / r,
        # n = ? / r  =>  ? = n * r
        operator.floordiv: lambda r, n: n * r,
    }[op]


class Monkey:
    def __init__(self, name, value, oper=None, l=None, r=None):
        self.name = name
        self.value = value
        self.oper = oper
        self.l = l
        self.r = r
        self.dependents = []
        self.needed = [0,2][r is not None]

    def calculate(self, values):
        if self.l in values and self.r in values:
            self.value = self.oper(values[self.l].value, values[self.r].value)
            return True
        return False

    def calculate_needed(self, target_value, values):
        # root: pppw = sjmn  sjmn=150
        # pppw: target_value = 150,  cczh / lfqf
        if self.l in values:
            inv_oper = inv_oper_l(self.oper)
            m = self.r
            o = values[self.l].value
        else:
            inv_oper = inv_oper_r(self.oper)
            m = self.l
            o = values[self.r].value
        return m, inv_oper(o, target_value)

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
            try:
                o,r = expr_monkeys[t], False
            except KeyError:
                return False
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
            if name == 'humn':
                expr_monkeys[name] = Monkey(name, None, operator.add, 'x', 'x')
            else:
                parts = expr.split()
                if len(parts) == 1:
                    value_monkeys[name] = Monkey(name, int(expr))
                else:
                    expr_monkeys[name] = Monkey(name, None, opers[parts[1]], parts[0], parts[2])

    ready = []
    for m in expr_monkeys.values():
        if add_dep(m, value_monkeys, expr_monkeys):
            ready.append(m)

    return value_monkeys, ready, expr_monkeys

def calculate(values, ready):
    while ready:
        m = ready.pop(0)
        s1 = str(m)
        m.calculate(value_monkeys)
        #print(f'{s1} -> {m}')
        value_monkeys[m.name] = m

        for o in m.dependents:
            o.needed -= 1
            if not o.needed and o.name != 'root':
                ready.append(o)


if __name__ == "__main__":
    import sys
    value_monkeys, ready, expr_monkeys = monkeys_from_file(sys.argv[1])
    calculate(value_monkeys, ready)

    # root should equal
    root = expr_monkeys['root']
    if root.l in value_monkeys:
        unk = root.r
        needed = value_monkeys[root.l].value
    else:
        unk = root.l
        needed = value_monkeys[root.r].value
    while unk != 'humn':
        unk, needed = expr_monkeys[unk].calculate_needed(needed, value_monkeys)
    print(f'{unk}: {needed}')

