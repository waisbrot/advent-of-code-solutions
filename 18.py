#!/usr/bin/python3

from sys import call_tracing
import sys
from typing import Any, Iterable, List, Optional, Union

SnailPairItem = Union[int, 'SnailPair']
MaybeSnailPair = Optional['SnailPair']
class SnailPair:
    car: SnailPairItem
    cdr: SnailPairItem
    parent: MaybeSnailPair
    is_left: bool
    def __init__(self) -> None:
        pass

    def __repr__(self) -> str:
        return f'[{self.car},{self.cdr}]'

    @property
    def depth(self) -> int:
        if self.is_top:
            return 0
        else:
            return 1 + self.parent.depth

    def try_explode(self) -> bool:
        if self.depth == 4:
            print(f"Explodes: {self}")
            self.add_left(self.car)
            self.add_right(self.cdr)
            if self.is_left:
                self.parent.car = 0
            else:
                self.parent.cdr = 0
            return True
        else:
            return self.car.try_explode() or self.cdr.try_explode()

    @property
    def is_top(self) -> bool:
        return self.parent is None

    def add_left(self, number: int) -> None:
        if self.is_top:
            print("No left to explode into")
        elif self.is_left:
            print("Walking left")
            self.parent.add_left(number)
        elif isinstance(self.car, int):
            print("Add to right")
            self.car += number
        else:
            print("Walking right")
            self.car.add_right(number)

    def add_right(self, number: int) -> None:
        if self.is_top:
            print("No right to explode into")
            return
        elif not self.is_left:
            print("walking right")
            self.parent.add_right(number)
        elif isinstance(self.cdr, int):
            print("add to right")
            self.cdr += number
        else:
            print("walking left")
            self.cdr.add_left(number)

def main():
    data = read()
    parsed = list(parse(data))[0]
    print(f"Top pair: {parsed}")
    parsed.try_explode()
    print(f"After explode: {parsed}")

def read(filename: Optional[str] = None) -> List[str]:
    if filename is None:
        return sys.stdin.readlines()

    with open(filename) as fh:
        return fh.readlines()

def parse(data: List[str]) -> Iterable[SnailPair]:
    for line in data:
        [a, b] = eval(line)
        yield parse_pair(a, b, parent=None)


def parse_half_pair(half: Union[List,int], parent: SnailPair, is_left: bool) -> Union[int,SnailPair]:
    if isinstance(half, int):
        return half
    else:
        [a,b] = half
        return parse_pair(a,b, parent=parent, left_side=True)

def parse_pair(a: Union[List,int], b: Union[List,int], parent: MaybeSnailPair = None, left_side: bool = True) -> SnailPair:
    self = SnailPair()
    car = parse_half_pair(a, self, True)
    cdr = parse_half_pair(b, self, False)
    self.car = car
    self.cdr = cdr
    self.parent = parent
    self.is_left = left_side
    return self

def print_number(expressions: Iterable[SnailPair]):
    print('[')
    for e in expressions:
        print(e, end=',\n')
    print(']\n')


main()