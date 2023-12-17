import sys
import re
import logging
from typing import Iterable, TypeAlias
from python.problem import ProblemBase, Grid, Coordinate, MCoordinate, manhattan_distance
from collections import OrderedDict, defaultdict
from enum import Enum
from copy import copy
from itertools import pairwise, combinations, groupby

logging.basicConfig(level=logging.DEBUG)

def hash(string: str) -> int:
    value = 0
    for c in string:
        value += ord(c)
        value *= 17
        value = value % 256
    return value

    
class Problem15a(ProblemBase):
    strings: list[str] = []

    def __init__(self) -> None:
        super().__init__()

    def read_input(self) -> None:
        for line in self.input_as_lines():
            for string in line.split(','):
                self.strings.append(string)

    def solve1(self) -> str:
        return sum([hash(h) for h in self.strings])
                
class Command:
    label: str
    hash: int
    action: str
    length: int
    def __init__(self, string: str) -> None:
        m = re.search(r'^([a-z]+)(=|-)([0-9]+)?$', string)
        assert m, f'Failed to match {string}'
        self.label = m.group(1)
        self.hash = hash(self.label)
        self.action = m.group(2)
        if self.action == '=':
            self.length = int(m.group(3))
        else:
            assert self.action == '-'
            assert not m.group(3)

Lens: TypeAlias = tuple[str,int]
Box: TypeAlias = Iterable[Lens]

def remove_label(box: Box, label: str) -> Box:
    removed = False
    for lens in box:
        if not removed:
            if lens[0] == label:
                removed = True
                continue
        yield lens

def add_label(box: Box, label: str, value: int) -> Box:
    replaced = False
    for lens in box:
        if replaced:
            yield lens
        elif lens[0] == label:
            replaced = True
            yield (label, value)
        else:
            yield lens
    if not replaced:
        yield (label, value)


class Problem15b(ProblemBase):
    commands: list[Command] = []
    boxes: list[Box] = [[] for _ in range(256)]

    def read_input(self) -> None:
        for line in self.input_as_lines():
            for string in line.split(','):
                self.commands.append(Command(string))

    def solve2(self) -> str:
        for command in self.commands:
            box = self.boxes[command.hash]
            if command.action == '-':
                box = remove_label(box, command.label)
            else:
                box = add_label(box, command.label, command.length)
            self.boxes[command.hash] = box

        sum = 0
        for i,box in enumerate(self.boxes):
            for j,lens in enumerate(box):
                sum += (i+1) * (j+1) * lens[1]
        
        return str(sum)
    
if __name__ == '__main__':
    with Problem15a() as p:
        p.set_filename(sys.argv[1])
        p.read_input()
        print(f'SOLUTION1: {p.solve1()}')
    with Problem15b() as p:
        p.set_filename(sys.argv[1])
        p.read_input()
        print(f'SOLUTION2: {p.solve2()}')
