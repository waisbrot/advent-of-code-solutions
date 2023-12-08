import sys
import re
import logging
from python.problem import ProblemBase
from collections import OrderedDict, defaultdict
from enum import Enum

logging.basicConfig(level=logging.INFO)

class World:
    map: dict[str, (str, str)]
    pos: str
    def __init__(self, lines: list[str]) -> None:
        self.pos = 'AAA'
        self.map = {}
        for line in lines:
            m = re.match(r'(...) = \((...), (...)\)', line)
            assert m
            self.map[m.group(1)] = (m.group(2), m.group(3))

    def move(self, direction: str) -> str:
        fork = self.map[self.pos]
        if direction == 'L':
            self.pos = fork[0]
        elif direction == 'R':
            self.pos = fork[1]
        else:
            assert False, 'bad direction'
        return self.pos

class Problem8(ProblemBase):
    world: World
    directions: list[str]
    def __init__(self) -> None:
        super().__init__()

    def read_input(self) -> None:
        lines = list(self.input_as_lines())
        self.directions = lines[0]
        self.world = World(lines[1:])

    def solve1(self) -> str:
        steps = 0
        while self.world.pos != 'ZZZ':
            next_step = self.directions[steps % len(self.directions)]
            self.world.move(next_step)
            steps += 1
        return str(steps)

    def solve2(self) -> str:
        pass

if __name__ == '__main__':
    with Problem8() as p:
        p.set_filename(sys.argv[1])
        p.read_input()
        print(f'SOLUTION1: {p.solve1()}')
        print(f'SOLUTION2: {p.solve2()}')
