import sys
import re
import logging
from typing import Iterable
from python.problem import ProblemBase
from collections import OrderedDict, defaultdict
from enum import Enum
from copy import copy

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

    def possible_starts(self) -> Iterable[str]:
        for k in self.map.keys():
            if k.endswith('A'):
                yield k

    def move(self, direction: str) -> str:
        fork = self.map[self.pos]
        if direction == 'L':
            self.pos = fork[0]
        elif direction == 'R':
            self.pos = fork[1]
        else:
            assert False, 'bad direction'
        return self.pos
    
    def done(self) -> bool:
        return self.pos.endswith('Z')
    
class Problem8(ProblemBase):
    worlds: list[World]
    directions: list[str]
    def __init__(self) -> None:
        super().__init__()

    def read_input(self) -> None:
        lines = list(self.input_as_lines())
        self.directions = lines[0]
        map = lines[1:]
        world = World(map)
        self.worlds = []
        for start in world.possible_starts():
            w = copy(world)
            w.pos = start
            self.worlds.append(w)

    def solve1(self) -> str:
        for i, w in enumerate(self.worlds):
            steps = 0
            while not w.done():
                next_step = self.directions[steps % len(self.directions)]
                w.move(next_step)
                steps += 1
            logging.info(f'world {i} done in {steps} steps')
            while not w.done():
                next_step = self.directions[steps % len(self.directions)]
                w.move(next_step)
                steps += 1
            logging.info(f'world {i} done in {steps} steps')
            while not w.done():
                next_step = self.directions[steps % len(self.directions)]
                w.move(next_step)
                steps += 1
            logging.info(f'world {i} done in {steps} steps')

    def solve2(self) -> str:
        pass

if __name__ == '__main__':
    with Problem8() as p:
        p.set_filename(sys.argv[1])
        p.read_input()
        print(f'SOLUTION1: {p.solve1()}')
        print(f'SOLUTION2: {p.solve2()}')
