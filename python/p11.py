import sys
import re
import logging
from typing import Iterable
from python.problem import ProblemBase, Grid, Coordinate, manhattan_distance
from collections import OrderedDict, defaultdict
from enum import Enum
from copy import copy
from itertools import pairwise, combinations

logging.basicConfig(level=logging.DEBUG)
    
class Problem11(ProblemBase):
    map: list[list[str]]

    def __init__(self) -> None:
        super().__init__()
        self.map = []

    def replace_char(self, c:str) -> str:
        return '-' if c == '.' else c

    def read_input(self) -> None:
        for line in self.input_as_lines():
            self.map.append([self.replace_char(c) for c in line])

    def solve1(self) -> str:
        self.expand_universe(1)
        galaxies = self.find_galaxies()
        distances = [manhattan_distance(a,b) for a,b in combinations(galaxies, 2)]
        return sum(distances)
    
    def expand_universe(self, multiplier: int) -> None:
        for c in range(len(self.map[0])-1, -1, -1):
            blank = True
            for r in range(len(self.map)-1, -1, -1):
                logging.debug(f'checking {r},{c}')
                if self.map[r][c] != '-':
                    blank = False
                    break
            if blank:
                logging.debug(f'expand column {c}')
                for r in range(len(self.map)-1, -1, -1):
                    for _ in range(multiplier):
                        self.map[r].insert(c+1, '-')
        for r in range(len(self.map)-1, -1, -1):
            if all([c == '-' for c in self.map[r]]):
                for _ in range(multiplier):
                    self.map.insert(r+1, copy(self.map[r]))

    def find_galaxies(self) -> list[tuple[int,int]]:
        found = []
        for r in range(len(self.map)):
            for c in range(len(self.map[0])):
                if self.map[r][c] == '#':
                    found.append((r,c))
        return found
            
    def solve2(self) -> str:
        self.expand_universe(1000000)
        galaxies = self.find_galaxies()
        distances = [manhattan_distance(a,b) for a,b in combinations(galaxies, 2)]
        return sum(distances)
    
    def __str__(self) -> str:
        return '\n' + '\n'.join([''.join(row) for row in self.map])

if __name__ == '__main__':
    with Problem11() as p:
        p.set_filename(sys.argv[1])
        p.read_input()
        print(f'SOLUTION1: {p.solve1()}')
    with Problem11() as p:
        p.set_filename(sys.argv[1])
        p.read_input()
        print(f'SOLUTION2: {p.solve2()}')
