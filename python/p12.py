import sys
import re
import logging
from typing import Iterable
from python.problem import ProblemBase, Grid, Coordinate, MCoordinate, manhattan_distance
from collections import OrderedDict, defaultdict
from enum import Enum
from copy import copy
from itertools import pairwise, combinations, groupby

logging.basicConfig(level=logging.DEBUG)
    
class Problem12(ProblemBase):
    solutions = 0

    def __init__(self) -> None:
        super().__init__()
        self.galaxies = []
        self.max_c = 0
        self.max_r = 0

    def read_input(self) -> None:
        for r, line in enumerate(self.input_as_lines()):
            for c, char in enumerate(line):
                if char == '#':
                    self.galaxies.append([r,c])
                    if c > self.max_c:
                        self.max_c = c
                    if r > self.max_r:
                        self.max_r = r

    def solve1(self) -> str:
        logging.debug(f'before expanding: {self}')
        self.expand_universe(1)
        logging.debug(f'after expanding: {self}')
        galaxies = self.find_galaxies()
        distances = [manhattan_distance(a,b) for a,b in combinations(galaxies, 2)]
        return sum(distances)
    
    def expand_universe(self, multiplier: int) -> None:
        # add rows
        last_row = 0
        add_rows = 0
        for galaxy in sorted(self.galaxies, key=lambda k: k[0]):
            if galaxy[0] > last_row+1:
                add_rows += (galaxy[0] - (last_row+1)) * multiplier
                logging.debug(f'Add a row: {galaxy} more than a row away from {last_row}. add_rows now is {add_rows}')
            last_row = galaxy[0]
            galaxy[0] += add_rows

        last_col = 0
        add_cols = 0
        for galaxy in sorted(self.galaxies, key=lambda k: k[1]):
            if galaxy[1] > last_col+1:
                add_cols += (galaxy[1] - (last_col+1)) * multiplier
            last_col = galaxy[1]
            galaxy[1] += add_cols

    def find_galaxies(self) -> list[Coordinate]:
        return self.galaxies
            
    def solve2(self) -> str:
        logging.debug(f'before expanding: {self}')
        self.expand_universe(2)
        logging.debug(f'after expanding: {self}')
        galaxies = self.find_galaxies()
        distances = [manhattan_distance(a,b) for a,b in combinations(galaxies, 2)]
        return sum(distances)
    
    def __str__(self) -> str:
        s = '\n'
        last_row = 0
        for g in sorted(self.galaxies):
            if g[0] != last_row:
                s += '\n'
                last_row = g[0]
            s += str(g) + ' '
        return s

if __name__ == '__main__':
    with Problem11() as p:
        p.set_filename(sys.argv[1])
        p.read_input()
        print(f'SOLUTION1: {p.solve1()}')
    with Problem11() as p:
        p.set_filename(sys.argv[1])
        p.read_input()
        print(f'SOLUTION2: {p.solve2()}')
