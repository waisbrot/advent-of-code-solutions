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

    
class Problem14a(ProblemBase):
    map: Grid
    maxes: tuple[int,int]

    def __init__(self) -> None:
        super().__init__()

    def read_input(self) -> None:
        self.map = self.input_as_chargrid()
        self.maxes = (max([r for (r,_) in self.map.keys()]), max([c for (_,c) in self.map.keys()]))

    def tilt(self, tuple_primary: int, tuple_secondary: int, roll_range_args: tuple[int,int,int]) -> int:
        weight = 0
        for secondary in range(self.maxes[tuple_secondary]+1):
            for primary in range(self.maxes[tuple_primary]+1):
                coord = [-1,-1]
                coord[tuple_primary] = primary
                coord[tuple_secondary] = secondary
                if self.map[tuple(coord)] == 'O':
                    new_p = primary
                    new_coord = copy(coord)
                    for pr in range(primary + roll_range_args[0], roll_range_args[1], roll_range_args[2]):
                        new_coord[tuple_primary] = pr
                        if self.map[tuple(new_coord)] == '.':
                            new_p = pr
                        else:
                            break
                    new_coord[tuple_primary] = new_p
                    if new_coord != coord:
                        # logging.debug(f'move from {coord} to {new_coord}')
                        self.map[tuple(new_coord)] = 'O'
                        self.map[tuple(coord)] = '.'
                    weight += self.maxes[0]+1 - new_coord[0]
        return weight

    def solve1(self) -> str:
        logging.debug(f'before: {self}')
        weight = self.tilt(0, 1, (-1,-1,-1))
        logging.debug(f'after: {self}')
        return weight
    
    def solve2(self) -> str:
        self.tilt(0, 1, (-1, -1, -1)) # N
        logging.debug(f'after N: {self}')
        self.tilt(1, 0, (-1, -1, -1)) # W
        logging.debug(f'after W: {self}')
        self.tilt(0, 1, (1, self.maxes[0], 1)) # S
        logging.debug(f'after S: {self}')
        w = self.tilt(1, 0, (1, self.maxes[1], 1)) # E
        logging.debug(f'after E: {self}')
        return w

    def __repr__(self) -> str:
        s = ''
        r = 0
        while True:
            if (r,0) not in self.map:
                return s
            s += '\n'
            c = 0
            while True:
                if (r,c) not in self.map:
                    break
                s += self.map[(r,c)]
                c += 1
            r += 1
                

class Problem14b(Problem14a):
    pass

if __name__ == '__main__':
    with Problem14a() as p:
        p.set_filename(sys.argv[1])
        p.read_input()
        print(f'SOLUTION1: {p.solve1()}')
    with Problem14b() as p:
        p.set_filename(sys.argv[1])
        p.read_input()
        print(f'SOLUTION2: {p.solve2()}')
