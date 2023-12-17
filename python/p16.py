import sys
import re
import logging
from typing import Iterable, Self, TypeAlias
from python.problem import ProblemBase, Grid, Coordinate, MCoordinate, manhattan_distance
from collections import OrderedDict, UserDict, defaultdict
from enum import Enum
from copy import copy
from itertools import pairwise, combinations, groupby

logging.basicConfig(level=logging.INFO)

class Beam:
    location: Coordinate
    direction: Coordinate
    def __init__(self, location: Coordinate, direction: Coordinate) -> None:
        self.location = location
        self.direction = direction

    def same_direction(self, other: Self) -> bool:
        return self.direction == other.direction
    
    def advance(self) -> Self:
        return Beam((self.location[0] + self.direction[0], self.location[1] + self.direction[1]), self.direction)
    
    def split(self) -> [Self,Self]:
        one = (self.direction[1], self.direction[0])
        two = (one[0] * -1, one[1] * -1)
        return [
            Beam((self.location[0] + one[0], self.location[1] + one[1]), one),
            Beam((self.location[0] + two[0], self.location[1] + two[1]), two),
        ]
    
    def bounce(self, mirror: str) -> Self:
        b: Coordinate
        if self.direction[0] == 1:
            if mirror == '/':
                b = (0, -1)
            elif mirror == '\\':
                b = (0, 1)
        elif self.direction[0] == -1:
            if mirror == '/':
                b = (0, 1)
            elif mirror == '\\':
                b = (0, -1)
        elif self.direction[1] == 1:
            if mirror == '/':
                b = (-1, 0)
            elif mirror == '\\':
                b = (1, 0)
        elif self.direction[1] == -1:
            if mirror == '/':
                b = (1, 0)
            elif mirror == '\\':
                b = (-1, 0)
        new = Beam(self.location, b)
        return new.advance()
    
    def __str__(self) -> str:
        if self.direction[0] == 1:
            return 'V'
        elif self.direction[0] == -1:
            return '^'
        elif self.direction[1] == 1:
            return '>'
        elif self.direction[1] == -1:
            return '<'
        assert False, "fell through"

class LightSquare:
    mirror: str
    energized: bool
    beams: list[Beam]

    def __init__(self, mirror: str) -> None:
        self.mirror = mirror
        self.energized = False
        self.beams = []

    def add_beam(self, beam: Beam) -> Iterable[Beam]:
        self.energized = True
        if any([already.same_direction(beam) for already in self.beams]):
            return []
        self.beams.append(beam)
        if self.mirror == '.' or (self.mirror == '|' and beam.direction[0] != 0) or (self.mirror == '-' and beam.direction[1] != 0):
            new_beam = beam.advance()
            return [new_beam]
        elif self.mirror == '|' or self.mirror == '-':
            return beam.split()
        elif self.mirror == '/' or self.mirror == '\\':
            bounce = beam.bounce(self.mirror)
            return [bounce]
        
        assert False, "Fell through"

    def __str__(self) -> str:
        if self.mirror != '.':
            return self.mirror
        if len(self.beams) == 0:
            return '.'
        if len(self.beams) == 1:
            return str(self.beams[0])
        else:
            return str(len(self.beams))

class LightGrid(UserDict):
    def __init__(self, chargrid: Grid) -> None:
        super().__init__(self)
        for k,v in chargrid.items():
            self[k] = LightSquare(v)
    
    def __setitem__(self, key: Coordinate, item: LightSquare) -> None:
        super().__setitem__(key, item)

    def __getitem__(self, key: Coordinate) -> LightSquare:
        return super().__getitem__(key)
    
    def __repr__(self) -> str:
        s = ''
        r = -1
        while True:
            s += '\n'
            r += 1
            c = 0
            while (r,c) in self:
                s += str(self[(r,c)])
                c += 1
            if c == 0:
                return s

class Problem16a(ProblemBase):
    map: LightGrid
    queue: list[Beam] = []

    def __init__(self) -> None:
        super().__init__()

    def read_input(self) -> None:
        chargrid = self.input_as_chargrid()
        self.map = LightGrid(chargrid)
        self.queue.append(Beam((0,0), (0, 1)))

    def solve1(self) -> str:
        while len(self.queue) > 0:
            beam = self.queue.pop()
            self.queue.extend(self.advance_beam(beam))
        logging.debug(self.map)
        sum = 0
        for sq in self.map.values():
            if sq.energized:
                sum += 1
        return sum
    
    def advance_beam(self, beam: Beam) -> Iterable[Beam]:
        if beam.location in self.map:
            return self.map[beam.location].add_beam(beam)
        else:
            return []

class Problem16b(ProblemBase):
    best_start: Beam = Beam((0,0),(0,1))
    max_energy: int = 0
    chargrid: Grid

    def read_input(self) -> None:
        self.chargrid = self.input_as_chargrid()

    def solve2(self) -> str:
        starts = [Beam((0, n), (1, 0)) for n in range(self.chargrid.maxes[1]+1)]
        starts.extend([Beam((n, 0), (0, 1)) for n in range(self.chargrid.maxes[0]+1)])
        starts.extend([Beam((self.chargrid.maxes[0], n), (-1, 0)) for n in range(self.chargrid.maxes[1]+1)])
        starts.extend([Beam((n, self.chargrid.maxes[1]), (0, -1)) for n in range(self.chargrid.maxes[0]+1)])
        for start in starts:
            p = Problem16a()
            p.map = LightGrid(self.chargrid)
            p.queue = [start]
            score = int(p.solve1())
            if score > self.max_energy:
                self.max_energy = score
                self.best_start = start
        return f'Best start: {self.best_start.location}:{self.best_start} scores {self.max_energy}'
    
if __name__ == '__main__':
    with Problem16a() as p:
        p.set_filename(sys.argv[1])
        p.read_input()
        print(f'SOLUTION1: {p.solve1()}')
    with Problem16b() as p:
        p.set_filename(sys.argv[1])
        p.read_input()
        print(f'SOLUTION2: {p.solve2()}')
