import sys
import re
import logging
from typing import Iterable
from python.problem import ProblemBase, Grid
from collections import OrderedDict, defaultdict
from enum import Enum
from copy import copy
from itertools import pairwise

logging.basicConfig(level=logging.DEBUG)
    
class Problem10(ProblemBase):
    map: Grid
    def __init__(self) -> None:
        super().__init__()
        self.sequences = []

    def read_input(self) -> None:
        self.map = self.input_as_chargrid()

    def find_start(self) -> tuple[int,int]:
        for pos, sq in self.map.items():
            if sq == 'S':
                return pos
            
    def find_start_direction(self, start:tuple[int,int]) -> tuple[int,int]:
        sy,sx = start
        for d, valid in [
            ((sy, sx+1), ('J', '7', '-')),
            ((sy, sx-1), ('F', 'L', '-')),
        ]:
            if self.map[d] in valid:
                dy,dx = d
                return (dy-sy,dx-sx)
            
    def step(self, start:tuple[int,int], direction:tuple[int,int]) -> tuple[int,int]:
        sy,sx = start
        dy,dx = direction
        return (sy+dy, sx+dx)
    
    def unstep(self, current:tuple[int,int], origin:tuple[int,int]) -> tuple[int,int]:
        cy,cx = current
        oy,ox = origin
        return (cy-oy, cx-ox)
    
    def find_next(self, current:tuple[int,int], prior_direction:tuple[int,int]) -> tuple[int,int]:
        pipe = self.map[current]
        if pipe == '|':
            return prior_direction
        elif pipe == '-':
            return prior_direction
        elif pipe == 'L':
            if prior_direction == (1,0):
                return (0,1)
            else:
                return (-1,0)
        elif pipe == '7':
            if prior_direction == (0,1):
                return (1,0)
            else:
                return (0,-1)
        elif pipe == 'J':
            if prior_direction == (1,0):
                return (0,-1)
            else:
                return (-1,0)
        elif pipe == 'F':
            if prior_direction == (0,-1):
                return (1,0)
            else:
                return (0,1)
        else:
            return None

    def solve1(self) -> str:
        start = self.find_start()
        direction = self.find_start_direction(start)
        pos = self.step(start, direction)
        direction = self.find_next(pos, direction)
        count = 1
        while direction:
            pos = self.step(pos, direction)
            direction = self.find_next(pos, direction)
            count += 1
        return str(count/2)
    
    def find_path(self, start, start_direction) -> list[tuple[int,int]]:
        path = [start]
        pos = start
        direction = start_direction
        while direction:
            pos = self.step(pos, direction)
            path.append(pos)
            direction = self.find_next(pos, direction)
        return path
    
    def isolate_path(self, path: Iterable[tuple[int,int]]) -> None:
        path = set(path)
        for k in self.map.keys():
            if k not in path:
                self.map[k] = '.'

    def mark_side(self, pos:tuple[int,int], side1:tuple[int,int]) -> None:
        x,y = side1
        side2 = (-1*x, -1*y)
        a = self.step(pos, side1)
        if self.map[a] == '.':
            self.map[a] = '1'
        b = self.step(pos, side2)
        if self.map[b] == '.':
            self.map[b] == '2'

    def outline_path(self, path: Iterable[tuple[int,int]]) -> None:
        pairs = pairwise(path)
        (_,sx),(_,fx) = _,first = next(pairs)
        assert fx - sx == 1
        side1 = (-1,0)
        self.mark_side(first, side1)
        (ly,lx) = first
        for pos in pairs:
            x,y = pos


            
            

    def solve2(self) -> str:
        start = self.find_start()
        direction = self.find_start_direction(start)
        path = self.find_path(start, direction)
        self.isolate_path(path)
        return str(self)
    
    def __str__(self) -> str:
        s = '\n'
        yold = 0
        for (y,_),v in self.map.items():
            if y != yold:
                s += '\n'
                yold = y
            s += v
        return s

if __name__ == '__main__':
    with Problem10() as p:
        p.set_filename(sys.argv[1])
        p.read_input()
        print(f'SOLUTION1: {p.solve1()}')
    with Problem10() as p:
        p.set_filename(sys.argv[1])
        p.read_input()
        print(f'SOLUTION2: {p.solve2()}')
