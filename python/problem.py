import io
from typing import Any, Generator, TypeAlias
from contextlib import AbstractContextManager
from collections import UserDict

Coordinate: TypeAlias = tuple[int,int]
MCoordinate: TypeAlias = list[int,int]

class Grid(UserDict):
    maxes: Coordinate

    def __init__(self) -> None:
        super().__init__(self)
        self.maxes = (-1,-1)
    
    def __setitem__(self, key: Coordinate, item: str) -> None:
        if key[0] > self.maxes[0]:
            self.maxes = (key[0], self.maxes[1])
        if key[1] > self.maxes[1]:
            self.maxes = (self.maxes[0], key[1])
        super().__setitem__(key, item)

    def __getitem__(self, key: Coordinate) -> str:
        return super().__getitem__(key)
    
def manhattan_distance(a: Coordinate, b: Coordinate) -> int:
    ax,ay = a
    bx,by = b
    return abs(ax - bx) + abs(ay - by)

class ProblemBase(AbstractContextManager):
    def __init__(self) -> None:
        self.input = None
        pass
    
    def __exit__(self, exc_type, exc_value, traceback):
        if self.input:
            self.input.close()

    def set_filename(self, filename):
        assert not self.input
        self.input = open(filename)
    
    def set_string(self, string):
        assert not self.input, "Can't pass both filename and string"
        self.input = io.StringIO(string)

    def solve(self) -> str:
        pass

    def input_as_lines(self):
        for line in self.input:
            line = line.strip()
            if len(line) > 0:
                yield line

    def input_as_chargrid(self):
        grid = Grid()
        row = 0
        for line in self.input:
            line = line.strip()
            if len(line) == 0:
                continue
            col = 0
            for char in line:
                grid[(row,col)] = char
                col += 1
            row += 1
        return grid
