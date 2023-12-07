import io
from typing import Any, Generator
from contextlib import AbstractContextManager
from collections import UserDict

class Grid(UserDict):
    def __init__(self) -> None:
        super().__init__(self)
        self.grid = {}
    
    def __setitem__(self, key: tuple[int,int], item: str) -> None:
        super().__setitem__(key, item)

    def __getitem__(self, key: tuple[int,int]) -> str:
        return super().__getitem__(key)

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
