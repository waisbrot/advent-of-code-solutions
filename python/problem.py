import io
from typing import Generator
from contextlib import AbstractContextManager

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
            yield line.strip()
