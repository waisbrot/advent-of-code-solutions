import sys
import re
import logging
from typing import Iterable
from python.problem import ProblemBase
from collections import OrderedDict, defaultdict
from enum import Enum
from copy import copy

logging.basicConfig(level=logging.DEBUG)

class Sequence:
    layers: list[list[int]]
    def __init__(self, line: str) -> None:
        self.layers = [[int(n) for n in line.split(' ')]]
        #
        # logging.debug(f'Read starting seq {self.layers} from {line}')

    def predict(self) -> int:
        self.build_differences()
        for i in range(len(self.layers)-1, 0, -1):
            bottom = self.layers[i]
            above = self.layers[i-1]
            above.append(bottom[-1] + above[-1])
            logging.debug(f'Set layer {i-1} to end with {above[-1]}')
        
        return self.layers[0][-1]
    
    def build_differences(self) -> None:
        all_zero = False
        current_layer_index = 0
        while not all_zero:
            current_layer = self.layers[current_layer_index]
            logging.debug(f'Current layer {current_layer_index}: {current_layer}')
            new_layer = []
            all_zero = True
            for i in range(1, len(current_layer)):
                diff = current_layer[i] - current_layer[i-1]
                if diff != 0:
                    all_zero = False
                new_layer.append(diff)
            self.layers.append(new_layer)
            current_layer_index += 1

    def rpredict(self) -> int:
        self.build_differences()
        for i in range(len(self.layers)-2, 0, -1):
            bottom = self.layers[i]
            above = self.layers[i-1]
            new = above[0] - bottom[0]
            logging.debug(f'{above[0]} - {bottom[0]} of {above} = {new}')
            above.insert(0, new)
            logging.debug(f'Set layer {i-1} to end with {above[0]} ({above})')
        
        return self.layers[0][0]
    
    def __repr__(self) -> str:
        return self.layers.__repr__()
    
class Problem9(ProblemBase):
    sequences: list[Sequence]

    def __init__(self) -> None:
        super().__init__()
        self.sequences = []

    def read_input(self) -> None:
        for line in self.input_as_lines():
            self.sequences.append(Sequence(line))
        logging.debug(f'Read {self.sequences}')

    def solve1(self) -> str:
        sum = 0
        for seq in self.sequences:
            sum += seq.predict()
        return str(sum)

    def solve2(self) -> str:
        sum = 0
        for seq in self.sequences:
            sum += seq.rpredict()
        return str(sum)

if __name__ == '__main__':
    with Problem9() as p:
        p.set_filename(sys.argv[1])
        p.read_input()
        print(f'SOLUTION1: {p.solve1()}')
    with Problem9() as p:
        p.set_filename(sys.argv[1])
        p.read_input()
        print(f'SOLUTION2: {p.solve2()}')
