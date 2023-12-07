import sys
import re
import logging
from python.problem import ProblemBase
from collections import OrderedDict

logging.basicConfig(level=logging.INFO)

class Race:
    def __init__(self, time: int, distance: int) -> None:
        self.time = time
        self.distance = distance

    def passing_hold_times(self) -> list[int]:
        times:list[int] = []
        for hold_time in range(1, self.time - 1):
            remaining = self.time - hold_time
            travel = remaining * hold_time
            if travel > self.distance:
                times.append(travel)
        logging.debug(f'Passing hold times: {times}')
        return times
    
    def first_passing_hold_time(self) -> int:
        for hold_time in range(1, self.time - 1):
            remaining = self.time - hold_time
            travel = remaining * hold_time
            if travel > self.distance:
                return hold_time
        assert False, f'Could not find a first hold time'

    def last_passing_hold_time(self) -> int:
        for hold_time in range(self.time - 1, 1, -1):
            remaining = self.time - hold_time
            travel = remaining * hold_time
            if travel > self.distance:
                return hold_time
        assert False, f'Could not find a first hold time'

class Problem5(ProblemBase):        
    def __init__(self) -> None:
        super().__init__()
        self.races: list[Race] = []

    def read_input1(self) -> None:
        (times_str, distances_str) = self.input_as_lines()
        times = [int(s) for s in re.split(r'\s+', times_str) if s.isdigit()]
        distances = [int(s) for s in re.split(r'\s+', distances_str) if s.isdigit()]
        self.races = [Race(t, d) for (t, d) in zip(times, distances)]

    def read_input2(self) -> None:
        (times_str, distances_str) = self.input_as_lines()
        time = int(times_str[5:].replace(' ',''))
        distance = int(distances_str[9:].replace(' ', ''))
        self.races = [Race(time, distance)]
 
    def solve1(self) -> str:
        product = 1
        for race in self.races:
            product *= len(race.passing_hold_times())
        return str(product)

    def solve2(self) -> str:
        first = self.races[0].first_passing_hold_time()
        last = self.races[0].last_passing_hold_time()
        return str(last - first + 1)

if __name__ == '__main__':
    with Problem5() as p:
        p.set_filename(sys.argv[1])
        p.read_input1()
        print(f'SOLUTION1: {p.solve1()}')
        p.input = None
        p.set_filename(sys.argv[1])
        p.read_input2()
        print(f'SOLUTION2: {p.solve2()}')
