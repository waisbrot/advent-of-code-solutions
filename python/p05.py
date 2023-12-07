import sys
import re
import logging
from python.problem import ProblemBase
from collections import OrderedDict
import itertools

logging.basicConfig(level=logging.INFO)

class RangeMap:
    def __init__(self) -> None:
        self.data = {}

    def add_line(self, line:str) -> None:
        m = re.match(r'(\d+) (\d+) (\d+)', line)
        assert m, f'Failed to match line "{line}"'
        dest, src, count = int(m.group(1)), int(m.group(2)), int(m.group(3))
        self.data[src] = (count, dest)

    def finish_read(self) -> None:
        self.data = dict(sorted(self.data.items()))

    def __repr__(self) -> str:
        return self.data.__repr__()
    
    def __getitem__(self, key:int) -> int:
        logging.debug(f'lookup {key} in {self.data}')
        for start, (count, dstart) in self.data.items():
            if key < start:
                logging.debug(f'{start} < {key}')
                return key
            elif key >= start and key <= start+count-1:
                logging.debug(f'{key} >= {start} and {key} <= {start+count-1}')
                return key - start + dstart
        logging.debug('Off the end of the map')
        return key
    
class ReverseRangeMap(RangeMap):
    def finish_read(self) -> None:
        for src, (count, dest) in self.data.items()

class Problem5a(ProblemBase):        
    def __init__(self) -> None:
        super().__init__()
        self.seeds: list[int] = []
        self.seed_soil = RangeMap()
        self.soil_fertilizer = RangeMap()
        self.fertilizer_water = RangeMap()
        self.water_light = RangeMap()
        self.light_temperature = RangeMap()
        self.temperature_humidity = RangeMap()
        self.humidity_location = RangeMap()

    def read_input(self) -> None:
        current_map:RangeMap|None = None
        for line in self.input_as_lines():
            if line.startswith('seeds:'):
                self.seeds = sorted([int(s) for s in line.split(' ') if s != 'seeds:'])
            elif line.startswith('seed-'):
                current_map = self.seed_soil
            elif line.startswith('soil-'):
                current_map = self.soil_fertilizer
            elif line.startswith('fertilizer-'):
                current_map = self.fertilizer_water
            elif line.startswith('water-'):
                current_map = self.water_light
            elif line.startswith('light-'):
                current_map = self.light_temperature
            elif line.startswith('temperature-'):
                current_map = self.temperature_humidity
            elif line.startswith('humidity-'):
                current_map = self.humidity_location
            else:
                current_map.add_line(line)
        for map in (self.seed_soil, self.soil_fertilizer, self.fertilizer_water, self.water_light, self.light_temperature, self.temperature_humidity, self.humidity_location):
            map.finish_read()

    def chain_deref(self, seed:int) -> int:
        soil = self.seed_soil[seed]
        fert = self.soil_fertilizer[soil]
        wate = self.fertilizer_water[fert]
        ligh = self.water_light[wate]
        temp = self.light_temperature[ligh]
        humi = self.temperature_humidity[temp]
        loca = self.humidity_location[humi]
        return loca
    
    def solve1(self) -> str:
        min_seed = self.seeds[0]
        min_loc = self.chain_deref(self.seeds[0])
        for seed in self.seeds[1:]:
            loc = self.chain_deref(seed)
            if loc < min_loc:
                min_seed = seed
                min_loc = loc
        return str(min_loc)

class Problem5b(Problem5a):
    def read_input(self) -> None:
        super().read_input()
        self.seed_ranges = itertools.batched(self.seeds, 2)

    def solve2(self) -> str:
        return ''

if __name__ == '__main__':
    with Problem5a() as p:
        p.set_filename(sys.argv[1])
        p.read_input()
        print(f'SOLUTION1: {p.solve1()}')
    with Problem5b() as p:
        p.set_filename(sys.argv[1])
        p.read_input()
        print(f'SOLUTION2: {p.solve2()}')
