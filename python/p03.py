import re
import functools
import sys
import logging
from python.problem import ProblemBase

logging.basicConfig(level=logging.DEBUG)

class Problem3(ProblemBase):        
    def __init__(self) -> None:
        super().__init__()

    def read_input(self) -> None:
        self.grid = self.input_as_chargrid()

    def adjacent_numbers(self, pos: tuple[int,int]) -> list[int]:
        logging.debug(f'finding adjacent numbers to {pos}')
        digit_locations = []
        for offset in [(-1,-1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]:
            check_pos = (pos[0] + offset[0], pos[1] + offset[1])
            if check_pos not in self.grid:
                continue
            if self.grid[check_pos].isdigit():
                digit_locations.append(check_pos)
        logging.debug(f'adjacent digits are at {digit_locations}')
        numbers = []
        while len(digit_locations) > 0:
            loc = (r,c) = digit_locations.pop()
            number_str = self.grid[loc]
            logging.debug(f'expand {loc} from {number_str} into a full number')
            offset = 1
            before = (r, c - offset)
            while before in self.grid:
                logging.debug(f'looking at {before}')
                if not self.grid[before].isdigit():
                    logging.debug(f'digit {loc} ends at {offset} before')
                    break
                if before in digit_locations:
                    logging.debug(f'This position was also adjacent to the symbol; removing that record')
                    digit_locations.remove(before)
                number_str = self.grid[before] + number_str
                logging.debug(f'New number so far: {number_str}')
                offset += 1
                before = (r, c - offset)
            offset = 1
            after = (r, c + offset)
            while after in self.grid:
                logging.debug(f'looking at {after}')
                if not self.grid[after].isdigit():
                    logging.debug(f'digit {loc} ends at {offset} after')
                    break
                try:
                    digit_locations.remove(after)
                except ValueError:
                    pass
                number_str = number_str + self.grid[after]
                logging.debug(f'New number so far: {number_str}')
                offset += 1
                after = (r, c + offset)
            numbers.append(int(number_str))
        return numbers

    def solve1(self) -> str:
        def is_symbol(s:str) -> bool:
            assert s != ' '
            return (not s.isdigit()) and s != '.'
        self.read_input()
        logging.debug('Read input:\n' + str(self.grid))
        symbol_positions = [kv[0] for kv in self.grid.items() if is_symbol(kv[1])]
        numbers = []
        for pos in symbol_positions:
            numbers.extend(self.adjacent_numbers(pos))

        return functools.reduce(lambda x,y: x+y, numbers)
    
    def solve2(self) -> str:
        def maybe_gear(s:str) -> bool:
            return s == '*'
        self.read_input()
        logging.debug('Read input:\n' + str(self.grid))
        maybe_gear_positions = [kv[0] for kv in self.grid.items() if maybe_gear(kv[1])]
        numbers = []
        for pos in maybe_gear_positions:
            adjacent = self.adjacent_numbers(pos)
            if len(adjacent) == 2:
                logging.debug(f'Found a gear at {pos} with numbers {adjacent[0]} and {adjacent[1]}')
                numbers.append(adjacent[0] * adjacent[1])

        return functools.reduce(lambda x,y: x+y, numbers)

if __name__ == '__main__':
    with Problem3() as p:
        p.set_filename(sys.argv[1])
        #print(f'SOLUTION1: {p.solve1()}')
        print(f'SOLUTION2: {p.solve2()}')
