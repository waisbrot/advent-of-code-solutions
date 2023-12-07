import re
import functools
import sys
import logging
from python.problem import ProblemBase
from collections import UserDict

logging.basicConfig(level=logging.DEBUG)

class Iteration(UserDict):
    def __init__(self, iter_str: str) -> None:
        super().__init__()
        for color_bit in iter_str.split(', '):
            m = re.match(r'(\d+) (red|green|blue)', color_bit)
            assert m, f'Failed to match {color_bit}'
            color = m.group(2)
            count = int(m.group(1))
            logging.debug(f'parsed {color_bit} to color={color} count={count}')
            assert color not in self, f'Duplicate color {color}'
            self[color] = count

    def __str__(self) -> str:
        return ', '.join([f'{count} {color}' for (color, count) in self.items()])
    
    def is_possible(self, limits: dict) -> bool:
        for (color, count) in limits.items():
            if color in self:
                if self[color] <= count:
                    pass
                else:
                    logging.debug(f'Not possible: saw {color}={self[color]} but max is {count}')
                    return False
            else:
                pass
        return True
    
    def minimal(self) -> dict:
        result = {}
        for color in ('red', 'green', 'blue'):
            if color in self:
                result[color] = self[color]
            else:
                result[color] = 0
        return result

class Game:
    def __init__(self, game_str: str) -> None:
        self.id = id
        self.iterations: list[Iteration] = []

        m = re.match(r'Game (\d+): (.*)$', game_str)
        assert m, f'Failed to parse game {game_str}'
        self.id = int(m.group(1))
        iter_str = m.group(2)

        for iter in iter_str.split('; '):
            self.append(Iteration(iter))

    def append(self, iteration: Iteration) -> None:
        self.iterations.append(iteration)

    def is_possible(self, limits: dict) -> bool:
        for index, iter in enumerate(self.iterations):
            if iter.is_possible(limits):
                pass
            else:
                logging.debug(f'Iteration {index} is not possible')
                return False
        return True
    
    def minimal(self) -> dict:
        def minimize(a:dict, b:dict) -> dict:
            result = {}
            logging.debug(f'minimize({a}, {b})')
            for color in ('red', 'green', 'blue'):
                result[color] = max(a[color], b[color])
            return result
        return functools.reduce(minimize, [i.minimal() for i in self.iterations], {'red':0, 'green':0, 'blue':0})

    def __str__(self) -> str:
        return f'Game {self.id}: {"; ".join([str(i) for i in self.iterations])}'
    
    def __repr__(self) -> str:
        return self.__str__()

class Problem2(ProblemBase):        
    def __init__(self) -> None:
        super().__init__()
        self.games = []

    def read_input(self) -> None:
        for line in self.input_as_lines():
            self.parse_and_store(line)

    def parse_and_store(self, line: str) -> None:
        self.games.append(Game(line))

    def solve1(self) -> str:
        self.read_input()
        logging.debug('Read input:\n\t' + "\n\t".join([str(g) for g in self.games]))
        constraint = {'red': 12, 'green': 13, 'blue': 14}
        sum = 0
        for game in self.games:
            if game.is_possible(constraint):
                logging.debug(f'Game {game.id} is possible')
                sum += game.id
            else:
                logging.debug(f'Game {game.id} is not possible')
        return str(sum)
    
    def solve2(self) -> str:
        self.read_input()
        logging.debug('Read input:\n\t' + "\n\t".join([str(g) for g in self.games]))
        sum = 0
        for game in self.games:
            minimal = game.minimal()
            logging.debug(f'Minimal counts for game {game.id}: {minimal}')
            sum += minimal['red'] * minimal['green'] * minimal['blue']
        return sum   

if __name__ == '__main__':
    with Problem2() as p:
        p.set_filename(sys.argv[1])
        print(f'SOLUTION1: {p.solve1()}')
        print(f'SOLUTION2: {p.solve2()}')
