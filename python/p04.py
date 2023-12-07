import re
import functools
import sys
import logging
from python.problem import ProblemBase

logging.basicConfig(level=logging.DEBUG)

class Game:
    def __init__(self, line:str) -> None:
        (card, data) = line.split(': ')
        
        self.id = int(card[4:])

        (want, have) = data.split(' | ')
        self.want = set([int(n) for n in want.split(' ') if len(n) > 0])
        self.have = [int(n) for n in have.split(' ') if len(n) > 0]

        self.copies = 1

    def score(self) -> int:
        count = len([n for n in self.have if n in self.want])
        if count > 0:
            return pow(2, count - 1)
        else:
            return 0
        
    def count(self) -> int:
        return len([n for n in self.have if n in self.want])

class Problem4(ProblemBase):        
    def __init__(self) -> None:
        super().__init__()
        self.games:list[Game] = []

    def read_input(self) -> None:
        for line in self.input_as_lines():
            self.games.append(Game(line))

    def solve1(self) -> str:
        sum = 0
        for g in self.games:
            sum += g.score()
        return str(sum)

    def solve2(self) -> str:
        total = 0
        for (i, g) in enumerate(self.games):
            total += g.copies
            more = g.count()
            logging.debug(f'Game {g.id} ({i}): {g.copies} copies and affects up to {more} next games. range({i+1}, {i+more})')
            for j in range(i+1, i+1+more):
                if j >= len(self.games):
                    logging.debug(f'Index {j} off the end')
                    break
                self.games[j].copies += g.copies
        return str(total)

if __name__ == '__main__':
    with Problem4() as p:
        p.set_filename(sys.argv[1])
        p.read_input()
        print(f'SOLUTION1: {p.solve1()}')
        print(f'SOLUTION2: {p.solve2()}')
