import re
import functools
import sys
import logging
from python.problem import ProblemBase

logging.basicConfig(level=logging.DEBUG)
WORDS_TO_NUMBERS = {
    'one': '1',
    'two': '2',
    'three': '3',
    'four': '4',
    'five': '5',
    'six': '6',
    'seven': '7',
    'eight': '8',
    'nine': '9',
}

class Problem1(ProblemBase):
    def __init__(self) -> None:
        super().__init__()
        self.lines = []

    def read_input(self) -> None:
        for line in self.input_as_lines():
            number = self.parse_line(line)
            logging.debug(f'Appending {number}')
            self.lines.append(int(number))

    def parse_line(self, line: str) -> int:
        logging.debug(f'Parse line "{line}"')
        line = self.replace_words(line)
        line = self.strip_letters(line)
        return line[0] + line[-1]

    def replace_words(self, line:str) -> str:
        words_disjunction = '|'.join(WORDS_TO_NUMBERS.keys())
        regex = words_disjunction
        m = re.search(regex, line)
        if m:
            word = m.group(0)
            line = line.replace(word, WORDS_TO_NUMBERS[word]+word, 1)
            logging.debug(f'Replaced 1st {word}: {line}')
        else:
            logging.debug(f'No first word in {line} with {regex}')
        regex = f'.*({words_disjunction})'
        m = re.search(regex, line)
        if m:
            word = m.group(1)
            line = line.replace(word, word+WORDS_TO_NUMBERS[word])
            logging.debug(f'Replaced last {word}: {line}')
        else:
            logging.debug(f'No last word in {line} with {regex}')
        return line

    def strip_letters(self, line) -> str:
        line = re.sub(r'[^0-9]', '', line)
        logging.info(f'stripped: "{line}"')
        return line


    def solve(self) -> str:
        self.read_input()
        logging.debug(f'Summation of {self.lines}')
        return functools.reduce(lambda x, y: x+y, self.lines)
    
if __name__ == '__main__':
    with Problem1() as p:
        p.set_filename(sys.argv[1])
        print(f'SOLUTION: {p.solve()}')