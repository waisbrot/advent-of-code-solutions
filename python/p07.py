import sys
import re
import logging
from python.problem import ProblemBase
from collections import OrderedDict, defaultdict
from enum import Enum

logging.basicConfig(level=logging.INFO)

class Card:
    face:str
    value:int
    def __init__(self, value_str: str) -> None:
        self.face = value_str
        if value_str.isdigit():
            self.value = int(value_str)
        elif value_str == 'T': self.value = 10
        elif value_str == 'J': self.value = 11
        elif value_str == 'Q': self.value = 12
        elif value_str == 'K': self.value = 13
        elif value_str == 'A': self.value = 14

    def __eq__(self, other) -> bool:
        return self.value == other.value
    
    def __lt__(self, other) -> bool:
        return self.value < other.value
    
    def __repr__(self) -> str:
        return self.face
    
    def __hash__(self) -> int:
        return self.value

class HandType(Enum):
    HIGH_CARD = 1
    ONE_PAIR = 2
    TWO_PAIR = 3
    THREE_OF_KIND = 4
    FULL_HOUSE = 5
    FOUR_OF_KIND = 6
    FIVE_OF_KIND = 7

    def __lt__(self, other) -> bool:
        return self.value < other.value

class Hand:
    bid: int
    cards: list[Card]
    type: HandType

    def __init__(self, hand_str: str) -> None:
        cards, bid = hand_str.split(' ')
        self.bid = int(bid)
        self.cards = []
        for c in cards:
            self.cards.append(Card(c))
        self.type = self.calculate_type()

    def calculate_type(self) -> HandType:
        count = defaultdict(int)
        for c in self.cards:
            count[c] += 1
        cards = sorted(count.items(), key=lambda i: i[1], reverse=True)
        if len(cards) == 1:
            return HandType.FIVE_OF_KIND
        elif len(cards) == 2:
            if cards[0][1] == 4:
                return HandType.FOUR_OF_KIND
            else:
                return HandType.FULL_HOUSE
        elif len(cards) == 3:
            if cards[0][1] == 3:
                return HandType.THREE_OF_KIND
            elif cards[0][1] == 2:
                return HandType.TWO_PAIR
        elif len(cards) == 4:
            return HandType.ONE_PAIR
        else:
            return HandType.HIGH_CARD
        assert False, f'No hand type for {cards}'

    def __repr__(self) -> str:
        return ''.join([str(c) for c in self.cards]) + ' ' + str(self.bid) + ' ' + str(self.type)
        
    def __lt__(self, other) -> bool:
        if self.type != other.type:
            return self.type < other.type
        else:
            for c1,c2 in zip(self.cards, other.cards):
                if c1 != c2:
                    return c1 < c2
        assert False, "equal hands"

class Problem7(ProblemBase):        
    hands: list[Hand]
    def __init__(self) -> None:
        super().__init__()
        self.hands = []

    def read_input(self) -> None:
        for line in self.input_as_lines():
            self.hands.append(Hand(line))
 
    def solve1(self) -> str:
        sum = 0
        ranked = sorted(self.hands)
        for rank, hand in enumerate(ranked, start=1):
            sum += rank * hand.bid
        return sum

    def solve2(self) -> str:
        pass

if __name__ == '__main__':
    with Problem7() as p:
        p.set_filename(sys.argv[1])
        p.read_input()
        print(f'SOLUTION1: {p.solve1()}')
        print(f'SOLUTION2: {p.solve2()}')
