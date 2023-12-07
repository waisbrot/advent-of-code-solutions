import unittest
from os.path import dirname, abspath
from python.p02 import Problem2

class TestProblem1(unittest.TestCase):
    def test_example1(self):
        pwd = dirname(abspath(__file__))
        for (example, result) in (('02a', 8)):
            file = abspath(f'{pwd}/../examples/{example}')
            with Problem2() as prob:
                prob.set_filename(file)
                self.assertEquals(prob.solve1(), result)
