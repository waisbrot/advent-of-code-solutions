import unittest
from python.p01 import Problem1
from os.path import abspath, dirname

class TestProblem1(unittest.TestCase):
    def test_examples(self):
        pwd = dirname(abspath(__file__))
        for (example, result) in (('01a', 142), ('01b', 281)):
            file = abspath(f'{pwd}/../examples/{example}')
            with Problem1() as prob:
                prob.set_filename(file)
                self.assertEquals(prob.solve(), result)

    def test_err1(self):
        with Problem1() as prob:
            prob.set_string('two4745three65seven')
            self.assertEquals(prob.solve(), 27)

    def test_err2(self):
        with Problem1() as prob:
            prob.set_string('oneight')
            self.assertEquals(prob.solve(), 18)
