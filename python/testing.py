import unittest
import os.path

def main():
    class DiscoverTests(unittest.TestProgram):
        def __init__(self):
            self.testLoader=unittest.loader.defaultTestLoader
    tester = DiscoverTests()
    root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    tester.start = root
    tester.pattern = 'test*.py'
    tester.top = None
    tests = tester.createTests(from_discovery=True)
    print(f'root={root} file={__file__}')
    unittest.main(tests)

if __name__ == '__main__':
    main()