import unittest

from src.AoC2023 import Day01 as AoC202301

class AoCTest(unittest.TestCase):
    def assertDay(self, solver, year, day_inp, expected): 
         with open(f"../inputs/{year}/day{day_inp}.txt") as f:
             data = f.read()
         self.assertEqual(solver(data), expected)

    def test_day_01(self):
         self.assertDay(AoC202301.aoc202301, 2023, "01S", (142, 142))
         self.assertDay(AoC202301.aoc202301, 2023, "01", (54338, 53389))


if __name__ == '__main__':
    unittest.main()
