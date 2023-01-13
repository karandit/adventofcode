import Test.Tasty
import Test.Tasty.HUnit
import AoC2015.Day01
import AoC2015.Day02
import AoC2015.Day03
import AoC2015.Day05
import AoC2015.Day06
import AoC2020.Day01
import AoC2020.Day02
import AoC2020.Day03
import AoC2020.Day04
import AoC2020.Day05
import AoC2020.Day06
import AoC2020.Day07
import AoC2020.Day08
import AoC2020.Day09
import AoC2020.Day10
import AoC2020.Day11
import AoC2020.Day12
import AoC2020.Day13
import AoC2020.Day14
import AoC2020.Day15
import AoC2020.Day16
import AoC2020.Day17
import AoC2020.Day18
import AoC2020.Day19
import AoC2020.Day20
import AoC2020.Day21
import AoC2020.Day22
import AoC2020.Day23
import AoC2020.Day24
import AoC2020.Day25
import AoC2021.Day01
import AoC2021.Day02
import AoC2021.Day03
import AoC2021.Day04
import AoC2021.Day05
import AoC2021.Day06
import AoC2021.Day07
import AoC2021.Day08
import AoC2021.Day09
import AoC2021.Day10
import AoC2021.Day11
import AoC2021.Day12
import AoC2021.Day13
import AoC2021.Day14
import AoC2021.Day15
import AoC2021.Day16
import AoC2021.Day17
import AoC2021.Day18
import AoC2021.Day25
import AoC2022.Day01
import AoC2022.Day02
import AoC2022.Day03
import AoC2022.Day04
import AoC2022.Day05
import AoC2022.Day06
import AoC2022.Day07
import AoC2022.Day08
import AoC2022.Day09
import AoC2022.Day10
import AoC2022.Day11
import AoC2022.Day12
import AoC2022.Day13
import AoC2022.Day21
import AoC2022.Day25

main = defaultMain tests

assertAoC :: (Eq a, Show a) => String -> (String -> a) -> [(String, a)] -> TestTree
assertAoC year sut inputs =
    testGroup ("Day " ++ (take 2 $ fst $ head $ inputs)) $
      map (\(fileName, expected) ->
                let path = year ++ "/day" ++ fileName ++ ".txt"
                in testCase path $ do
                    input <- readFile ("../inputs/" ++ path)
                    assertEqual ("AoC " ++ path) expected (sut input))
          inputs

assert2015 :: (Eq a, Show a) => (String -> a) -> [(String, a)] -> TestTree
assert2015 = assertAoC "2015"

assert2020 :: (Eq a, Show a) => (String -> a) -> [(String, a)] -> TestTree
assert2020 = assertAoC "2020"

assert2021 :: (Eq a, Show a) => (String -> a) -> [(String, a)] -> TestTree
assert2021 = assertAoC "2021"

assert2022 :: (Eq a, Show a) => (String -> a) -> [(String, a)] -> TestTree
assert2022 = assertAoC "2022"

exp202113S =
  [ "xxxxx",
    "x   x",
    "x   x",
    "x   x",
    "xxxxx"
  ]

exp202113 =
  [ "xxx  x    x  x xxxx   xx xxx    xx xxxx",
    "x  x x    x  x x       x x  x    x x   ",
    "xxx  x    xxxx xxx     x x  x    x xxx ",
    "x  x x    x  x x       x xxx     x x   ",
    "x  x x    x  x x    x  x x    x  x x   ",
    "xxx  xxxx x  x x     xx  x     xx  x   "
  ]

exp202210S =
  [ "##..##..##..##..##..##..##..##..##..##..",
    "###...###...###...###...###...###...###.",
    "####....####....####....####....####....",
    "#####.....#####.....#####.....#####.....",
    "######......######......######......####",
    "#######.......#######.......#######....."
  ]

exp202210 =
  [ "###..###....##..##..####..##...##..###..",
    "#..#.#..#....#.#..#....#.#..#.#..#.#..#.",
    "###..#..#....#.#..#...#..#....#..#.#..#.",
    "#..#.###.....#.####..#...#.##.####.###..",
    "#..#.#....#..#.#..#.#....#..#.#..#.#....",
    "###..#.....##..#..#.####..###.#..#.#...."
  ]

tests =
  testGroup "Advent of Code unit tests"
    [ testGroup "Year 2015"
        [ assert2015 aoc201501 [("01", (138, 1771))],
          assert2015 aoc201502 [("02", (1586300, 3737498))],
          assert2015 aoc201503 [("03", (2081, 2341))],
          assert2015 aoc201505 [("05", (255, 55)), ("05S", (2, 0))],
          assert2015 aoc201506 [("06", (377891, 14110788))]
        ],
      testGroup "Year 2020"
        [ assert2020 aoc202001 [("01", (1007104, 18847752))],
          assert2020 aoc202002 [("02", (603, 404))],
          assert2020 aoc202003 [("03", (191, 1478615040)), ("03S", (7, 336))],
          assert2020 aoc202004 [("04", (222, 140)), ("04S", (2, 2)), ("04M", (4, 0)), ("04L", (4, 4))],
          assert2020 aoc202005 [("05", (880, 731))],
          assert2020 aoc202006 [("06", (6683, 3122)), ("06S", (11, 6))],
          assert2020 aoc202007 [("07", (179, 18925)), ("07S", (4, 32))],
          assert2020 aoc202008 [("08", (2014, 2251)), ("08S", (5, 8))],
          assert2020 aoc202009 [("09", (1639024365, 219202240))],
          assert2020 aoc202010 [("10", (1625, 3100448333024)), ("10S", (35, 8)), ("10L", (220, 19208))],
          assert2020 aoc202011 [("11", (2247, 2011)), ("11S", (37, 26))],
          assert2020 aoc202012 [("12", (1603, 52866)), ("12S", (25, 286))],
          assert2020 aoc202013 [("13", (174, 780601154795940)), ("13S", (295, 1068781))],
          assert2020 aoc202014 [("14", (13496669152158, 3278997609887))],
          assert2020 aoc202015 [("15", (1259, "MISSING"))], -- part2 is too slow, it was implemented in Go
          assert2020 aoc202016 [("16", (23044, 3765150732757))],
          assert2020 aoc202017 [("17", (257, 2532)), ("17S", (112, 848))],
          assert2020 aoc202018 [("18", (86311597203806, 276894767062189))],
          assert2020 aoc202019 [("19", "MISSING"), ("19S", "MISSING"), ("19L", "MISSING")],
          assert2020 aoc202020 [("20", (104831106565027, "MISSING")), ("20S", (20899048083289, "MISSING"))],
          assert2020 aoc202021 [("21", (2485, "bqkndvb,zmb,bmrmhm,snhrpv,vflms,bqtvr,qzkjrtl,rkkrx")), ("21S", (5, "mxmxvkd,sqjhc,fvjkl"))],
          assert2020 aoc202022 [("22", (31957, 33212)), ("22S", (306, 291))],
          assert2020 aoc202023 [("23", ("29385746", "680435423892")), ("23S", ("67384529", "149245887792"))],
          assert2020 aoc202024 [("24", (232, 3519)), ("24S", (10, 2208))],
          assert2020 aoc202025 [("25", 12285001), ("25S", 14897079)]
        ],
      testGroup "Year 2021"
        [ assert2021 aoc202101 [("01", (1532, 1571)), ("01S", (7, 5))],
          assert2021 aoc202102 [("02", (1635930, 1781819478)), ("02S", (150, 900))],
          assert2021 aoc202103 [("03", (3633500, 4550283)), ("03S", (198, 230))],
          assert2021 aoc202104 [("04", (33462, 30070)), ("04S", (4512, 1924))],
          assert2021 aoc202105 [("05", (4745, 18442)), ("05S", (5, 12))],
          assert2021 aoc202106 [("06", (356190, 1617359101538)), ("06S", (5934, 26984457539))],
          assert2021 aoc202107 [("07", (344735, 96798233)), ("07S", (37, 168))],
          assert2021 aoc202108 [("08", (554, 990964)), ("08S", (26, 61229))],
          assert2021 aoc202109 [("09", (439, 900900)), ("09S", (15, 1134))],
          assert2021 aoc202110 [("10", (411471, 3122628974)), ("10S", (26397, 288957))],
          assert2021 aoc202111 [("11", (1647, 348)), ("11S", (1656, 195))],
          assert2021 aoc202112 [("12", (4241, 122134)), ("12S", (10, 36)), ("12M", (19, 103)), ("12L", (226, 3509))],
          assert2021 aoc202113 [("13", (712, exp202113)), ("13S", (17, exp202113S))],
          assert2021 aoc202114 [("14", (2549, 2516901104210)), ("14S", (1588, 2188189693529))],
          assert2021 aoc202115 [("15", (739, 3040)), ("15S", (40, 315))],
          assert2021 aoc202116 [("16", (993, 144595909277)), ("16S", (31, 54))],
          assert2021 aoc202117 [("17", (12561, 3785)), ("17S", (45, 112))],
          assert2021 aoc202118 [("18", (4173, 4706)), ("18S", (4140, 3993))],
          --assert2021 aoc202119 [("19", MISSING
          --assert2021 aoc202120 [("20", MISSING
          --assert2021 aoc202121 [("21", MISSING
          --assert2021 aoc202122 [("22", MISSING
          --assert2021 aoc202123 [("23", MISSING
          --assert2021 aoc202124 [("24", MISSING
          assert2021 aoc202125 [("25", 474), ("25S", 58)]
        ],
      testGroup "Year 2022"
        [ assert2022 aoc202201 [("01", (72240, 210957)), ("01S", (24000, 45000))],
          assert2022 aoc202202 [("02", (11841, 13022)), ("02S", (15, 12))],
          assert2022 aoc202203 [("03", (7824, 2798)), ("03S", (157, 70))],
          assert2022 aoc202204 [("04", (496, 847)), ("04S", (2, 4))],
          assert2022 aoc202205 [("05", ("RFFFWBPNS", "CQQBBJFCS")), ("05S", ("CMZ", "MCD"))],
          assert2022 aoc202206 [("06", ([1100], [2421])), ("06S", ([7, 5, 6, 10, 11], [19, 23, 23, 29, 26]))],
          assert2022 aoc202207 [("07", (1770595, 2195372)), ("07S", (95437, 24933642))],
          assert2022 aoc202208 [("08", (1719, 590824)), ("08S", (21, 8))],
          assert2022 aoc202209 [("09", (6498, 2531)), ("09S", (13, 1)), ("09L", (88, 36))],
          assert2022 aoc202210 [("10", (15140, exp202210)), ("10S", (13140, exp202210S))],
          assert2022 aoc202211 [("11", (112221, 25272176808)), ("11S", (10605, 2713310158))],
          assert2022 aoc202212 [("12", (425, 418)), ("12S", (31, 29))],
          assert2022 aoc202213 [("13", (6086, 27930)), ("13S", (13, 140))],
          assert2022 aoc202221 [("21", (75147370123646, 3423279932937)), ("21S", (152, 301))],
          assert2022 aoc202225 [("25", "20-1-11==0-=0112-222"), ("25S", "2=-1=0")]
        ]
    ]
