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
import AoC2022.Day15
import AoC2022.Day18
import AoC2022.Day21
import AoC2022.Day22
import AoC2022.Day23
import AoC2022.Day24
import AoC2022.Day25
import AoC2023.Day01
import AoC2023.Day02
import AoC2023.Day03
import AoC2023.Day04
import AoC2023.Day05
import AoC2023.Day06
import AoC2023.Day07
import AoC2023.Day08
import AoC2023.Day09
import AoC2023.Day10
import AoC2023.Day11
import AoC2023.Day13
import AoC2023.Day14
import AoC2023.Day15
import AoC2023.Day16
import AoC2023.Day17
import AoC2023.Day18
import AoC2023.Day19
import AoC2024.Day01
import AoC2024.Day02

main = defaultMain tests

type TestDay = [(String, String -> String -> Assertion)]

testYear :: Int -> [[(String, String -> String -> Assertion)]] -> TestTree
testYear year days =
    testGroup ("Year " ++ show year) $
        map (\inputs -> testGroup ("Day " ++ (take 2 $ fst $ head $ inputs)) $
              map (\(fileName, generator) ->
                        let path = show year ++ "/day" ++ fileName ++ ".txt"
                        in testCase path $ do
                            input <- readFile ("../inputs/" ++ path)
                            generator path input)
                  inputs)
        days

testDay1 :: (Eq a, Show a) => (String -> a) -> [(String, a)] -> TestDay
testDay1 sut inputs =
    map (\(fileName, expected) ->
          (fileName, \path input -> assertEqual ("AoC" ++ path) expected (sut input)))
    inputs

testDay2 :: (Eq a, Show a) => (b -> String -> a) -> [(String, b, a)] -> TestDay
testDay2 sut inputs =
    map (\(fileName, extra, expected) ->
          (fileName, \path input -> assertEqual ("AoC" ++ path) expected (sut extra input)))
    inputs

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
    [ testYear 2015
        [ testDay1 aoc201501 [("01", (138, 1771))],
          testDay1 aoc201502 [("02", (1586300, 3737498))],
          testDay1 aoc201503 [("03", (2081, 2341))],
          testDay1 aoc201505 [("05", (255, 55)), ("05S", (2, 0))],
          testDay1 aoc201506 [("06", (377891, 14110788))]
        ],
      testYear 2020
        [ testDay1 aoc202001 [("01", (1007104, 18847752))],
          testDay1 aoc202002 [("02", (603, 404))],
          testDay1 aoc202003 [("03", (191, 1478615040)), ("03S", (7, 336))],
          testDay1 aoc202004 [("04", (222, 140)), ("04S", (2, 2)), ("04M", (4, 0)), ("04L", (4, 4))],
          testDay1 aoc202005 [("05", (880, 731))],
          testDay1 aoc202006 [("06", (6683, 3122)), ("06S", (11, 6))],
          testDay1 aoc202007 [("07", (179, 18925)), ("07S", (4, 32))],
          testDay1 aoc202008 [("08", (2014, 2251)), ("08S", (5, 8))],
          testDay1 aoc202009 [("09", (1639024365, 219202240))],
          testDay1 aoc202010 [("10", (1625, 3100448333024)), ("10S", (35, 8)), ("10L", (220, 19208))],
          testDay1 aoc202011 [("11", (2247, 2011)), ("11S", (37, 26))],
          testDay1 aoc202012 [("12", (1603, 52866)), ("12S", (25, 286))],
          testDay1 aoc202013 [("13", (174, 780601154795940)), ("13S", (295, 1068781))],
          testDay1 aoc202014 [("14", (13496669152158, 3278997609887))],
          testDay1 aoc202015 [("15", (1259, "MISSING"))], -- part2 is too slow, it was implemented in Go
          testDay1 aoc202016 [("16", (23044, 3765150732757))],
          testDay1 aoc202017 [("17", (257, 2532)), ("17S", (112, 848))],
          testDay1 aoc202018 [("18", (86311597203806, 276894767062189))],
          testDay1 aoc202019 [("19", "MISSING"), ("19S", "MISSING"), ("19L", "MISSING")],
          testDay1 aoc202020 [("20", (104831106565027, "MISSING")), ("20S", (20899048083289, "MISSING"))],
          testDay1 aoc202021 [("21", (2485, "bqkndvb,zmb,bmrmhm,snhrpv,vflms,bqtvr,qzkjrtl,rkkrx")), ("21S", (5, "mxmxvkd,sqjhc,fvjkl"))],
          testDay1 aoc202022 [("22", (31957, 33212)), ("22S", (306, 291))],
          testDay1 aoc202023 [("23", ("29385746", "680435423892")), ("23S", ("67384529", "149245887792"))],
          testDay1 aoc202024 [("24", (232, 3519)), ("24S", (10, 2208))],
          testDay1 aoc202025 [("25", 12285001), ("25S", 14897079)]
        ],
      testYear 2021
        [ testDay1 aoc202101 [("01", (1532, 1571)), ("01S", (7, 5))],
          testDay1 aoc202102 [("02", (1635930, 1781819478)), ("02S", (150, 900))],
          testDay1 aoc202103 [("03", (3633500, 4550283)), ("03S", (198, 230))],
          testDay1 aoc202104 [("04", (33462, 30070)), ("04S", (4512, 1924))],
          testDay1 aoc202105 [("05", (4745, 18442)), ("05S", (5, 12))],
          testDay1 aoc202106 [("06", (356190, 1617359101538)), ("06S", (5934, 26984457539))],
          testDay1 aoc202107 [("07", (344735, 96798233)), ("07S", (37, 168))],
          testDay1 aoc202108 [("08", (554, 990964)), ("08S", (26, 61229))],
          testDay1 aoc202109 [("09", (439, 900900)), ("09S", (15, 1134))],
          testDay1 aoc202110 [("10", (411471, 3122628974)), ("10S", (26397, 288957))],
          testDay1 aoc202111 [("11", (1647, 348)), ("11S", (1656, 195))],
          testDay1 aoc202112 [("12", (4241, 122134)), ("12S", (10, 36)), ("12M", (19, 103)), ("12L", (226, 3509))],
          testDay1 aoc202113 [("13", (712, exp202113)), ("13S", (17, exp202113S))],
          testDay1 aoc202114 [("14", (2549, 2516901104210)), ("14S", (1588, 2188189693529))],
          testDay1 aoc202115 [("15", (739, 3040)), ("15S", (40, 315))],
          testDay1 aoc202116 [("16", (993, 144595909277)), ("16S", (31, 54))],
          testDay1 aoc202117 [("17", (12561, 3785)), ("17S", (45, 112))],
          testDay1 aoc202118 [("18", (4173, 4706)), ("18S", (4140, 3993))],
          --testDay1 aoc202119 [("19", MISSING
          --testDay1 aoc202120 [("20", MISSING
          --testDay1 aoc202121 [("21", MISSING
          --testDay1 aoc202122 [("22", MISSING
          --testDay1 aoc202123 [("23", MISSING
          --testDay1 aoc202124 [("24", MISSING
          testDay1 aoc202125 [("25", 474), ("25S", 58)]
        ],
      testYear 2022
        [ testDay1 aoc202201 [("01", (72240, 210957)), ("01S", (24000, 45000))],
          testDay1 aoc202202 [("02", (11841, 13022)), ("02S", (15, 12))],
          testDay1 aoc202203 [("03", (7824, 2798)), ("03S", (157, 70))],
          testDay1 aoc202204 [("04", (496, 847)), ("04S", (2, 4))],
          testDay1 aoc202205 [("05", ("RFFFWBPNS", "CQQBBJFCS")), ("05S", ("CMZ", "MCD"))],
          testDay1 aoc202206 [("06", ([1100], [2421])), ("06S", ([7, 5, 6, 10, 11], [19, 23, 23, 29, 26]))],
          testDay1 aoc202207 [("07", (1770595, 2195372)), ("07S", (95437, 24933642))],
          testDay1 aoc202208 [("08", (1719, 590824)), ("08S", (21, 8))],
          testDay1 aoc202209 [("09", (6498, 2531)), ("09S", (13, 1)), ("09L", (88, 36))],
          testDay1 aoc202210 [("10", (15140, exp202210)), ("10S", (13140, exp202210S))],
          testDay1 aoc202211 [("11", (112221, 25272176808)), ("11S", (10605, 2713310158))],
          testDay1 aoc202212 [("12", (425, 418)), ("12S", (31, 29))],
          testDay1 aoc202213 [("13", (6086, 27930)), ("13S", (13, 140))],
          --testDay1 aoc202214 "MISSING, in java"
          testDay2 aoc202215 [("15", 2000000, (4811413, 13171855019123)), ("15S", 10, (26, 56000011))],
          --testDay1 aoc202216 "MISSING, in java"
          --testDay1 aoc202217 "MISSING, by hand"
          testDay1 aoc202218 [("18", (3498, 2008)), ("18S", (64, 58))],
          --testDay1 aoc202219 "MISSING"
          --testDay1 aoc202220 "MISSING, in java"
          testDay1 aoc202221 [("21", (75147370123646, 3423279932937)), ("21S", (152, 301))],
          testDay2 aoc202222 [("22", False, (50412, 130068)), ("22S", True, (6032, 5031))],
          testDay1 aoc202223 [("23", (4082, 1065)), ("23S", (110, 20))],
          testDay1 aoc202224 [("24", (301, 859)), ("24S", (18, 54))],
          testDay1 aoc202225 [("25", "20-1-11==0-=0112-222"), ("25S", "2=-1=0")]
        ],
      testYear 2023
        [ testDay1 aoc202301 [("01", (54338, 53389)), ("01S", (142, 142))]
        , testDay1 aoc202302 [("02", (2085, 79315)), ("02S", (8, 2286))]
        , testDay1 aoc202303 [("03", (538046, 81709807)), ("03S", (4361, 467835))]
        , testDay1 aoc202304 [("04", (28538, 9425061)), ("04S", (13, 30))]
        , testDay1 aoc202305 [("05", (650599855, 1240035)), ("05S", (35, 46))]
        , testDay1 aoc202306 [("06", (4568778, 28973936)), ("06S", (288, 71503))]
        , testDay1 aoc202307 [("07", (241344943, 243101568)), ("07S", (6440, 5905))]
        , testDay1 aoc202308 [("08", ("MISSING", "MISSING")), ("08S", ("MISSING", "MISSING")), ("08M", ("MISSING", "MISSING")), ("08L", ("MISSING", "MISSING"))]
        , testDay1 aoc202309 [("09", (1834108701, 993)), ("09S", (114, 2))]
        , testDay1 aoc202310 [("10", (7097, 355)), ("10XS", (4, 1)), ("10S", (8, 1)), ("10M", (23, 4)), ("10L", (70, 8)), ("10XL", (80, 10))]
        , testDay1 aoc202311 [("11", (9724940, 569052586852)), ("11S", (374, 82000210))]
        --, testDay1 aoc202312 MISSING
        , testDay1 aoc202313 [("13", (33975, 29083)), ("13S", (405, 400))]
        , testDay1 aoc202314 [("14", (108641, 84328)), ("14S", (136, 64))]
        , testDay1 aoc202315 [("15", (502139, 284132)), ("15S", (1320, 145))]
        , testDay1 aoc202316 [("16", (8021, 8216)), ("16S", (46, 51))]
        , testDay1 aoc202317 [("17", ("MISSING", "MISSING")), ("17S", ("MISSING", "MISSING"))]
        , testDay1 aoc202318 [("18", (41019, 96116995735219)), ("18S", (62, 952408144115))]
        , testDay1 aoc202319 [("19", (350678, 124831893423809)), ("19S", (19114, 167409079868000))]
        ],
      testYear 2024
        [ testDay1 aoc202401 [("01", (2285373, 21142653)), ("01S", (11, 31))]
        , testDay1 aoc202402 [("02", (502, 544)), ("02S", (2, 4))]
        ]
    ]
