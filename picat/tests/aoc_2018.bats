setup() {
    load './test_helper/bats-support/load'
    load './test_helper/bats-assert/load'
    load './test_helper/aoc-support/load'
}

# bats test_tags=input:sample, year:2018, day:01
@test "AoC 2018 Day 01: Chronal Calibration         - sample" { run aoc_day; assert_aoc_day; }
# bats test_tags=input:real, year:2018, day:01
@test "AoC 2018 Day 01: Chronal Calibration         - real  " { run aoc_day; assert_aoc_day; }
