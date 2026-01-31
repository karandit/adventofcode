setup() {
    load './test_helper/bats-support/load'
    load './test_helper/bats-assert/load'
    load './test_helper/aoc-support/load'
}

# bats test_tags=input:sample, year:2020, day:01
@test "AoC 2020 Day 01: Report Repair               - sample" { run aoc_day; assert_aoc_day; }
# bats test_tags=input:real, year:2020, day:01
@test "AoC 2020 Day 01: Report Repair               - real  " { run aoc_day; assert_aoc_day; }

# bats test_tags=input:sample, year:2020, day:20
@test "AoC 2020 Day 20: Jurassic Jigsaw             - sample" { run aoc_day; assert_aoc_day; }
# bats test_tags=input:real, year:2020, day:20
@test "AoC 2020 Day 20: Jurassic Jigsaw             - real  " { run aoc_day; assert_aoc_day; }
