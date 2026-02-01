setup() {
    load './test_helper/bats-support/load'
    load './test_helper/bats-assert/load'
    load './test_helper/aoc-support/load'
}

# bats test_tags=input:sample, year:2024, day:16
@test "AoC 2024 Day 16: Reindeer Maze               - sample" {
    run aoc_day _1
    assert_aoc_day _1
    run aoc_day _2
    assert_aoc_day _2
}
# bats test_tags=input:real, year:2024, day:16
@test "AoC 2024 Day 16: Reindeer Maze               - real  " { run aoc_day; assert_aoc_day; }

# bats test_tags=input:sample, year:2024, day:17
@test "AoC 2024 Day 17: Chronospatial Computer      - sample" {
    skip "No test file"
    run aoc_day; assert_aoc_day
}
# bats test_tags=input:real, year:2024, day:17
@test "AoC 2024 Day 17: Chronospatial Computer      - real  " { run aoc_day; assert_aoc_day; }

# bats test_tags=input:sample, year:2024, day:18
@test "AoC 2024 Day 18: RAM Run                     - sample" { run aoc_day _6_12; assert_aoc_day _6_12; }
# bats test_tags=input:real, year:2024, day:18
@test "AoC 2024 Day 18: RAM Run                     - real  " { run aoc_day; assert_aoc_day; }

# bats test_tags=input:sample, year:2024, day:22
@test "AoC 2024 Day 22: Monkey Market               - sample" { run aoc_day; assert_aoc_day; }
# bats test_tags=input:real, year:2024, day:22
@test "AoC 2024 Day 22: Monkey Market               - real  " { run aoc_day; assert_aoc_day; }

# bats test_tags=input:sample, year:2024, day:23
@test "AoC 2024 Day 23: LAN Party                   - sample" { run aoc_day; assert_aoc_day; }
# bats test_tags=input:real, year:2024, day:23
@test "AoC 2024 Day 23: LAN Party                   - real  " { run aoc_day; assert_aoc_day; }

# bats test_tags=input:sample, year:2024, day:25
@test "AoC 2024 Day 25: Code Chronicle              - sample" { run aoc_day; assert_aoc_day; }
# bats test_tags=input:real, year:2024, day:25
@test "AoC 2024 Day 25: Code Chronicle              - real  " { run aoc_day; assert_aoc_day; }
