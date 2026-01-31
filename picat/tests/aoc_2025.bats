setup() {
    load './test_helper/bats-support/load'
    load './test_helper/bats-assert/load'
    load './test_helper/aoc-support/load'
}

# bats test_tags=input:sample, year:2025, day:01
@test "AoC 2025 Day 01 Sample" { run aoc_day; assert_aoc_day; }
# bats test_tags=input:real, year:2025, day:01
@test "AoC 2025 Day 01" { run aoc_day; assert_aoc_day; }

# bats test_tags=input:sample, year:2025, day:02
@test "AoC 2025 Day 02 Sample" { run aoc_day; assert_aoc_day; }
# bats test_tags=input:real, year:2025, day:02
@test "AoC 2025 Day 02" {
    skip "Too slow"
    run aoc_day
    assert_aoc_day
}

# bats test_tags=input:sample, year:2025, day:03
@test "AoC 2025 Day 03 Sample" { run aoc_day; assert_aoc_day; }
# bats test_tags=input:real, year:2025, day:03
@test "AoC 2025 Day 03" { run aoc_day; assert_aoc_day; }

# bats test_tags=input:sample, year:2025, day:04
@test "AoC 2025 Day 04 Sample" { run aoc_day; assert_aoc_day; }
# bats test_tags=input:real, year:2025, day:04
@test "AoC 2025 Day 04" { run aoc_day; assert_aoc_day; }

# bats test_tags=input:sample, year:2025, day:05
@test "AoC 2025 Day 05 Sample" {
    skip "Solved in Haskell"
    run aoc_day; assert_aoc_day;
}
# bats test_tags=input:real, year:2025, day:05
@test "AoC 2025 Day 05" {
    skip "Solved in Haskell"
    run aoc_day
    assert_aoc_day
}

# bats test_tags=input:sample, year:2025, day:06
@test "AoC 2025 Day 06 Sample" { run aoc_day; assert_aoc_day; }
# bats test_tags=input:real, year:2025, day:06
@test "AoC 2025 Day 06" { run aoc_day; assert_aoc_day; }

# bats test_tags=input:sample, year:2025, day:07
@test "AoC 2025 Day 07 Sample" { run aoc_day; assert_aoc_day; }
# bats test_tags=input:real, year:2025, day:07
@test "AoC 2025 Day 07" { run aoc_day; assert_aoc_day; }

# bats test_tags=input:sample, year:2025, day:08
@test "AoC 2025 Day 08 Sample" {
    run picat picat/src/2025/day08.pi 10 inputs/sample/2025/day08.txt
    assert_aoc_day
}
# bats test_tags=input:real, year:2025, day:08
@test "AoC 2025 Day 08" {
    run picat picat/src/2025/day08.pi 1000 inputs/real/2025/day08.txt
    assert_aoc_day
}

# bats test_tags=input:sample, year:2025, day:09
@test "AoC 2025 Day 09 Sample" { run aoc_day; assert_aoc_day; }
# bats test_tags=input:real, year:2025, day:09
@test "AoC 2025 Day 09" { run aoc_day; assert_aoc_day; }

# bats test_tags=input:sample, year:2025, day:10
@test "AoC 2025 Day 10 Sample" { run aoc_day; assert_aoc_day; }
# bats test_tags=input:real, year:2025, day:10
@test "AoC 2025 Day 10" {
    skip "Too slow"
    run aoc_day; assert_aoc_day
}

# bats test_tags=input:sample, year:2025, day:11
@test "AoC 2025 Day 11 Sample" {
    run picat picat/src/2025/day11.pi 1 inputs/sample/2025/day11_1.txt
    assert_aoc_day _1
    run picat picat/src/2025/day11.pi 2 inputs/sample/2025/day11_2.txt
    assert_aoc_day _2
}
# bats test_tags=input:real, year:2025, day:11
@test "AoC 2025 Day 11" {
    run picat picat/src/2025/day11.pi 12 inputs/real/2025/day11.txt
    assert_aoc_day
}

# bats test_tags=input:sample, year:2025, day:12
@test "AoC 2025 Day 12 Sample" { run aoc_day; assert_aoc_day; }
# bats test_tags=input:real, year:2025, day:12
@test "AoC 2025 Day 12" { run aoc_day; assert_aoc_day; }
