setup() {
    load './test_helper/bats-support/load'
    load './test_helper/bats-assert/load'
    load './test_helper/aoc-support/load'
}

# bats test_tags=input:sample, year:2025, day:01
@test "AoC 2025 Day 01 Sample" {
    run aoc_day
    assert_output 'Part1: 3; Part2: 6'
}
# bats test_tags=input:real, year:2025, day:01
@test "AoC 2025 Day 01" {
    run aoc_day
    assert_output 'Part1: 1182; Part2: 6907'
}

# bats test_tags=input:sample, year:2025, day:02
@test "AoC 2025 Day 02 Sample" {
    run aoc_day
    assert_output 'Part1: 1227775554; Part2: 4174379265'
}
# bats test_tags=input:real, year:2025, day:02
@test "AoC 2025 Day 02" {
    skip "Too slow"
    run aoc_day
    assert_output 'Part1: 24043483400; Part2: 38262920235'
}

# bats test_tags=input:sample, year:2025, day:03
@test "AoC 2025 Day 03 Sample" {
    run aoc_day
    assert_output 'Part1: 357; Part2: 3121910778619'
}
# bats test_tags=input:real, year:2025, day:03
@test "AoC 2025 Day 03" {
    run aoc_day
    assert_output 'Part1: 17405; Part2: 171990312704598'
}

# bats test_tags=input:sample, year:2025, day:04
@test "AoC 2025 Day 04 Sample" {
    run aoc_day
    assert_output 'Part1: 13; Part2: 43'
}
# bats test_tags=input:real, year:2025, day:04
@test "AoC 2025 Day 04" {
    run aoc_day
    assert_output 'Part1: 1478; Part2: 9120'
}

# bats test_tags=input:sample, year:2025, day:05
@test "AoC 2025 Day 05 Sample" {
    skip "Solved in Haskell"
    run aoc_day
    assert_output 'Part1: 3; Part2: 14'
}
# bats test_tags=input:real, year:2025, day:05
@test "AoC 2025 Day 05" {
    skip "Solved in Haskell"
    run aoc_day
    assert_output 'Part1: 613; Part2: 336495597913098'
}

# bats test_tags=input:sample, year:2025, day:06
@test "AoC 2025 Day 06 Sample" {
    run aoc_day
    assert_output 'Part1: 4277556; Part2: 3263827'
}
# bats test_tags=input:real, year:2025, day:06
@test "AoC 2025 Day 06" {
    run aoc_day
    assert_output 'Part1: 5171061464548; Part2: 10189959087258'
}

# bats test_tags=input:sample, year:2025, day:07
@test "AoC 2025 Day 07 Sample" {
    run aoc_day
    assert_output 'Part1: 21; Part2: 40'
}
# bats test_tags=input:real, year:2025, day:07
@test "AoC 2025 Day 07" {
    run aoc_day
    assert_output 'Part1: 1535; Part2: 4404709551015'
}

# bats test_tags=input:sample, year:2025, day:08
@test "AoC 2025 Day 08 Sample" {
    run picat picat/src/2025/day08.pi 10 inputs/sample/2025/day08.txt
    assert_output 'Part1: 40; Part2: 25272'
}
# bats test_tags=input:real, year:2025, day:08
@test "AoC 2025 Day 08" {
    run picat picat/src/2025/day08.pi 1000 inputs/real/2025/day08.txt
    assert_output 'Part1: 57970; Part2: 8520040659'
}

# bats test_tags=input:sample, year:2025, day:09
@test "AoC 2025 Day 09 Sample" {
    run aoc_day
    assert_output 'Part1: 50; Part2: 24'
}
# bats test_tags=input:real, year:2025, day:09
@test "AoC 2025 Day 09" {
    run aoc_day
    assert_output 'Part1: 4756718172; Part2: 1665679194'
}

# bats test_tags=input:sample, year:2025, day:10
@test "AoC 2025 Day 10 Sample" {
    run aoc_day
    assert_output 'Part1: 7; Part2: 33'
}
# bats test_tags=input:real, year:2025, day:10
@test "AoC 2025 Day 10" {
    skip "Too slow"
    run aoc_day
    assert_output 'Part1: 505; Part2: 20002'
}

# bats test_tags=input:sample, year:2025, day:11
@test "AoC 2025 Day 11 Sample" {
    run picat picat/src/2025/day11.pi 1 inputs/sample/2025/day11_1.txt
    assert_output 'Part1: 5; '
    run picat picat/src/2025/day11.pi 2 inputs/sample/2025/day11_2.txt
    assert_output 'Part2: 2'
}
# bats test_tags=input:real, year:2025, day:11
@test "AoC 2025 Day 11" {
    run picat picat/src/2025/day11.pi 12 inputs/real/2025/day11.txt
    assert_output 'Part1: 652; Part2: 362956369749210'
}

# bats test_tags=input:sample, year:2025, day:12
@test "AoC 2025 Day 12 Sample" {
    run aoc_day
    assert_output 'Part1: Sorry, I can not solve this'
}
# bats test_tags=input:real, year:2025, day:12
@test "AoC 2025 Day 12" {
    run aoc_day
    assert_output 'Part1: 536'
}
