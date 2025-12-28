setup() {
    load './test_helper/bats-support/load'
    load './test_helper/bats-assert/load'
    load './test_helper/aoc-support/load'
}

# bats test_tags=input:sample, year:2018, day:01
@test "AoC 2018 Day 01 Sample" {
    run aoc_day _test
    assert_output 'Part1: 3; Part2: 2'
}
# bats test_tags=input:real, year:2018, day:01
@test "AoC 2018 Day 01" {
    run aoc_day
    assert_output 'Part1: 472; Part2: 66932'
}
