setup() {
    load './test_helper/bats-support/load'
    load './test_helper/bats-assert/load'
    load './test_helper/aoc-support/load'
}

# bats test_tags=input:sample, year:2016, day:15
@test "AoC 2016 Day 15 Sample" {
    skip "no test file"
    run aoc_day _test
    assert_output 'Part1: 3'
}
# bats test_tags=input:real, year:2016, day:15
@test "AoC 2016 Day 15" {
    run aoc_day
    assert_output 'Part1: 3208583'
}
