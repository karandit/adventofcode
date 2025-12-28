setup() {
    load './test_helper/bats-support/load'
    load './test_helper/bats-assert/load'
    load './test_helper/aoc-support/load'
}

# bats test_tags=input:sample, year:2015, day:04
@test "AoC 2015 Day 04 Sample" {
    skip "too slow"
    run aoc_day _test
    assert_output 'Part1: 3609043'
}
# bats test_tags=input:real, year:2015, day:04
@test "AoC 2015 Day 04" {
    skip "too slow"
    run aoc_day
    assert_output 'Part1: 346386; Part2: 9958218'
}
