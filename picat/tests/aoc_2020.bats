setup() {
    load './test_helper/bats-support/load'
    load './test_helper/bats-assert/load'
    load './test_helper/aoc-support/load'
}

# bats test_tags=input:sample, year:2020, day:01
@test "AoC 2020 Day 01 Sample" {
    run aoc_day
    assert_output 'Part1: 514579; Part2: 241861950'
}
# bats test_tags=input:real, year:2020, day:01
@test "AoC 2020 Day 01" {
    run aoc_day
    assert_output 'Part1: 1007104; Part2: 18847752'
}

# bats test_tags=input:sample, year:2020, day:20
@test "AoC 2020 Day 20 Sample" {
    run aoc_day
    assert_output 'Part1: 20899048083289; Part2: 273'
}
# bats test_tags=input:real, year:2020, day:20
@test "AoC 2020 Day 20" {
    run aoc_day
    assert_output 'Part1: 104831106565027; Part2: 2093'
}
