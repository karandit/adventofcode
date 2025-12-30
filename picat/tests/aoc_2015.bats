setup() {
    load './test_helper/bats-support/load'
    load './test_helper/bats-assert/load'
    load './test_helper/aoc-support/load'
}

# bats test_tags=input:sample, year:2015, day:04
@test "AoC 2015 Day 04: The Ideal Stocking Stuffer - sample" {
    skip "too slow"
    run aoc_day _test
    assert_output 'Part1: 3609043'
}
# bats test_tags=input:real, year:2015, day:04
@test "AoC 2015 Day 04: The Ideal Stocking Stuffer - real  " {
    skip "too slow"
    run aoc_day
    assert_output 'Part1: 346386; Part2: 9958218'
}

# bats test_tags=input:sample, year:2015, day:07
@test "AoC 2015 Day 07: Some Assembly Required     - sample" {
    run aoc_day _test
    assert_output 'Part1: 492; Part2: 492'
}
# bats test_tags=input:real, year:2015, day:07
@test "AoC 2015 Day 07: Some Assembly Required     - real  " {
    run aoc_day
    assert_output 'Part1: 3176; Part2: 14710'
}
