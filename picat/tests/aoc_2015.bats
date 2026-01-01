setup() {
    load './test_helper/bats-support/load'
    load './test_helper/bats-assert/load'
    load './test_helper/aoc-support/load'
}

# bats test_tags=input:sample, year:2015, day:04
@test "AoC 2015 Day 04: The Ideal Stocking Stuffer  - sample" {
    skip "too slow"
    run aoc_day _test
    assert_output 'Part1: 3609043'
}
# bats test_tags=input:real, year:2015, day:04
@test "AoC 2015 Day 04: The Ideal Stocking Stuffer  - real  " {
    skip "too slow"
    run aoc_day
    assert_output 'Part1: 346386; Part2: 9958218'
}

# bats test_tags=input:sample, year:2015, day:07
@test "AoC 2015 Day 07: Some Assembly Required      - sample" {
    run aoc_day _test
    assert_output 'Part1: 492; Part2: 492'
}
# bats test_tags=input:real, year:2015, day:07
@test "AoC 2015 Day 07: Some Assembly Required      - real  " {
    run aoc_day
    assert_output 'Part1: 3176; Part2: 14710'
}

# bats test_tags=input:sample, year:2015, day:10
@test "AoC 2015 Day 10: Elves Look, Elves Say       - sample" {
    run aoc_day _test
    assert_output 'Part1: 245442; Part2: 3476808'
}
# bats test_tags=input:real, year:2015, day:10
@test "AoC 2015 Day 10: Elves Look, Elves Say       - real  " {
    run aoc_day
    assert_output 'Part1: 252594; Part2: 3579328'
}

# bats test_tags=input:sample, year:2015, day:13
@test "AoC 2015 Day 13: Knights of the Dinner Table - sample" {
    run aoc_day _test
    assert_output 'Part1: 330; Part2: 286'
}
# bats test_tags=input:real, year:2015, day:13
@test "AoC 2015 Day 13: Knights of the Dinner Table - real  " {
    run aoc_day
    assert_output 'Part1: 618; Part2: 601'
}

# bats test_tags=input:sample, year:2015, day:15
@test "AoC 2015 Day 15: Science for Hungry People   - sample" {
    run aoc_day _test
    assert_output 'Part1: 62842880; Part2: 57600000'
}
# bats test_tags=input:real, year:2015, day:15
@test "AoC 2015 Day 15: Science for Hungry People   - real  " {
    run aoc_day
    assert_output 'Part1: 222870; Part2: 117936'
}

# bats test_tags=input:real, year:2015, day:16
@test "AoC 2015 Day 16: Aunt Sue                    - real  " {
    run aoc_day
    assert_output 'Part1: 40; Part2: 241'
}
