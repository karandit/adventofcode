setup() {
    load './test_helper/bats-support/load'
    load './test_helper/bats-assert/load'
    load './test_helper/aoc-support/load'
}

# bats test_tags=input:sample, year:2024, day:16
@test "AoC 2024 Day 16 Sample" {
    run aoc_day _1
    assert_output 'Part1: 7036'
    run aoc_day _2
    assert_output 'Part1: 11048'
}
# bats test_tags=input:real, year:2024, day:16
@test "AoC 2024 Day 16" {
    run aoc_day
    assert_output 'Part1: 109496'
}
# bats test_tags=input:sample, year:2024, day:17
@test "AoC 2024 Day 17 Sample" {
    skip "No test file"
    run aoc_day
    assert_output 'Part1: 11048'
}
# bats test_tags=input:real, year:2024, day:17
@test "AoC 2024 Day 17" {
    run aoc_day
    assert_output 'Part1: 5,1,4,0,5,1,0,2,6; Part2: 202322936867370'
}
# bats test_tags=input:sample, year:2024, day:18
@test "AoC 2024 Day 18 Sample" {
    run picat picat/src/2024/day18.pi inputs/sample/2024/day18.txt 6 12
    assert_output 'Part1: 22; Part2: 6,1'
}
# bats test_tags=input:real, year:2024, day:18
@test "AoC 2024 Day 18" {
    run picat picat/src/2024/day18.pi inputs/real/2024/day18.txt 70 1024
    assert_output 'Part1: 340; Part2: 34,32'
}
# bats test_tags=input:sample, year:2024, day:22
@test "AoC 2024 Day 22 Sample" {
    run aoc_day
    assert_output 'Part1: 37327623'
}
# bats test_tags=input:real, year:2024, day:22
@test "AoC 2024 Day 22" {
    run aoc_day
    assert_output 'Part1: 17612566393'
}
# bats test_tags=input:sample, year:2024, day:23
@test "AoC 2024 Day 23 Sample" {
    run aoc_day
    assert_output 'Part1: 7; Part2: co,de,ka,ta'
}
# bats test_tags=input:real, year:2024, day:23
@test "AoC 2024 Day 23" {
    run aoc_day
    assert_output 'Part1: 1046; Part2: de,id,ke,ls,po,sn,tf,tl,tm,uj,un,xw,yz'
}
# bats test_tags=input:sample, year:2024, day:25
@test "AoC 2024 Day 25 Sample" {
    run aoc_day
    assert_output 'Part1: 3'
}
# bats test_tags=input:real, year:2024, day:25
@test "AoC 2024 Day 25" {
    run aoc_day
    assert_output 'Part1: 3338'
}
