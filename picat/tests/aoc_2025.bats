setup() {
    load './test_helper/bats-support/load'
    load './test_helper/bats-assert/load'
    load './test_helper/aoc-support/load'
}

# bats test_tags=input:sample, year:2025, day:01
@test "AoC 2025 Day 01: Secret Entrance             - sample" { run aoc_day; assert_aoc_day; }
# bats test_tags=input:real, year:2025, day:01
@test "AoC 2025 Day 01: Secret Entrance             - real  " { run aoc_day; assert_aoc_day; }

# bats test_tags=input:sample, year:2025, day:02
@test "AoC 2025 Day 02: Gift Shop                   - sample" { run aoc_day; assert_aoc_day; }
# bats test_tags=input:real, year:2025, day:02
@test "AoC 2025 Day 02: Gift Shop                   - real  " {
    skip "Too slow"
    run aoc_day; assert_aoc_day
}

# bats test_tags=input:sample, year:2025, day:03
@test "AoC 2025 Day 03: Lobby                       - sample" { run aoc_day; assert_aoc_day; }
# bats test_tags=input:real, year:2025, day:03
@test "AoC 2025 Day 03: Lobby                       - real  " { run aoc_day; assert_aoc_day; }

# bats test_tags=input:sample, year:2025, day:04
@test "AoC 2025 Day 04: Printing Department         - sample" { run aoc_day; assert_aoc_day; }
# bats test_tags=input:real, year:2025, day:04
@test "AoC 2025 Day 04: Printing Department         - real  " { run aoc_day; assert_aoc_day; }

# bats test_tags=input:sample, year:2025, day:05
@test "AoC 2025 Day 05: Cafeteria                   - sample" {
    skip "Solved in Haskell"
    run aoc_day; assert_aoc_day;
}
# bats test_tags=input:real, year:2025, day:05
@test "AoC 2025 Day 05: Cafeteria                   - real  " {
    skip "Solved in Haskell"
    run aoc_day; assert_aoc_day
}

# bats test_tags=input:sample, year:2025, day:06
@test "AoC 2025 Day 06: Trash Compactor             - sample" { run aoc_day; assert_aoc_day; }
# bats test_tags=input:real, year:2025, day:06
@test "AoC 2025 Day 06: Trash Compactor             - real  " { run aoc_day; assert_aoc_day; }

# bats test_tags=input:sample, year:2025, day:07
@test "AoC 2025 Day 07: Laboratories                - sample" { run aoc_day; assert_aoc_day; }
# bats test_tags=input:real, year:2025, day:07
@test "AoC 2025 Day 07: Laboratories                - real  " { run aoc_day; assert_aoc_day; }

# bats test_tags=input:sample, year:2025, day:08
@test "AoC 2025 Day 08: Playground                  - sample" { run aoc_day _10; assert_aoc_day _10; }
# bats test_tags=input:real, year:2025, day:08
@test "AoC 2025 Day 08: Playground                  - real  " { run aoc_day; assert_aoc_day; }

# bats test_tags=input:sample, year:2025, day:09
@test "AoC 2025 Day 09: Movie Theater               - sample" { run aoc_day; assert_aoc_day; }
# bats test_tags=input:real, year:2025, day:09
@test "AoC 2025 Day 09: Movie Theater               - real  " { run aoc_day; assert_aoc_day; }

# bats test_tags=input:sample, year:2025, day:10
@test "AoC 2025 Day 10: Factory                     - sample" { run aoc_day; assert_aoc_day; }
# bats test_tags=input:real, year:2025, day:10
@test "AoC 2025 Day 10: Factory                     - real  " {
    #skip "Too slow"
    run aoc_day; assert_aoc_day
}

# bats test_tags=input:sample, year:2025, day:11
@test "AoC 2025 Day 11: Reactor                     - sample" {
    run aoc_day _1; assert_aoc_day _1
    run aoc_day _2; assert_aoc_day _2
}
# bats test_tags=input:real, year:2025, day:11
@test "AoC 2025 Day 11: Reactor                     - real  " { run aoc_day; assert_aoc_day; }

# bats test_tags=input:sample, year:2025, day:12
@test "AoC 2025 Day 12: Christmas Tree Farm         - sample" { run aoc_day; assert_aoc_day; }
# bats test_tags=input:real, year:2025, day:12
@test "AoC 2025 Day 12: Christmas Tree Farm         - real  " { run aoc_day; assert_aoc_day; }
