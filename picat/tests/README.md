# Install

```
brew install picat
brew install bats-core
```

# Usage

```
bats picat/tests/*.bats -T
bats picat/tests/aoc_2016.bats -T
bats picat/tests/aoc_2016.bats -T --filter-tags input:sample
bats picat/tests/aoc_2016.bats -T --filter-tags input:real
bats picat/tests/aoc_2016.bats -T --filter-tags day:11
```
