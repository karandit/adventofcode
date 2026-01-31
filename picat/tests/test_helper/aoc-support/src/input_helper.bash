aoc_day() {
    local year day input
    for tag in "${BATS_TEST_TAGS[@]}"; do
        case $tag in
            *day:*) day="${tag#*:}";;
            *year:*) year="${tag#*:}";;
            *input:*) input="${tag#*:}";;
        esac
    done
    picat picat/src/${year}/day${day}.pi inputs/${input}/${year}/day${day}${1}.txt
}

assert_aoc_day() {
    local year day input
    for tag in "${BATS_TEST_TAGS[@]}"; do
        case $tag in
            *day:*) day="${tag#*:}";;
            *year:*) year="${tag#*:}";;
            *input:*) input="${tag#*:}";;
        esac
    done

    cat inputs/${input}/${year}/day${day}${1}_expected.txt | assert_output -
}
