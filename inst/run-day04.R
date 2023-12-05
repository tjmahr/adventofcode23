library(adventofcode23)
x <- readLines("./inst/input04.txt")

p1 <- f04a_score_cards(x)
p2 <- f04b_count_cards(x)

stopifnot(p1 == aoc_solutions$day04a)
stopifnot(p2 == aoc_solutions$day04b)
