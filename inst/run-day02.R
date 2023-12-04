library(adventofcode23)
x <- readLines("./inst/input02.txt")

p1 <- f02a_sum_valid_game_ids(x)
p2 <- f02b_sum_game_powers(x)

stopifnot(p1 == aoc_solutions$day02a)
stopifnot(p2 == aoc_solutions$day02b)
