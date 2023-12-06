library(adventofcode23)
x <- readLines("./inst/input06.txt")

p1 <- f06a_multiply_winning_distances(x)
p2 <- f06b_multiply_winning_distances(x)

stopifnot(p1 == aoc_solutions$day06a)
stopifnot(p2 == aoc_solutions$day06b)
