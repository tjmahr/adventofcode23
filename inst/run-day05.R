library(adventofcode23)
x <- readLines("./inst/input05.txt")

p1 <- f05a_find_lowest_location(x)
p2 <- f05b_find_lowest_location(x)

stopifnot(p1 == aoc_solutions$day05a)
stopifnot(p2 == aoc_solutions$day05b)
