library(adventofcode23)
x <- readLines("./inst/input11.txt")

p1 <- f11a_sum_galaxy_distances(x)
p2 <- f11a_sum_galaxy_distances(x, 1000000)

stopifnot(p1 == aoc_solutions$day11a)
stopifnot(p2 == aoc_solutions$day11b)
