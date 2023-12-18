library(adventofcode23)
x <- readLines("./inst/input15.txt")

p1 <- f15a_sum_hashes(x)
p2 <- f15b_compute_focusing_power(x)

stopifnot(p1 == aoc_solutions$day15a)
stopifnot(p2 == aoc_solutions$day15b)
