library(adventofcode23)
x <- readLines("./inst/input14.txt")

p1 <- f14a_find_total_load(x)
p2 <- f14b_find_load_after_many_cycles(x)

stopifnot(p1 == aoc_solutions$day14a)
stopifnot(p2 == aoc_solutions$day14b)
