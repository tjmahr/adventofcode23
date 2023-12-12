library(adventofcode23)
x <- readLines("./inst/input10.txt")

p1 <- f10a_find_farthest_distance(x)
p2 <- f10b(x)

stopifnot(p1 == aoc_solutions$day10a)
stopifnot(p2 == aoc_solutions$day10b)
