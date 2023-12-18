library(adventofcode23)
x <- readLines("./inst/input13.txt")

p1 <- f13a_summarize_mirrors(x)
p2 <- f13b_summarize_adjusted_mirrors(x)

stopifnot(p1 == aoc_solutions$day13a)
stopifnot(p2 == aoc_solutions$day13b)
