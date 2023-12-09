library(adventofcode23)
x <- readLines("./inst/input09.txt")

p1 <- f09a_oasis_forecast(x)
p2 <- f09b_oasis_backcast(x)

stopifnot(p1 == aoc_solutions$day09a)
stopifnot(p2 == aoc_solutions$day09b)
